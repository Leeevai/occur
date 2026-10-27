#lang racket/base
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/time.rkt"
         "../private/event.rkt"
         "../private/operator.rkt"
         "../private/dataflow.rkt"
         "../private/ops/structure.rkt")

(define (ev id t [payload '()] #:te [te t] #:n [n 1]) (make-event 'X payload #:id id #:ts t #:te te #:n n))
(define (ids es) (map event-id es))

(spec-test 'ST.merge "merge interleaves its inputs by arrival"
  (define a (make-source 'a))
  (define b (make-source 'b))
  (define m (make-node 'm (merge-operator 2) (list a b) #:sink? #t))
  (define r (run (make-program (list m))
                 (list (arrive 1 'a (ev "a1" 1)) (arrive 2 'b (ev "b1" 0)) (arrive 3 'a (ev "a2" 3)))))
  (check-equal? (ids (run-result-events r)) '("a1" "b1" "a2"))
  (check-equal? (map output-at (run-result-outputs r)) '(1 2 3)))

(spec-test '(ST.key-by ES.keyed) "key-by partitions downstream state; re-keying is the same operation"
  (define src (make-source 'in))
  (define by-sender (make-node 'k (key-by-operator (attribute-key-fn 'sender)) (list src)))
  (define count (make-operator 'count #:key (attribute-key-fn 'owner) #:init-key (λ (g k c) 0)
                               #:on-event (λ (g s e c)
                                            (emit! c (make-event 'N `((owner . ,(ctx-key c)) (n . ,(add1 s)))
                                                                 #:at (event-te e)))
                                            (add1 s))))
  (define n (make-node 'n count (list by-sender) #:sink? #t))
  (define r (run (make-program (list n))
                 (for/list ([s '(d1 d2 d1)] [o '(d7 d7 d8)] [i (in-naturals)])
                   (arrive i 'in (ev (format "e~a" i) i `((sender . ,s) (owner . ,o)))))))
  ;; keyed by the owning device, not the device that sent the reading
  (check-equal? (map (λ (e) (list (event-attr e 'owner) (event-attr e 'n))) (run-result-events r))
                '((d7 1) (d7 2) (d8 1)))
  (check-equal? (sort (hash-keys (cdr (hash-ref (run-result-states r) 'n))) symbol<?) '(d7 d8)))

(spec-test 'ST.dedupe "an event is dropped when an earlier one with its key lies within the horizon"
  (define op (dedupe-operator (λ (e) (event-attr e 'k)) #:horizon 10))
  (define in (list (ev "a" 0 '((k . 1))) (ev "b" 5 '((k . 1))) (ev "c" 5 '((k . 2)))
                   (ev "d" 30 '((k . 1))) (ev "e" 38 '((k . 1)))))
  (define r (run-operator op (list in)))
  (check-equal? (ids (run-result-events r)) '("a" "c" "d"))
  (check-equal? (run-result-counter r 'op 'duplicates) 2))

(spec-test 'ST.dedupe "at-least-once redelivery is removed on the processing clock too"
  (define op (dedupe-operator (λ (e) (event-id e)) #:horizon 60 #:clock 'processing-time))
  (define a (ev "a" 0))
  (define r (run-operator op (list (list a a a)) #:arrivals '((0 10 100))))
  (check-equal? (ids (run-result-events r)) '("a" "a"))
  ;; the remembered times are forgotten a horizon after the last event
  (check-equal? (hash-count (cdr (hash-ref (run-result-states r) 'op))) 1))

(define (sensor id ts te n) (make-batch-event 'Reading '((sensor . "s1")) #:id id #:ts ts #:te te #:n n))
(define (merge-run arrivals)
  (run-operator (monotonic-merge-operator (λ (e) (event-attr e 'sensor))) (list arrivals)
                #:arrivals (list (for/list ([i (length arrivals)]) i))))

(spec-test '(ST.monotonic-merge ES.batch) "trace R2: contributions, high-water marks, and late counts"
  (define arrivals (list (sensor "a" 90 100 10) (sensor "b" 95 105 10)
                         (sensor "c" 95 105 10) (sensor "d" 80 90 10)))
  ;; replay the trace one arrival at a time and check the table row by row
  (define rows
    (for/list ([k (in-range 1 5)])
      (define r (merge-run (take arrivals k)))
      (define st (hash-ref (cdr (hash-ref (run-result-states r) 'op)) "s1"))
      (list (hwm-state-hw st) (hwm-state-late st))))
  (check-equal? rows '((100 0) (105 5) (105 15) (105 25)))
  (define out (run-result-events (merge-run arrivals)))
  (check-equal? (map (λ (e) (list (event-ts e) (event-te e) (event-n e))) out) '((90 100 10) (100 105 5)))
  (check-equal? (map (λ (e) (map event-id (event-constituents e))) out) '(("a") ("b"))))

(define gen:batches
  (gen:let ([specs (gen:list (gen:tuple (gen:integer-in 0 50) (gen:integer-in 0 20) (gen:integer-in 0 12))
                             #:max-length 10)])
    (for/list ([s specs] [i (in-naturals)])
      (sensor (format "e~a" i) (car s) (+ (car s) (cadr s)) (caddr s)))))

(define (merged arrivals)
  (define r (merge-run arrivals))
  (values (map (λ (e) (list (event-ts e) (event-te e) (event-n e))) (run-result-events r))
          (let ([st (hash-ref (cdr (hash-ref (run-result-states r) 'op)) "s1" #f)])
            (and st (list (hwm-state-hw st))))))

(spec-test 'ST.monotonic-merge "property: re-applying already-applied arrivals is a no-op"
  (check-property
   (make-config #:tests (trials))
   (property ([xs gen:batches] [k (gen:integer-in 0 10)])
     ;; replay any prefix of what was already applied, after the fact
     (define replay (take xs (min k (length xs))))
     (define-values (out1 hw1) (merged xs))
     (define-values (out2 hw2) (merged (append xs replay)))
     (check-equal? out2 out1)
     (check-equal? hw2 hw1))))

(spec-test 'ST.monotonic-merge "property: contributions never overlap and never exceed their arrival"
  (check-property
   (make-config #:tests (trials))
   (property ([xs gen:batches])
     (define-values (out _hw) (merged xs))
     (for ([a out] [b (if (null? out) '() (cdr out))])
       (check-true (<= (cadr a) (car b))))
     (check-true (<= (apply + 0 (map caddr out)) (apply + 0 (map event-n xs)))))))

(spec-test 'ST.reorder "reorder releases buffered events in event-time order at the watermark"
  (define src (make-source 'in))
  (define n (make-node 'r (reorder-operator) (list src) #:sink? #t))
  (define r (run (make-program (list n))
                 (list (arrive 1 'in (ev "c" 30)) (arrive 2 'in (ev "a" 10)) (arrive 3 'in (ev "b" 20))
                       (watermark-to 'in 25)
                       (arrive 4 'in (ev "late" 5)) (arrive 5 'in (ev "d" 26)))))
  (check-equal? (ids (run-result-events r)) '("a" "b" "d" "c"))
  (check-equal? (map output-at (run-result-outputs r)) '(3 3 5 5))
  (check-equal? (run-result-counter r 'r 'late) 1))

(spec-test 'ST.reorder "property: the released order is sorted by end time and nothing on time is lost"
  (check-property
   (make-config #:tests (trials))
   (property ([ts (gen:list (gen:integer-in 0 40) #:max-length 15)])
     (define es (for/list ([t ts] [i (in-naturals)]) (ev (format "e~a" i) t)))
     (define out (run-result-events (run-operator (reorder-operator) (list es) #:watermark 'none
                                                  #:arrivals (list (for/list ([i (length es)]) i)))))
     (check-equal? (map event-te out) (sort (map event-te es) <))
     (check-equal? (length out) (length es)))))
