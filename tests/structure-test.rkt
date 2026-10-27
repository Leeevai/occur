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

(define (owned id owner sensor ts te n)
  (make-batch-event 'Reading `((owner . ,owner) (sensor . ,sensor)) #:id id #:ts ts #:te te #:n n))
(define gen:owned
  (gen:let ([specs (gen:list (gen:tuple (gen:one-of '("d1" "d2")) (gen:one-of '("s1" "s2")) (gen:integer-in 0 50)
                                        (gen:integer-in 0 20) (gen:integer-in 0 12))
                             #:max-length 12)])
    (for/list ([s specs] [i (in-naturals)])
      (owned (format "e~a" i) (car s) (string-append (car s) "-" (cadr s)) (caddr s) (+ (caddr s) (cadddr s)) (list-ref s 4)))))
(define (owned-out op xs)
  (map (λ (e) (list (event-attr e 'sensor) (event-ts e) (event-te e) (event-n e)))
       (run-result-events (run-operator op (list xs) #:arrivals (list (for/list ([i (length xs)]) i))))))

(spec-test '(ST.monotonic-merge ES.substream) "property: keyed by entity, the merge emits what the per-sub-stream merge emits"
  (check-property
   (make-config #:tests (trials))
   (property ([xs gen:owned])
     (define sensor (λ (e) (event-attr e 'sensor)))
     (check-equal? (owned-out (monotonic-merge-operator sensor #:entity (λ (e) (event-attr e 'owner))) xs)
                   (owned-out (monotonic-merge-operator sensor) xs)))))

(require "../private/context.rkt")

(spec-test '(ST.monotonic-merge CX.entity) "keyed by entity, the merge forgets a removed entity's high-water marks"
  (define (program)
    (define src (make-source 'in))
    (define gate (make-node 'gate (entity-context-operator 'devices #:key (λ (e) (event-attr e 'owner))) (list src)))
    (define m (make-node 'm (monotonic-merge-operator (λ (e) (event-attr e 'sensor)) #:entity (λ (e) (event-attr e 'owner)))
                         (list gate) #:sink? #t))
    (make-program (list m)))
  (define steps (list (directory-push 'devices '(("d1" . active)) #:at 0)
                      (arrive 1 'in (owned "a" "d1" "s1" 90 100 10))
                      (directory-push 'devices '() #:at 2)                       ; d1 removed
                      (directory-push 'devices '(("d1" . active)) #:at 3)        ; and listed again
                      (arrive 4 'in (owned "b" "d1" "s1" 80 90 10))))
  ;; activated afresh, d1's sensor starts from −∞ again, so [80, 90] is new
  (check-equal? (map event-n (run-result-events (run (program) steps))) '(10 10))
  ;; without the removal, [80, 90] is behind the mark and dropped
  (define kept (run (program) (list (car steps) (cadr steps) (last steps))))
  (check-equal? (map event-n (run-result-events kept)) '(10))
  (check-equal? (run-result-counter kept 'm 'late) 10))

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

(define ticks (for/list ([i 10]) (ev (format "e~a" i) i)))

(spec-test 'ST.throttle "at most r events per Δ, aligned buckets"
  (define r (run-operator (throttle-operator '(rate 2 5)) (list ticks)))
  (check-equal? (ids (run-result-events r)) '("e0" "e1" "e5" "e6"))
  (check-equal? (run-result-counter r 'op 'throttled) 6))

(spec-test 'ST.throttle "every n-th event"
  (check-equal? (ids (run-result-events (run-operator (throttle-operator '(every 3)) (list ticks))))
                '("e2" "e5" "e8")))

(spec-test 'ST.throttle "sampling with probability p is reproducible from its seed"
  (define (sample seed) (ids (run-result-events (run-operator (throttle-operator `(probability 1/2 ,seed))
                                                              (list ticks)))))
  (check-equal? (sample 7) (sample 7))
  (check-equal? (ids (run-result-events (run-operator (throttle-operator '(probability 1 7)) (list ticks))))
                (ids ticks))
  (check-equal? (run-result-events (run-operator (throttle-operator '(probability 0 7)) (list ticks))) '()))

(spec-test 'ST.throttle "property: a rate throttle never exceeds r per bucket"
  (check-property
   (make-config #:tests (trials))
   (property ([ts (gen:list (gen:integer-in 0 60) #:max-length 20)] [r (gen:integer-in 1 3)])
     (define es (for/list ([t (sort ts <)] [i (in-naturals)]) (ev (format "e~a" i) t)))
     (define out (run-result-events (run-operator (throttle-operator `(rate ,r 10)) (list es))))
     (for ([b (remove-duplicates (map (λ (e) (quotient (event-te e) 10)) out))])
       (check-true (<= (length (filter (λ (e) (= b (quotient (event-te e) 10))) out)) r))))))
