#lang racket/base
(require rackunit
         racket/list
         "util.rkt"
         "../private/event.rkt"
         "../private/dataflow.rkt"
         "../private/ops/output.rkt")

(define (ev id t [payload '()] #:te [te t] #:n [n 1]) (make-event 'Out payload #:id id #:ts t #:te te #:n n))
(define (run-op op steps)
  (define src (make-source 'in))
  (define n (make-node 'op op (list src) #:sink? #t))
  (run (make-program (list n)) steps))
(define (shape r) (for/list ([o (run-result-outputs r)])
                    (define e (output-item o))
                    (list (output-at o) (event-ts e) (event-te e) (event-n e))))

(spec-test 'OUT.coalesce "outputs due together become one: hull interval, summed multiplicity, a count"
  (define r (run-op (coalesce-operator #:sum '(samples))
                    (list (arrive 5 'in (ev "w1" 0 '((samples . 3)) #:te 60))
                          (arrive 5 'in (ev "w2" 60 '((samples . 2)) #:te 120))
                          (arrive 9 'in (ev "w3" 120 '((samples . 1)) #:te 180)))))
  (check-equal? (shape r) '((5 0 120 2) (9 120 180 1)))
  (define merged (car (run-result-events r)))
  (check-equal? (list (event-attr merged 'merged) (event-attr merged 'samples)) '(2 5))
  (check-equal? (map event-id (event-constituents merged)) '("w1" "w2")))

(spec-test 'OUT.rate-limit "drop discards outputs inside the cap"
  (define r (run-op (rate-limit-operator 60 #:mode 'drop)
                    (list (arrive 20 'in (ev "a" 0)) (arrive 40 'in (ev "b" 1)) (arrive 90 'in (ev "c" 2)))))
  (check-equal? (map event-id (run-result-events r)) '("a" "c"))
  (check-equal? (run-result-counter r 'op 'rate-limited) 1))

(spec-test 'OUT.rate-limit "defer holds outputs until the cap ends and merges what arrived meanwhile (trace R1's cap)"
  (define r (run-op (rate-limit-operator 60 #:mode 'defer)
                    (list (arrive 20 'in (ev "a" 0 #:te 20 #:n 3)) (arrive 40 'in (ev "b" 25 #:te 40 #:n 3))
                          (arrive 50 'in (ev "c" 50 #:n 1)) (advance-to 100))))
  (check-equal? (shape r) '((20 0 20 3) (80 25 50 4))))

(spec-test 'OUT.edge "only transitions are reported, never while the condition persists"
  (define (st id t s) (ev id t `((state . ,s))))
  (define r (run-op (edge-operator #:enter? (λ (e) (eq? (event-attr e 'state) 'silent))
                                   #:leave? (λ (e) (eq? (event-attr e 'state) 'live)))
                    (list (arrive 1 'in (st "s1" 1 'silent)) (arrive 2 'in (st "s2" 2 'silent))
                          (arrive 3 'in (st "l1" 3 'live)) (arrive 4 'in (st "l2" 4 'live))
                          (arrive 5 'in (st "s3" 5 'silent)))))
  (check-equal? (map event-id (run-result-events r)) '("s1" "l1" "s3"))
  (check-equal? (run-result-counter r 'op 'persisting) 2))

(require rackcheck)

(spec-test 'OUT.rate-limit "property: a deferring rate limit never loses a contribution and keeps its cap"
  (check-property
   (make-config #:tests (trials))
   (property ([gaps (gen:list (gen:integer-in 0 40) #:max-length 15)] [ns (gen:list (gen:integer-in 0 5) #:max-length 15)]
              [cap (gen:integer-in 1 60)])
     (define times (let loop ([gs gaps] [t 0] [acc '()]) (if (null? gs) (reverse acc) (loop (cdr gs) (+ t (car gs)) (cons (+ t (car gs)) acc)))))
     (define es (for/list ([t times] [n (in-cycle (if (null? ns) '(1) ns))] [i (in-naturals)]) (ev (format "e~a" i) t #:n n)))
     (define r (run-op (rate-limit-operator cap #:mode 'defer)
                       (append (for/list ([e es]) (arrive (event-ts e) 'in e)) (list (advance-to 100000)))))
     (check-equal? (apply + (map event-n (run-result-events r))) (apply + (map event-n es)))
     (define ats (map output-at (run-result-outputs r)))
     (for ([a ats] [b (if (null? ats) '() (cdr ats))]) (check-true (>= (- b a) cap))))))
