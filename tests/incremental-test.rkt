#lang racket/base
;; AG.incremental: windows maintained incrementally equal refolded windows.
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/event.rkt"
         "../private/dataflow.rkt"
         "../private/window.rkt"
         "../private/aggregate.rkt")

(define (ev i x) (make-event 'R `((v . ,x)) #:id (format "e~a" i) #:at i))
(define v (λ (e) (event-attr e 'v)))

(define (count-windows acc xs n m)
  (define es (for/list ([x xs] [i (in-naturals)]) (ev i x)))
  (map event-payload (run-result-events (run-operator (count-window-operator n m acc) (list es)))))

;; The same aggregates, stripped of remove and combine: always refolded.
(define (refolded acc)
  (make-accumulator #:init (accumulator-init acc) #:add (accumulator-add acc) #:result (accumulator-result acc)))
(define (without-remove acc)
  (make-accumulator #:init (accumulator-init acc) #:add (accumulator-add acc) #:result (accumulator-result acc)
                    #:combine (accumulator-combine acc)))

(spec-test 'AG.incremental "named aggregates form a window accumulator"
  (define acc (aggregate-accumulator (list (cons 'total (sum-agg v)) (cons 'n (count-agg)))))
  (check-true (procedure? (accumulator-remove acc)))
  (check-equal? (count-windows acc '(1 2 3 4) 2 2) (list (hasheq 'total 3 'n 2) (hasheq 'total 7 'n 2)))
  ;; max is associative but not invertible
  (define mx (aggregate-accumulator (list (cons 'hi (max-agg v)))))
  (check-false (accumulator-remove mx))
  (check-true (procedure? (accumulator-combine mx))))

(spec-test '(AG.incremental AG.max AG.min AG.sum) "property: invertible, two-stack, and refolded windows agree"
  (check-property
   (make-config #:tests (trials))
   (property ([xs (gen:list (gen:integer-in -20 20) #:max-length 25)] [n (gen:integer-in 1 6)] [m (gen:integer-in 1 4)])
     (define acc (aggregate-accumulator (list (cons 'total (sum-agg v)) (cons 'lo (min-agg v))
                                              (cons 'hi (max-agg v)) (cons 'k (count-agg)))))
     (define expected (count-windows (refolded acc) xs n m))
     (check-equal? (count-windows acc xs n m) expected)                    ; invertible? no: min/max
     (check-equal? (count-windows (without-remove acc) xs n m) expected)   ; two-stack queue
     (define inv (aggregate-accumulator (list (cons 'total (sum-agg v)) (cons 'k (count-agg)))))
     (check-equal? (count-windows inv xs n m) (count-windows (refolded inv) xs n m)))))

(spec-test '(AG.incremental AG.max) "property: the monotonic deque gives the sliding maximum"
  (check-property
   (make-config #:tests (trials))
   (property ([xs (gen:list (gen:integer-in 0 50) #:max-length 30)] [n (gen:integer-in 1 8)])
     (for/fold ([d (make-mono-deque >)]) ([x xs] [i (in-naturals)])
       (define d2 (mono-deque-evict (mono-deque-push d i x) (- i n -1)))
       (check-equal? (mono-deque-best d2) (apply max (take-right (take xs (add1 i)) (min n (add1 i)))))
       d2)
     (void))))
