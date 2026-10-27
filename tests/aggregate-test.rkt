#lang racket/base
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/event.rkt"
         "../private/aggregate.rkt")

(define (ev i v #:n [n 1] #:ts [ts i] #:te [te ts] #:sensor [s "s1"] #:kind [k 'load])
  (make-event 'R `((v . ,v) (sensor . ,s) (kind . ,k)) #:id (format "e~a" i) #:ts ts #:te te #:n n))
(define v (λ (e) (event-attr e 'v)))
(define es (list (ev 0 4 #:n 2) (ev 1 1) (ev 2 7 #:n 3) (ev 3 7)))

(spec-test '(AG.count AG.multiplicity AG.events) "count is the multiplicity sum; events counts events"
  (check-equal? (aggregate-events (count-agg) es) 7)
  (check-equal? (aggregate-events (multiplicity-agg) es) 7)
  (check-equal? (aggregate-events (events-agg) es) 4)
  (check-equal? (aggregator-class (count-agg)) 'distributive))

(spec-test '(AG.sum AG.min AG.max AG.avg) "sum, min, max, and average"
  (check-equal? (aggregate-events (sum-agg v) es) 19)
  (check-equal? (aggregate-events (min-agg v) es) 1)
  (check-equal? (aggregate-events (max-agg v) es) 7)
  (check-equal? (aggregate-events (avg-agg v) es) 19/4)
  (check-false (aggregate-events (avg-agg v) '()))
  (check-equal? (aggregator-class (avg-agg v)) 'algebraic))

(spec-test 'AG.variance "variance and standard deviation from (n, Σx, Σx²)"
  (define xs (for/list ([x '(2 4 4 4 5 5 7 9)] [i (in-naturals)]) (ev i x)))
  (check-equal? (aggregate-events (variance-agg v) xs) 4)
  (check-equal? (aggregate-events (stddev-agg v) xs) 2)
  (check-equal? (aggregate-events (variance-agg v #:sample? #t) xs) 32/7)
  (check-false (aggregate-events (variance-agg v) '())))

(spec-test '(AG.first AG.last) "first and last by event time, not arrival"
  (define out-of-order (list (ev 0 'b #:ts 5) (ev 1 'a #:ts 1) (ev 2 'c #:ts 9) (ev 3 'z #:ts 3)))
  (check-equal? (aggregate-events (first-agg v) out-of-order) 'a)
  (check-equal? (aggregate-events (last-agg v) out-of-order) 'c))

(spec-test 'AG.span "span is max te − min ts"
  (check-equal? (aggregate-events (span-agg) (list (ev 0 1 #:ts 10 #:te 12) (ev 1 1 #:ts 4 #:te 6) (ev 2 1 #:ts 20 #:te 25)))
                21)
  (check-equal? (aggregate-events (span-agg) '()) 0))

(spec-test 'AG.sources "sources is the set of contributing sub-streams and kinds"
  (define xs (list (ev 0 1 #:sensor "s1" #:kind 'load) (ev 1 1 #:sensor "s2" #:kind 'position)
                   (ev 2 1 #:sensor "s1" #:kind 'load)))
  (check-equal? (aggregate-events (sources-agg (λ (e) (event-attr e 'sensor)) (λ (e) (event-attr e 'kind))) xs)
                '(("s1" . load) ("s2" . position))))

(spec-test '(AG.argmax AG.argmin) "argmax and argmin return the event with the extreme value"
  (check-equal? (event-id (aggregate-events (argmax-agg v) es)) "e2")   ; first of the tied 7s
  (check-equal? (event-id (aggregate-events (argmin-agg v) es)) "e1"))

(define gen:events
  (gen:let ([xs (gen:list (gen:tuple (gen:integer-in 0 50) (gen:integer-in 0 5)) #:max-length 12)])
    (for/list ([x xs] [i (in-naturals)]) (ev i (car x) #:n (cadr x) #:ts (car x) #:te (+ (car x) (cadr x))))))

(define distributive
  (list (count-agg) (events-agg) (sum-agg v) (min-agg v) (max-agg v) (avg-agg v) (variance-agg v)
        (first-agg v) (last-agg v) (span-agg) (argmax-agg v) (argmin-agg v)
        (sources-agg (λ (e) (event-attr e 'sensor)) (λ (e) (event-attr e 'kind)))))

(define (fold agg xs) (for/fold ([s ((aggregator-init agg))]) ([x xs]) ((aggregator-add agg) s x)))

(spec-test '(AG.count AG.sum AG.min AG.max AG.avg AG.variance AG.first AG.last AG.span AG.argmax AG.argmin AG.sources)
           "property: F(X ⊎ Y) = combine(F(X), F(Y)) for the distributive and algebraic aggregates"
  (check-property
   (make-config #:tests (trials))
   (property ([xs gen:events] [ys gen:events])
     (for ([agg distributive])
       (define whole ((aggregator-result agg) (fold agg (append xs ys))))
       (define parts ((aggregator-result agg) ((aggregator-combine agg) (fold agg xs) (fold agg ys))))
       (if (event? whole)
           (check-equal? (v parts) (v whole))
           (check-equal? parts whole (format "~a" (aggregator-name agg))))))))

(spec-test 'AG.incremental "property: remove undoes add for the invertible aggregates"
  (check-property
   (make-config #:tests (trials))
   (property ([xs gen:events] [ys gen:events])
     (for ([agg (list (count-agg) (events-agg) (sum-agg v) (avg-agg v) (variance-agg v))])
       (define with (fold agg (append xs ys)))
       (define without (for/fold ([s with]) ([y ys]) ((aggregator-remove agg) s y)))
       (check-equal? ((aggregator-result agg) without) ((aggregator-result agg) (fold agg xs)))))))

(define (vals xs) (for/list ([x xs] [i (in-naturals)]) (ev i x)))

(spec-test 'AG.collect "collect keeps every value in arrival order"
  (check-equal? (aggregate-events (collect-agg v) (vals '(3 1 3 2))) '(3 1 3 2))
  (check-equal? (aggregator-class (collect-agg v)) 'holistic))

(spec-test '(AG.median AG.percentile) "median and nearest-rank percentiles"
  (check-equal? (aggregate-events (median-agg v) (vals '(5 1 3))) 3)
  (check-equal? (aggregate-events (median-agg v) (vals '(4 1 3 2))) 5/2)
  (define xs (vals '(15 20 35 40 50)))
  (check-equal? (aggregate-events (percentile-agg v 0.3) xs) 20)
  (check-equal? (aggregate-events (percentile-agg v 0.4) xs) 20)
  (check-equal? (aggregate-events (percentile-agg v 0.5) xs) 35)
  (check-equal? (aggregate-events (percentile-agg v 1) xs) 50)
  (check-equal? (aggregate-events (percentile-agg v 0) xs) 15)
  (check-false (aggregate-events (percentile-agg v 0.5) '())))

(spec-test '(AG.count-distinct AG.mode AG.top-k) "distinct count, mode, and top-k"
  (define xs (vals '(3 1 3 2 1 3 9)))
  (check-equal? (aggregate-events (count-distinct-agg v) xs) 4)
  (check-equal? (aggregate-events (mode-agg v) xs) 3)
  (check-equal? (aggregate-events (mode-agg v) (vals '(2 1 1 2))) 2)     ; tie: seen first
  (check-equal? (aggregate-events (top-k-agg v 3) xs) '(9 3 3))
  (check-equal? (aggregate-events (top-k-agg v 2 #:by 'frequency) xs) '(3 1)))

(spec-test '(AG.median AG.percentile AG.count-distinct AG.mode AG.top-k AG.collect)
           "property: holistic aggregates combine and remove exactly"
  (check-property
   (make-config #:tests (trials))
   (property ([xs (gen:list (gen:integer-in 0 9) #:max-length 12)] [ys (gen:list (gen:integer-in 0 9) #:max-length 12)])
     (define ex (vals xs))
     (define ey (for/list ([y ys] [i (in-naturals 100)]) (ev i y)))
     (for ([agg (list (median-agg v) (percentile-agg v 0.9) (count-distinct-agg v) (top-k-agg v 3))])
       (check-equal? ((aggregator-result agg) ((aggregator-combine agg) (fold agg ex) (fold agg ey)))
                     ((aggregator-result agg) (fold agg (append ex ey))))
       (check-equal? ((aggregator-result agg) (for/fold ([s (fold agg (append ex ey))]) ([y ey]) ((aggregator-remove agg) s y)))
                     ((aggregator-result agg) (fold agg ex))))
     ;; the median is the 50th percentile for odd counts
     (when (odd? (length xs))
       (check-equal? (aggregate-events (median-agg v) ex) (aggregate-events (percentile-agg v 1/2) ex))))))
