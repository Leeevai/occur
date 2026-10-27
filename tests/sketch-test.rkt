#lang racket/base
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/event.rkt"
         "../private/sketch.rkt"
         "../private/aggregate.rkt")

(define (ev i x) (make-event 'R `((v . ,x)) #:id (format "e~a" i) #:at i))
(define v (λ (e) (event-attr e 'v)))

(spec-test 'AG.approx "HyperLogLog estimates distinct counts within a few percent"
  (for ([n '(10 1000 20000)])
    (define h (for/fold ([h (make-hll)]) ([i n]) (hll-add (hll-add h i) i)))   ; duplicates do not count
    (define est (hll-estimate h))
    (check-true (< (abs (- est n)) (max 2 (* 0.05 n))) (format "n=~a est=~a" n est)))
  ;; merging two sketches estimates the union
  (define a (for/fold ([h (make-hll)]) ([i 5000]) (hll-add h i)))
  (define b (for/fold ([h (make-hll)]) ([i (in-range 2500 7500)]) (hll-add h i)))
  (check-true (< (abs (- (hll-estimate (hll-merge a b)) 7500)) 375)))

(spec-test 'AG.approx "the t-digest estimates quantiles closely and exactly at the extremes"
  (define t (for/fold ([t (make-tdigest)]) ([i (in-range 10000)]) (tdigest-add t (modulo (* i 7919) 10000))))
  (for ([p '(0.01 0.1 0.5 0.9 0.99)])
    (define q (tdigest-quantile t p))
    (check-true (< (abs (- q (* p 10000))) 100) (format "p=~a q=~a" p q)))
  (check-equal? (tdigest-quantile t 0) 0)
  (check-equal? (tdigest-quantile t 1) 9999)
  (define halves (tdigest-merge (for/fold ([t (make-tdigest)]) ([i 5000]) (tdigest-add t i))
                                (for/fold ([t (make-tdigest)]) ([i (in-range 5000 10000)]) (tdigest-add t i))))
  (check-true (< (abs (- (tdigest-quantile halves 0.5) 5000)) 100)))

(spec-test 'AG.approx "Count-Min never underestimates and is exact without collisions"
  (define cm (for/fold ([cm (make-count-min)]) ([x '(a b a c a b)]) (count-min-add cm x)))
  (check-equal? (count-min-estimate cm 'a) 3)
  (check-equal? (count-min-estimate cm 'b) 2)
  (check-equal? (count-min-estimate cm 'zzz) 0))

(spec-test 'AG.approx "property: Count-Min estimates are at least the true count"
  (check-property
   (make-config #:tests (trials))
   (property ([xs (gen:list (gen:integer-in 0 300) #:max-length 60)])
     (define cm (for/fold ([cm (make-count-min 16 3)]) ([x xs]) (count-min-add cm x)))
     (for ([x (remove-duplicates xs)])
       (check-true (>= (count-min-estimate cm x) (count (λ (y) (= x y)) xs)))))))

(spec-test 'AG.approx "approximate aggregates run over events and merge"
  (define es (for/list ([i 2000]) (ev i (modulo i 500))))
  (check-true (< (abs (- (aggregate-events (approx-distinct-agg v) es) 500)) 25))
  (check-true (< (abs (- (aggregate-events (approx-percentile-agg v 0.5) es) 250)) 10))
  ;; 500 distinct values in 272 columns collide; a wider sketch has room
  (check-true (>= (aggregate-events (approx-frequency-agg v 7) es) 4))
  (check-true (<= 4 (aggregate-events (approx-frequency-agg v 7 #:width 8192) es) 6))
  (check-equal? (aggregator-class (approx-distinct-agg v)) 'approximate)
  (check-equal? (hash64 "same") (hash64 "same")))
