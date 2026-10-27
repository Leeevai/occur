#lang racket/base
;; Aggregation (spec §8).
;;
;; An aggregator keeps a partial state with init, add, and combine, and
;; reads its result from the state. Invertible aggregators also support
;; remove (AG.incremental). Aggregates fall into the classes of Gray et al.:
;; distributive (F(X ⊎ Y) = G(F(X), F(Y))), algebraic (computable from a
;; fixed-size tuple of distributive aggregates), holistic (no constant-size
;; partial state), and approximate (constant-size sketches).
(require racket/contract
         racket/list
         racket/math
         "time.rkt"
         "event.rkt"
         "window.rkt")

(struct aggregator (name class init add remove combine result) #:transparent)

(define (make-aggregator name class #:init init #:add add #:result [result values]
                         #:remove [remove #f] #:combine [combine #f])
  (aggregator name class init add remove combine result))

;; Apply an aggregator to a list of events (the batch definition).
(define (aggregate-events agg events)
  ((aggregator-result agg)
   (for/fold ([s ((aggregator-init agg))]) ([e (in-list events)]) ((aggregator-add agg) s e))))

;; ===========================================================================
;; Distributive and algebraic aggregates

;; AG.count: over batch events, the multiplicity sum Σ n(e) (AG.multiplicity).
(define (count-agg)
  (make-aggregator 'count 'distributive #:init (λ () 0) #:add (λ (s e) (+ s (event-n e)))
                   #:remove (λ (s e) (- s (event-n e))) #:combine +))
(define (multiplicity-agg) (count-agg))

;; AG.events: the number of events.
(define (events-agg)
  (make-aggregator 'events 'distributive #:init (λ () 0) #:add (λ (s e) (add1 s))
                   #:remove (λ (s e) (sub1 s)) #:combine +))

;; AG.sum
(define (sum-agg f)
  (make-aggregator 'sum 'distributive #:init (λ () 0) #:add (λ (s e) (+ s (f e)))
                   #:remove (λ (s e) (- s (f e))) #:combine +))

;; AG.min / AG.max: distributive but not invertible.
(define (extreme-agg name better?)
  (λ (f)
    (make-aggregator name 'distributive
                     #:init (λ () #f)
                     #:add (λ (s e) (define v (f e)) (if (or (not s) (better? v s)) v s))
                     #:combine (λ (a b) (cond [(not a) b] [(not b) a] [(better? b a) b] [else a])))))
(define min-agg (extreme-agg 'min <))
(define max-agg (extreme-agg 'max >))

;; AG.avg = sum / count (algebraic, from (Σx, k)).
(define (avg-agg f)
  (make-aggregator 'avg 'algebraic
                   #:init (λ () (cons 0 0))
                   #:add (λ (s e) (cons (+ (car s) (f e)) (add1 (cdr s))))
                   #:remove (λ (s e) (cons (- (car s) (f e)) (sub1 (cdr s))))
                   #:combine (λ (a b) (cons (+ (car a) (car b)) (+ (cdr a) (cdr b))))
                   #:result (λ (s) (and (positive? (cdr s)) (/ (car s) (cdr s))))))

;; AG.variance from (n, Σx, Σx²): population variance by default, the sample
;; variance with #:sample? #t; stddev is its square root.
(define (moments-agg name f finish)
  (make-aggregator name 'algebraic
                   #:init (λ () (vector 0 0 0))
                   #:add (λ (s e) (define x (f e))
                           (vector (add1 (vector-ref s 0)) (+ (vector-ref s 1) x) (+ (vector-ref s 2) (* x x))))
                   #:remove (λ (s e) (define x (f e))
                              (vector (sub1 (vector-ref s 0)) (- (vector-ref s 1) x) (- (vector-ref s 2) (* x x))))
                   #:combine (λ (a b) (vector (+ (vector-ref a 0) (vector-ref b 0))
                                              (+ (vector-ref a 1) (vector-ref b 1))
                                              (+ (vector-ref a 2) (vector-ref b 2))))
                   #:result finish))

(define (variance-of s sample?)
  (define n (vector-ref s 0))
  (define d (if sample? (sub1 n) n))
  (and (positive? d)
       (let ([m (/ (vector-ref s 1) n)])
         (max 0 (/ (- (vector-ref s 2) (* n m m)) d)))))

(define (variance-agg f #:sample? [sample? #f])
  (moments-agg 'variance f (λ (s) (variance-of s sample?))))
(define (stddev-agg f #:sample? [sample? #f])
  (moments-agg 'stddev f (λ (s) (let ([v (variance-of s sample?)]) (and v (sqrt v))))))

;; AG.first / AG.last by event time (ts; ties keep the earlier arrival for
;; first and the later arrival for last).
(define (first-agg f)
  (make-aggregator 'first 'distributive
                   #:init (λ () #f)
                   #:add (λ (s e) (if (or (not s) (< (event-ts e) (car s))) (cons (event-ts e) (f e)) s))
                   #:combine (λ (a b) (cond [(not a) b] [(not b) a] [(<= (car a) (car b)) a] [else b]))
                   #:result (λ (s) (and s (cdr s)))))
(define (last-agg f)
  (make-aggregator 'last 'distributive
                   #:init (λ () #f)
                   #:add (λ (s e) (if (or (not s) (>= (event-ts e) (car s))) (cons (event-ts e) (f e)) s))
                   #:combine (λ (a b) (cond [(not a) b] [(not b) a] [(>= (car b) (car a)) b] [else a]))
                   #:result (λ (s) (and s (cdr s)))))

;; AG.span: max te − min ts over the events (algebraic, from min and max).
(define (span-agg)
  (make-aggregator 'span 'algebraic
                   #:init (λ () #f)
                   #:add (λ (s e) (if s (cons (time-min (car s) (event-ts e)) (time-max (cdr s) (event-te e)))
                                      (cons (event-ts e) (event-te e))))
                   #:combine (λ (a b) (cond [(not a) b] [(not b) a]
                                            [else (cons (time-min (car a) (car b)) (time-max (cdr a) (cdr b)))]))
                   #:result (λ (s) (if s (- (cdr s) (car s)) 0))))

;; AG.sources: the set of contributing (sub-stream . kind) pairs (set union).
(define (sources-agg substream kind)
  (make-aggregator 'sources 'distributive
                   #:init (λ () '())
                   #:add (λ (s e) (define p (cons (substream e) (kind e))) (if (member p s) s (cons p s)))
                   #:combine (λ (a b) (remove-duplicates (append a b)))
                   #:result (λ (s) (sort s (λ (x y) (string<? (format "~s" x) (format "~s" y)))))))

;; AG.argmax / AG.argmin: the event with the max / min value (the earliest
;; arrival among ties).
(define (arg-agg name better?)
  (λ (f)
    (make-aggregator name 'distributive
                     #:init (λ () #f)
                     #:add (λ (s e) (define v (f e)) (if (or (not s) (better? v (car s))) (cons v e) s))
                     #:combine (λ (a b) (cond [(not a) b] [(not b) a] [(better? (car b) (car a)) b] [else a]))
                     #:result (λ (s) (and s (cdr s))))))
(define argmax-agg (arg-agg 'argmax >))
(define argmin-agg (arg-agg 'argmin <))

(provide
 (contract-out
  [struct aggregator ([name symbol?] [class (or/c 'distributive 'algebraic 'holistic 'approximate)]
                      [init (-> any/c)] [add (-> any/c event? any/c)] [remove (or/c #f procedure?)]
                      [combine (or/c #f procedure?)] [result (-> any/c any/c)])]
  [make-aggregator (->* (symbol? (or/c 'distributive 'algebraic 'holistic 'approximate)
                                 #:init (-> any/c) #:add (-> any/c event? any/c))
                        (#:result (-> any/c any/c) #:remove (or/c #f procedure?) #:combine (or/c #f procedure?))
                        aggregator?)]
  [aggregate-events (-> aggregator? (listof event?) any/c)]
  [count-agg (-> aggregator?)]
  [multiplicity-agg (-> aggregator?)]
  [events-agg (-> aggregator?)]
  [sum-agg (-> (-> event? real?) aggregator?)]
  [min-agg (-> (-> event? real?) aggregator?)]
  [max-agg (-> (-> event? real?) aggregator?)]
  [avg-agg (-> (-> event? real?) aggregator?)]
  [variance-agg (->* ((-> event? real?)) (#:sample? boolean?) aggregator?)]
  [stddev-agg (->* ((-> event? real?)) (#:sample? boolean?) aggregator?)]
  [first-agg (-> (-> event? any/c) aggregator?)]
  [last-agg (-> (-> event? any/c) aggregator?)]
  [span-agg (-> aggregator?)]
  [sources-agg (-> (-> event? any/c) (-> event? any/c) aggregator?)]
  [argmax-agg (-> (-> event? real?) aggregator?)]
  [argmin-agg (-> (-> event? real?) aggregator?)]))

;; ===========================================================================
;; Holistic aggregates keep all values: a multiset (value -> (count . order))
;; where order is the arrival index of the value's first occurrence, used to
;; break ties deterministically. Multisets support remove and combine.

(struct mset (counts next) #:transparent)   ; counts: hash value -> (cons count first-order)
(define empty-mset (mset (hash) 0))

(define (mset-add m x)
  (define old (hash-ref (mset-counts m) x #f))
  (mset (hash-set (mset-counts m) x (if old (cons (add1 (car old)) (cdr old)) (cons 1 (mset-next m))))
        (add1 (mset-next m))))
(define (mset-remove m x)
  (define old (hash-ref (mset-counts m) x #f))
  (cond [(not old) m]
        [(= (car old) 1) (mset (hash-remove (mset-counts m) x) (mset-next m))]
        [else (mset (hash-set (mset-counts m) x (cons (sub1 (car old)) (cdr old))) (mset-next m))]))
(define (mset-union a b)
  (for/fold ([m a]) ([(x c) (in-hash (mset-counts b))])
    (for/fold ([m m]) ([i (in-range (car c))]) (mset-add m x))))
(define (mset-sorted m)   ; all values, ascending, with duplicates
  (sort (for*/list ([(x c) (in-hash (mset-counts m))] [i (in-range (car c))]) x) <))
(define (mset-by-arrival m)
  (sort (hash->list (mset-counts m)) < #:key cddr))

(define (holistic name f result)
  (make-aggregator name 'holistic
                   #:init (λ () empty-mset)
                   #:add (λ (s e) (mset-add s (f e)))
                   #:remove (λ (s e) (mset-remove s (f e)))
                   #:combine mset-union
                   #:result result))

;; AG.collect: the list of values, in arrival order of first occurrence with
;; repetitions kept.
(define (collect-agg f)
  (make-aggregator 'collect 'holistic
                   #:init (λ () '())
                   #:add (λ (s e) (cons (f e) s))
                   #:remove (λ (s e) (let loop ([l (reverse s)] [acc '()])
                                       (cond [(null? l) s]
                                             [(equal? (car l) (f e)) (append (reverse (cdr l)) acc)]
                                             [else (loop (cdr l) (cons (car l) acc))])))
                   #:combine (λ (a b) (append b a))
                   #:result reverse))

;; AG.percentile by the nearest-rank method: the value at rank ⌈p·n⌉ of the
;; sorted values (p = 0 gives the minimum). docs/decisions/0008-aggregates.md
(define (nearest-rank sorted p)
  (define n (length sorted))
  (and (positive? n) (list-ref sorted (max 0 (sub1 (exact-ceiling (* p n)))))))

(define (percentile-agg f p)
  (holistic 'percentile f (λ (m) (nearest-rank (mset-sorted m) p))))

;; AG.median: the middle value, or the mean of the two middle values.
(define (median-agg f)
  (holistic 'median f
            (λ (m)
              (define xs (mset-sorted m))
              (define n (length xs))
              (cond [(zero? n) #f]
                    [(odd? n) (list-ref xs (quotient n 2))]
                    [else (/ (+ (list-ref xs (sub1 (quotient n 2))) (list-ref xs (quotient n 2))) 2)]))))

;; AG.count-distinct: the exact number of distinct values.
(define (count-distinct-agg f)
  (holistic 'count-distinct f (λ (m) (hash-count (mset-counts m)))))

;; AG.mode: the most frequent value; ties go to the value seen first.
(define (mode-agg f)
  (holistic 'mode f
            (λ (m)
              (for/fold ([best #f] [best-count 0] #:result best) ([p (in-list (mset-by-arrival m))])
                (if (> (cadr p) best-count) (values (car p) (cadr p)) (values best best-count))))))

;; AG.top-k: the k largest values (with repetitions), or with #:by 'frequency
;; the k most frequent values (ties to the value seen first).
(define (top-k-agg f k #:by [by 'value])
  (holistic 'top-k f
            (λ (m)
              (if (eq? by 'value)
                  (take (reverse (mset-sorted m)) (min k (apply + 0 (map car (hash-values (mset-counts m))))))
                  (map car (take (sort (mset-by-arrival m) > #:key cadr)
                                 (min k (hash-count (mset-counts m)))))))))

(provide
 (contract-out
  [collect-agg (-> (-> event? any/c) aggregator?)]
  [percentile-agg (-> (-> event? real?) (real-in 0 1) aggregator?)]
  [median-agg (-> (-> event? real?) aggregator?)]
  [count-distinct-agg (-> (-> event? any/c) aggregator?)]
  [mode-agg (-> (-> event? any/c) aggregator?)]
  [top-k-agg (->* ((-> event? any/c) exact-positive-integer?) (#:by (or/c 'value 'frequency)) aggregator?)]))
