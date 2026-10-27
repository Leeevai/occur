#lang racket/base
;; Approximate, constant-size sketches (spec §8 AG.approx): HyperLogLog for
;; distinct counts, a merging t-digest for quantiles, and Count-Min for
;; frequencies. All are immutable values, so they can live in operator state
;; and checkpoints, and all merge.
(require racket/contract
         racket/list
         racket/math)

;; ===========================================================================
;; A deterministic 64-bit hash: FNV-1a over the printed value, finished with
;; the splitmix64 mixer. Stable across runs and platforms.

(define mask64 (sub1 (expt 2 64)))

(define (fnv1a64 bs)
  (for/fold ([h 14695981039346656037]) ([b (in-bytes bs)])
    (bitwise-and (* (bitwise-xor h b) 1099511628211) mask64)))

(define (mix64 z0)
  (let* ([z (bitwise-and (+ z0 #x9E3779B97F4A7C15) mask64)]
         [z (bitwise-and (* (bitwise-xor z (arithmetic-shift z -30)) #xBF58476D1CE4E5B9) mask64)]
         [z (bitwise-and (* (bitwise-xor z (arithmetic-shift z -27)) #x94D049BB133111EB) mask64)])
    (bitwise-xor z (arithmetic-shift z -31))))

(define (hash64 v) (mix64 (fnv1a64 (string->bytes/utf-8 (format "~s" v)))))

;; ===========================================================================
;; HyperLogLog with 2^b registers. The top b bits of the hash pick a
;; register; the register keeps the maximum rank (leading zeros + 1) of the
;; remaining bits. Registers are kept sparsely.

(struct hll (b registers) #:transparent)   ; registers: hash index -> rank

(define (make-hll [b 12]) (hll b (hash)))

(define (hll-add h v)
  (define b (hll-b h))
  (define x (hash64 v))
  (define idx (arithmetic-shift x (- (- 64 b))))
  (define w (bitwise-and x (sub1 (expt 2 (- 64 b)))))
  (define rank (add1 (- (- 64 b) (integer-length w))))
  (define old (hash-ref (hll-registers h) idx 0))
  (if (> rank old) (hll b (hash-set (hll-registers h) idx rank)) h))

(define (hll-merge a b)
  (unless (= (hll-b a) (hll-b b)) (raise-arguments-error 'hll-merge "precisions differ"))
  (hll (hll-b a) (for/fold ([r (hll-registers a)]) ([(i k) (in-hash (hll-registers b))])
                   (hash-update r i (λ (o) (max o k)) 0))))

(define (hll-estimate h)
  (define m (expt 2 (hll-b h)))
  (define alpha (/ 0.7213 (+ 1 (/ 1.079 m))))
  (define zeros (- m (hash-count (hll-registers h))))
  (define sum (+ zeros (for/sum ([k (in-hash-values (hll-registers h))]) (expt 2.0 (- k)))))
  (define e (/ (* alpha m m) sum))
  (if (and (<= e (* 2.5 m)) (positive? zeros))
      (* m (log (/ m zeros)))          ; small-range correction: linear counting
      e))

;; ===========================================================================
;; Merging t-digest (Dunning) with the k1 scale function and compression δ.
;; Centroids are (mean . weight), sorted by mean; new points wait in a
;; buffer and are merged in when it fills. The exact minimum and maximum are
;; kept for the tails.

(struct tdigest (delta centroids buffer weight lo hi) #:transparent)

(define (make-tdigest [delta 100]) (tdigest delta '() '() 0 #f #f))   ; lo, hi: #f until a point arrives

(define (k1 delta q) (* (/ delta (* 2 pi)) (asin (- (* 2 q) 1))))

(define (compress delta points total)
  (define sorted (sort points < #:key car))
  (cond
    [(null? sorted) '()]
    [else
     (let loop ([ps (cdr sorted)] [cur (car sorted)] [q0 0] [acc '()])
       (cond
         [(null? ps) (reverse (cons cur acc))]
         [else
          (define c (car ps))
          (define q (/ (+ q0 (cdr cur) (cdr c)) total))
          (if (<= (- (k1 delta (min 1 q)) (k1 delta (/ q0 total))) 1)
              (loop (cdr ps)
                    (cons (/ (+ (* (car cur) (cdr cur)) (* (car c) (cdr c))) (+ (cdr cur) (cdr c)))
                          (+ (cdr cur) (cdr c)))
                    q0 acc)
              (loop (cdr ps) c (+ q0 (cdr cur)) (cons cur acc)))]))]))

(define (tdigest-flush t)
  (if (null? (tdigest-buffer t))
      t
      (struct-copy tdigest t
                   [centroids (compress (tdigest-delta t) (append (tdigest-centroids t) (tdigest-buffer t))
                                        (tdigest-weight t))]
                   [buffer '()])))

(define (tdigest-add t x)
  (define t2 (struct-copy tdigest t [buffer (cons (cons (exact->inexact x) 1) (tdigest-buffer t))]
                          [weight (add1 (tdigest-weight t))]
                          [lo (if (and (tdigest-lo t) (<= (tdigest-lo t) x)) (tdigest-lo t) x)]
                          [hi (if (and (tdigest-hi t) (>= (tdigest-hi t) x)) (tdigest-hi t) x)]))
  (if (>= (length (tdigest-buffer t2)) (* 5 (tdigest-delta t2))) (tdigest-flush t2) t2))

(define (tdigest-merge a b)
  (tdigest-flush (tdigest (tdigest-delta a)
                          (append (tdigest-centroids a) (tdigest-centroids b))
                          (append (tdigest-buffer a) (tdigest-buffer b))
                          (+ (tdigest-weight a) (tdigest-weight b))
                          (extreme < (tdigest-lo a) (tdigest-lo b)) (extreme > (tdigest-hi a) (tdigest-hi b)))))

(define (extreme better? a b) (cond [(not a) b] [(not b) a] [(better? b a) b] [else a]))

(define (tdigest-quantile t0 p)
  (define t (tdigest-flush t0))
  (define cs (tdigest-centroids t))
  (define W (tdigest-weight t))
  (cond
    [(zero? W) #f]
    [(<= p 0) (tdigest-lo t)]
    [(>= p 1) (tdigest-hi t)]
    [(= (length cs) 1) (car (car cs))]
    [else
     (define target (* p W))
     ;; centers: cumulative weight at each centroid's middle
     (define centers (for/fold ([acc '()] [cum 0] #:result (reverse acc)) ([c cs])
                       (values (cons (+ cum (/ (cdr c) 2)) acc) (+ cum (cdr c)))))
     (define (lerp x0 y0 x1 y1 x) (if (= x0 x1) y0 (+ y0 (* (- y1 y0) (/ (- x x0) (- x1 x0))))))
     (cond
       [(< target (car centers)) (lerp 0 (tdigest-lo t) (car centers) (car (car cs)) target)]
       [(> target (last centers)) (lerp (last centers) (car (last cs)) W (tdigest-hi t) target)]
       [else
        (for/first ([ca centers] [cb (cdr centers)] [a cs] [b (cdr cs)] #:when (<= ca target cb))
          (lerp ca (car a) cb (car b) target))])]))

;; ===========================================================================
;; Count-Min: d rows of w counters; row i hashes x to (h1 + i·h2) mod w. The
;; estimate is the minimum over the rows, never an underestimate.

(struct count-min (width depth counts) #:transparent)   ; counts: hash (row . col) -> n

(define (make-count-min [width 272] [depth 5]) (count-min width depth (hash)))

(define (cm-cols cm x)
  (define h (hash64 x))
  (define h1 (bitwise-and h #xFFFFFFFF))
  (define h2 (bitwise-ior 1 (arithmetic-shift h -32)))
  (for/list ([i (in-range (count-min-depth cm))]) (cons i (modulo (+ h1 (* i h2)) (count-min-width cm)))))

(define (count-min-add cm x [n 1])
  (struct-copy count-min cm
               [counts (for/fold ([c (count-min-counts cm)]) ([cell (cm-cols cm x)])
                         (hash-update c cell (λ (v) (+ v n)) 0))]))

(define (count-min-estimate cm x)
  (apply min (for/list ([cell (cm-cols cm x)]) (hash-ref (count-min-counts cm) cell 0))))

(define (count-min-merge a b)
  (struct-copy count-min a
               [counts (for/fold ([c (count-min-counts a)]) ([(k v) (in-hash (count-min-counts b))])
                         (hash-update c k (λ (o) (+ o v)) 0))]))

(provide
 (contract-out
  [hash64 (-> any/c exact-nonnegative-integer?)]
  [struct hll ([b exact-positive-integer?] [registers hash?])]
  [make-hll (->* () ((integer-in 4 16)) hll?)]
  [hll-add (-> hll? any/c hll?)]
  [hll-merge (-> hll? hll? hll?)]
  [hll-estimate (-> hll? real?)]
  [tdigest? (-> any/c boolean?)]
  [make-tdigest (->* () ((and/c real? positive?)) tdigest?)]
  [tdigest-add (-> tdigest? real? tdigest?)]
  [tdigest-merge (-> tdigest? tdigest? tdigest?)]
  [tdigest-quantile (-> tdigest? (real-in 0 1) (or/c #f real?))]
  [count-min? (-> any/c boolean?)]
  [make-count-min (->* () (exact-positive-integer? exact-positive-integer?) count-min?)]
  [count-min-add (->* (count-min? any/c) (exact-nonnegative-integer?) count-min?)]
  [count-min-estimate (-> count-min? any/c exact-nonnegative-integer?)]
  [count-min-merge (-> count-min? count-min? count-min?)]))
