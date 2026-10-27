#lang racket/base
;; Derived pattern families (spec §11.4). All are expressible with the
;; operators of §11.3, windows, aggregation and where clauses; engines offer
;; them as named forms. The first group (after Etzion and Niblett) are
;; predicates for where clauses; the second group (from data-arrival
;; trigger engines, §16) are pattern constructors and operators.
(require racket/contract
         racket/list
         racket/math
         "../time.rkt"
         "../event.rkt"
         "../operator.rkt"
         "../progress.rkt"
         "../context.rkt"
         "ast.rkt")

;; ===========================================================================
;; PT.threshold: an aggregate of the matched or windowed events crosses k.

(define (crosses? v op k)
  (and v (case op [(>) (> v k)] [(>=) (>= v k)] [(<) (< v k)] [(<=) (<= v k)] [(=) (= v k)])))

;; P where op(agg(bindings), k)
(define (threshold-pattern body agg op k) (where body (λ (bs) (crosses? (agg bs) op k))))

;; ===========================================================================
;; Trends over the successive values of a Kleene binding.

(define (pairwise ok? xs) (for/and ([a (in-list xs)] [b (in-list (if (null? xs) '() (cdr xs)))]) (ok? a b)))
(define (increasing? xs) (pairwise < xs))            ; PT.trend.increasing
(define (decreasing? xs) (pairwise > xs))            ; PT.trend.decreasing
(define (non-increasing? xs) (pairwise >= xs))       ; PT.trend.non-increasing
(define (non-decreasing? xs) (pairwise <= xs))       ; PT.trend.non-decreasing
(define (stable? xs [eps 0])                         ; PT.trend.stable
  (or (null? xs) (<= (- (apply max xs) (apply min xs)) eps)))
(define (mixed? xs [eps 0])                          ; PT.trend.mixed: none of the above
  (not (or (increasing? xs) (decreasing? xs) (non-increasing? xs) (non-decreasing? xs) (stable? xs eps))))

;; The most specific trend: increasing, decreasing, stable, non-decreasing,
;; non-increasing, or mixed.
(define (trend xs [eps 0])
  (cond [(and (pair? xs) (pair? (cdr xs)) (increasing? xs)) 'increasing]
        [(and (pair? xs) (pair? (cdr xs)) (decreasing? xs)) 'decreasing]
        [(stable? xs eps) 'stable]
        [(non-decreasing? xs) 'non-decreasing]
        [(non-increasing? xs) 'non-increasing]
        [else 'mixed]))

;; ===========================================================================
;; PT.relative: a binding is the minimum or maximum among the matched events.

(define (relative-max? x xs [key values]) (for/and ([y (in-list xs)]) (>= (key x) (key y))))
(define (relative-min? x xs [key values]) (for/and ([y (in-list xs)]) (<= (key x) (key y))))

;; ===========================================================================
;; PT.spatial.distance: distance(loc(e), anchor) within or beyond r, with
;; min, max and avg variants over several locations.

(define (within-distance? loc anchor r) (<= (distance loc anchor) r))
(define (beyond-distance? loc anchor r) (> (distance loc anchor) r))
(define (min-distance locs anchor) (apply min (map (λ (l) (distance l anchor)) locs)))
(define (max-distance locs anchor) (apply max (map (λ (l) (distance l anchor)) locs)))
(define (avg-distance locs anchor) (/ (apply + (map (λ (l) (distance l anchor)) locs)) (length locs)))

;; ===========================================================================
;; PT.spatial.moving: moving toward or away from an anchor, or in a
;; constant direction, judged from successive locations.

(define (moving-toward? locs anchor) (decreasing? (map (λ (l) (distance l anchor)) locs)))
(define (moving-away? locs anchor) (increasing? (map (λ (l) (distance l anchor)) locs)))
(define (heading a b) (atan (- (location-y b) (location-y a)) (- (location-x b) (location-x a))))
(define (constant-direction? locs #:tolerance [tol 0.1])
  (define hs (for/list ([a (in-list locs)] [b (in-list (if (null? locs) '() (cdr locs)))]) (heading a b)))
  (or (null? hs)
      (for/and ([h (in-list hs)])
        (define d (abs (- h (car hs))))
        (<= (min d (- (* 2 pi) d)) tol))))

;; ===========================================================================
;; Data-arrival families (§16) as patterns. Their policies are listed with
;; each constructor; §12 defines them.

;; PT.accumulate: (b:X)+ where count(b) >= N and span(b) >= S, over the
;; contributions since the last evaluation (WN.since-last), with SEL.next,
;; OUT.first and CON.cumulative. count is the multiplicity sum.
(define (accumulate-pattern type n s #:bind [b 'b])
  (where (kleene+ (prim type #:bind b))
         (λ (bs)
           (define xs (hash-ref bs b))
           (and (>= (apply + (map event-n xs)) n)
                (>= (- (apply max (map event-te xs)) (apply min (map event-ts xs))) s)))))

;; PT.silence: (ACT ∨ x:X) ; ¬X within Δ on the hybrid clock, with
;; CON.recent and OUT.first per initiator: it fires once when the watched
;; data goes quiet for Δ, measured from the newest data or the activation.
(define (silence-pattern type Δ #:bind [x 'x])
  (absence (disj act (prim type #:bind x)) (prim type) Δ #:clock 'hybrid))

;; PT.recovery: s:Silence ; x:X with SEL.next and OUT.first: the first
;; contribution after a silence. With PT.silence this is OUT.edge.
(define (recovery-pattern silence-type type)
  (seq (prim silence-type #:bind 's) (prim type #:bind 'x)))

;; ===========================================================================
;; PT.window-close: every window of WN.tumbling(w, o) that closes under a
;; key's data-driven watermark (TM.keyed-watermark, any or all), with the
;; windows that closed together coalesced into one output (OUT.coalesce).
;; The grid is anchored at the first event's window. An output covers the
;; closed windows [first start, last end], counts them (`windows`), and
;; reports the multiplicity of the events since the previous output
;; (`count`).

(struct wc-state (kwm anchor next pending) #:transparent)   ; next: start of the first unclosed window

(define (window-close-operator size #:offset [offset 0] #:mode [mode 'all] #:kinds [kinds #f]
                               #:kind kind #:key key #:type [type 'WindowClosed])
  (make-operator
   'window-close
   #:monotone? #f
   #:key key
   #:init-key (λ (g k c) (wc-state empty-keyed-wm #f #f '()))
   #:on-event
   (λ (g st e c)
     (define kwm (keyed-wm-observe (wc-state-kwm st) (kind e) (event-te e)))
     (define anchor (or (wc-state-anchor st)
                        (+ offset (* size (floor (/ (- (event-ts e) offset) size))))))
     (define next (or (wc-state-next st) anchor))
     (define pending (cons e (wc-state-pending st)))
     (define W (keyed-wm-value kwm mode kinds))
     (define closed (if (< W +inf.0) (max 0 (floor (/ (- W next) size))) 0))
     (cond
       [(positive? closed)
        (define end (+ next (* closed size)))
        (define contents (reverse pending))
        (emit! c (make-event type `((key . ,(ctx-key c)) (windows . ,closed)
                                    (count . ,(apply + (map event-n contents))))
                             #:ts next #:te end #:src 'window-close #:constituents contents))
        (wc-state kwm anchor end '())]
       [else (wc-state kwm anchor next pending)]))))

(provide
 (contract-out
  [crosses? (-> (or/c #f real?) (or/c '> '>= '< '<= '=) real? boolean?)]
  [threshold-pattern (-> pat? (-> hash? (or/c #f real?)) (or/c '> '>= '< '<= '=) real? pat?)]
  [increasing? (-> (listof real?) boolean?)]
  [decreasing? (-> (listof real?) boolean?)]
  [non-increasing? (-> (listof real?) boolean?)]
  [non-decreasing? (-> (listof real?) boolean?)]
  [stable? (->* ((listof real?)) (real?) boolean?)]
  [mixed? (->* ((listof real?)) (real?) boolean?)]
  [trend (->* ((listof real?)) (real?) symbol?)]
  [relative-max? (->* (any/c list?) ((-> any/c real?)) boolean?)]
  [relative-min? (->* (any/c list?) ((-> any/c real?)) boolean?)]
  [within-distance? (-> location? location? real? boolean?)]
  [beyond-distance? (-> location? location? real? boolean?)]
  [min-distance (-> (non-empty-listof location?) location? real?)]
  [max-distance (-> (non-empty-listof location?) location? real?)]
  [avg-distance (-> (non-empty-listof location?) location? real?)]
  [moving-toward? (-> (listof location?) location? boolean?)]
  [moving-away? (-> (listof location?) location? boolean?)]
  [constant-direction? (->* ((listof location?)) (#:tolerance real?) boolean?)]
  [accumulate-pattern (->* (symbol? real? real?) (#:bind symbol?) pat?)]
  [silence-pattern (->* (symbol? duration?) (#:bind symbol?) pat?)]
  [recovery-pattern (-> symbol? symbol? pat?)]
  [window-close-operator (->* ((and/c real? positive?) #:kind (-> event? any/c) #:key (-> event? any/c))
                              (#:offset real? #:mode (or/c 'any 'all) #:kinds (or/c #f list?) #:type symbol?)
                              operator?)]))
