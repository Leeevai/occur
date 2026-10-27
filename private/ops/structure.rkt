#lang racket/base
;; Stream-structure operations (spec §5).
(require racket/contract
         racket/list
         "../time.rkt"
         "../event.rkt"
         "../operator.rkt")

;; ---------------------------------------------------------------------------
;; ST.merge: S₁ ∪ … ∪ Sₙ, interleaved by arrival. The events pass unchanged.

(define (merge-operator n)
  (make-operator 'merge #:arity n))

;; ---------------------------------------------------------------------------
;; ST.key-by: partition by κ. The node passes events through unchanged; the
;; runner keeps one partition per key, and downstream stateful operators are
;; built with the same κ. Re-keying by another attribute is the same
;; operation with a different κ.

(define (key-by-operator κ)
  (make-operator 'key-by #:key κ))

(define (attribute-key-fn attr) (λ (e) (event-attr e attr)))

(provide
 (contract-out
  [merge-operator (-> exact-positive-integer? operator?)]
  [key-by-operator (-> (-> event? any/c) operator?)]
  [attribute-key-fn (-> symbol? (-> event? any/c))]))

;; ---------------------------------------------------------------------------
;; ST.dedupe: drop e if an earlier event with the same dedup key lies within
;; horizon h of it. Times are event times (te) or, with the processing-time
;; clock, arrival times. Each key remembers the times of its recent events
;; and forgets them once the watermark (or the clock) is a horizon past.

(define (dedupe-operator key-fn #:horizon h #:clock [clock 'event-time])
  (define (time-of e c) (if (eq? clock 'processing-time) (ctx-now c) (event-te e)))
  (define (limit c) (if (eq? clock 'processing-time) (ctx-now c) (ctx-watermark c)))
  (make-operator
   'dedupe
   #:key key-fn
   #:init-key (λ (g k c) '())
   #:on-event
   (λ (g times e c)
     (define t (time-of e c))
     (cond
       [(for/or ([u (in-list times)]) (<= (abs (- t u)) h))
        (count! c 'duplicates)
        (cons t times)]
       [else (emit! c e) (cons t times)]))
   #:deadline (λ (times) (and (eq? clock 'processing-time) (pair? times) (+ (apply min times) h)))
   #:event-deadline (λ (times) (and (eq? clock 'event-time) (pair? times) (+ (apply min times) h)))
   #:on-timer
   (λ (g times c)
     (define keep (filter (λ (u) (> (+ u h) (limit c))) times))
     (and (pair? keep) keep))))

(provide
 (contract-out
  [dedupe-operator (->* ((-> event? any/c) #:horizon duration?)
                        (#:clock (or/c 'event-time 'processing-time))
                        operator?)]))

;; ---------------------------------------------------------------------------
;; ST.monotonic-merge: per sub-stream s, a high-water mark hw(s), starting at
;; −∞, and counters. For each arrival e on s:
;;
;;   new(e) = share(e, hw(s))
;;   if new(e) > 0: emit a contribution c with ts(c) = max(ts(e), hw(s)),
;;                  te(c) = te(e), n(c) = new(e); then hw(s) := te(e)
;;   late(s) += n(e) − new(e)
;;
;; Re-applying an applied event is a no-op, data wholly behind hw(s) is
;; dropped and counted (a lateness policy), and hw(s) is a per-sub-stream
;; watermark.

(struct hwm-state (hw late contributions) #:transparent)

;; One arrival on a sub-stream with state s: the new state.
(define (merge-step s e c)
  (define hw (hwm-state-hw s))
  (define new (batch-share e hw))
  (define dropped (- (event-n e) new))
  (when (> dropped 0) (count! c 'late dropped))
  (cond
    [(> new 0)
     (emit! c (event-update e #:id (fresh-id) #:ts (time-max (event-ts e) hw) #:n new
                            #:src 'monotonic-merge #:constituents (list e)))
     (hwm-state (event-te e) (+ (hwm-state-late s) dropped) (add1 (hwm-state-contributions s)))]
    [else (hwm-state hw (+ (hwm-state-late s) dropped) (hwm-state-contributions s))]))

(define initial-hwm (hwm-state -inf.0 0 0))

;; Keyed by sub-stream; with #:entity, keyed by the entity that owns the
;; sub-streams, with one high-water mark per sub-stream inside the entity's
;; state, so that dropping an entity (CX.entity) forgets its marks.
(define (monotonic-merge-operator substream-fn #:entity [entity-fn #f])
  (if entity-fn
      (make-operator
       'monotonic-merge
       #:key entity-fn
       #:lifecycle? #t
       #:init-key (λ (g k c) (hash))
       #:on-event
       (λ (g marks e c)
         (define sub (substream-fn e))
         (hash-set marks sub (merge-step (hash-ref marks sub initial-hwm) e c))))
      (make-operator
       'monotonic-merge
       #:key substream-fn
       #:init-key (λ (g k c) initial-hwm)
       #:on-event (λ (g s e c) (merge-step s e c)))))

(provide
 (contract-out
  [struct hwm-state ([hw time?] [late exact-nonnegative-integer?]
                     [contributions exact-nonnegative-integer?])]
  [monotonic-merge-operator (->* ((-> event? any/c)) (#:entity (or/c #f (-> event? any/c))) operator?)]))

;; ---------------------------------------------------------------------------
;; ST.reorder: buffer events and release them in event-time order up to the
;; watermark. Order is (te, ts, arrival), so a release at watermark W never
;; precedes a later arrival (which has te >= W). An event arriving behind the
;; watermark (te < W) is late: dropped and counted, or emitted at once.

(struct reorder-state (buffer seq) #:transparent)   ; buffer: sorted list of (key . event)

(define (reorder-key e seq) (vector (event-te e) (event-ts e) seq))
(define (rkey<? a b)
  (or (< (vector-ref a 0) (vector-ref b 0))
      (and (= (vector-ref a 0) (vector-ref b 0))
           (or (< (vector-ref a 1) (vector-ref b 1))
               (and (= (vector-ref a 1) (vector-ref b 1)) (< (vector-ref a 2) (vector-ref b 2)))))))

(define (insert-sorted x lst)
  (cond [(null? lst) (list x)]
        [(rkey<? (car x) (car (car lst))) (cons x lst)]
        [else (cons (car lst) (insert-sorted x (cdr lst)))]))

(define (reorder-operator #:key [key #f] #:late [late 'drop])
  (make-operator
   'reorder
   #:monotone? #t
   #:key key
   #:init-key (λ (g k c) (reorder-state '() 0))
   #:on-event
   (λ (g s e c)
     (cond
       [(< (event-te e) (ctx-watermark c))
        (if (eq? late 'emit) (emit! c e) (count! c 'late))
        s]
       [else (reorder-state (insert-sorted (cons (reorder-key e (reorder-state-seq s)) e)
                                           (reorder-state-buffer s))
                            (add1 (reorder-state-seq s)))]))
   #:event-deadline (λ (s) (and (pair? (reorder-state-buffer s))
                                (vector-ref (car (car (reorder-state-buffer s))) 0)))
   #:on-timer
   (λ (g s c)
     (let loop ([buf (reorder-state-buffer s)])
       (cond [(and (pair? buf) (<= (vector-ref (car (car buf)) 0) (ctx-watermark c)))
              (emit! c (cdr (car buf)))
              (loop (cdr buf))]
             [else (reorder-state buf (reorder-state-seq s))])))))

(provide
 (contract-out
  [reorder-operator (->* () (#:key (or/c #f (-> event? any/c)) #:late (or/c 'drop 'emit)) operator?)]))

;; ---------------------------------------------------------------------------
;; ST.throttle: emit at most r events per Δ, or every n-th event, or each
;; event with probability p. Excess events are dropped and counted (compare
;; OUT.rate-limit, which defers instead). Rate buckets are aligned to the
;; epoch: [kΔ, (k+1)Δ). The sampling generator is a 48-bit linear
;; congruential generator kept in the state, so runs are reproducible.

(define lcg-a 25214903917)
(define lcg-c 11)
(define lcg-m (expt 2 48))
(define (lcg-next x) (modulo (+ (* lcg-a x) lcg-c) lcg-m))

(define (throttle-operator mode #:key [key #f])
  (define (decide s e c)
    (case (car mode)
      [(rate)
       (define r (list-ref mode 1))
       (define Δ (list-ref mode 2))
       (define clock (if (> (length mode) 3) (list-ref mode 3) 'event-time))
       (define t (if (eq? clock 'processing-time) (ctx-now c) (event-te e)))
       (define bucket (floor (/ t Δ)))
       (define used (if (and s (equal? (car s) bucket)) (cdr s) 0))
       (values (< used r) (cons bucket (if (< used r) (add1 used) used)))]
      [(every)
       (define i (add1 (or s 0)))
       (values (zero? (modulo i (list-ref mode 1))) i)]
      [(probability)
       (define x (lcg-next (or s (list-ref mode 2))))
       (values (< (/ x lcg-m) (list-ref mode 1)) x)]))
  (make-operator
   'throttle
   #:key key
   #:on-event (λ (g s e c)
                (define-values (keep? s2) (decide s e c))
                (if keep? (emit! c e) (count! c 'throttled))
                s2)))

(define throttle-mode/c
  (or/c (list/c 'rate exact-positive-integer? (and/c real? positive?))
        (list/c 'rate exact-positive-integer? (and/c real? positive?) (or/c 'event-time 'processing-time))
        (list/c 'every exact-positive-integer?)
        (list/c 'probability (real-in 0 1) exact-nonnegative-integer?)))

(provide
 throttle-mode/c
 (contract-out
  [throttle-operator (->* (throttle-mode/c) (#:key (or/c #f (-> event? any/c))) operator?)]))
