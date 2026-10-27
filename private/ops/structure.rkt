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

(define (monotonic-merge-operator substream-fn)
  (make-operator
   'monotonic-merge
   #:key substream-fn
   #:init-key (λ (g k c) (hwm-state -inf.0 0 0))
   #:on-event
   (λ (g s e c)
     (define hw (hwm-state-hw s))
     (define new (batch-share e hw))
     (define dropped (- (event-n e) new))
     (when (> dropped 0) (count! c 'late dropped))
     (cond
       [(> new 0)
        (emit! c (event-update e #:id (fresh-id) #:ts (time-max (event-ts e) hw) #:n new
                               #:src 'monotonic-merge #:constituents (list e)))
        (hwm-state (event-te e) (+ (hwm-state-late s) dropped) (add1 (hwm-state-contributions s)))]
       [else (hwm-state hw (+ (hwm-state-late s) dropped) (hwm-state-contributions s))]))))

(provide
 (contract-out
  [struct hwm-state ([hw time?] [late exact-nonnegative-integer?]
                     [contributions exact-nonnegative-integer?])]
  [monotonic-merge-operator (-> (-> event? any/c) operator?)]))
