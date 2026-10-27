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
