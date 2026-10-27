#lang racket/base
;; Time, progress, and correctness (spec §6): keyed data-driven watermarks,
;; the future-skew guard, lateness policies, timers, and idleness.
(require racket/contract
         racket/list
         "time.rkt"
         "event.rkt"
         "operator.rkt")

;; ===========================================================================
;; TM.keyed-watermark
;;
;; Per key k, a high-water mark hw_k(m) per kind m (−∞ for a kind never
;; seen). For watched kinds M:
;;   W_k^any = max_{m ∈ M} hw_k(m)        W_k^all = min_{m ∈ M} hw_k(m)
;; "All" waits for the slowest kind. When M is #f (watch every kind), M is
;; the set of kinds seen so far on the key.

(struct keyed-wm (marks) #:transparent)   ; marks: immutable hash kind -> hw
(define empty-keyed-wm (keyed-wm (hash)))

(define (keyed-wm-observe w kind t)
  (keyed-wm (hash-update (keyed-wm-marks w) kind (λ (old) (time-max old t)) -inf.0)))

(define (keyed-wm-mark w kind) (hash-ref (keyed-wm-marks w) kind -inf.0))

(define (watched w kinds) (or kinds (hash-keys (keyed-wm-marks w))))

(define (keyed-wm-any w kinds)
  (define ms (watched w kinds))
  (if (null? ms) -inf.0 (apply time-max (map (λ (m) (keyed-wm-mark w m)) ms))))

(define (keyed-wm-all w kinds)
  (define ms (watched w kinds))
  (if (null? ms) -inf.0 (apply time-min (map (λ (m) (keyed-wm-mark w m)) ms))))

(define (keyed-wm-value w mode kinds)
  (if (eq? mode 'any) (keyed-wm-any w kinds) (keyed-wm-all w kinds)))

;; An operator that tracks W_k per key and emits a Progress event whenever it
;; advances: payload (key, watermark, mode). Events pass through on port 0;
;; progress goes to port 1.
(define (keyed-watermark-operator #:key key #:kind kind #:kinds [kinds #f] #:mode [mode 'all])
  (make-operator
   'keyed-watermark
   #:outputs 2
   #:key key
   #:init-key (λ (g k c) empty-keyed-wm)
   #:on-event
   (λ (g w e c)
     (emit! c e 0)
     (define m (kind e))
     (cond
       [(and kinds (not (member m kinds))) w]
       [else
        (define before (keyed-wm-value w mode kinds))
        (define w2 (keyed-wm-observe w m (event-te e)))
        (define after (keyed-wm-value w2 mode kinds))
        (when (> after before)
          (emit! c (make-event 'Progress `((key . ,(ctx-key c)) (watermark . ,after) (mode . ,mode))
                               #:at after #:src 'keyed-watermark #:constituents (list e))
                 1))
        w2]))))

(provide
 empty-keyed-wm
 (contract-out
  [keyed-wm? (-> any/c boolean?)]
  [keyed-wm-observe (-> keyed-wm? any/c time? keyed-wm?)]
  [keyed-wm-mark (-> keyed-wm? any/c time?)]
  [keyed-wm-any (-> keyed-wm? (or/c #f list?) time?)]
  [keyed-wm-all (-> keyed-wm? (or/c #f list?) time?)]
  [keyed-wm-value (-> keyed-wm? (or/c 'any 'all) (or/c #f list?) time?)]
  [keyed-watermark-operator (->* (#:key (-> event? any/c) #:kind (-> event? any/c))
                                 (#:kinds (or/c #f list?) #:mode (or/c 'any 'all))
                                 operator?)]))
