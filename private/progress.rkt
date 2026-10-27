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

;; ===========================================================================
;; TM.skew-guard: drop and count any event with te(e) > now + B. Under
;; high-water-mark merging, one far-future timestamp would otherwise make
;; every later real event late.

(define (skew-guard-operator bound)
  (stateless-operator
   'skew-guard
   (λ (e c)
     (if (> (event-te e) (+ (ctx-now c) bound))
         (count! c 'future-skew)
         (emit! c e)))))

;; ===========================================================================
;; TM.lateness: an event is late when it arrives with te(e) < W. The policy
;; drops it (and counts it), sends it to a side output (port 1), or accepts
;; it when it is within an allowed lateness L (te(e) >= W − L).

(define (lateness-operator policy)
  (make-operator
   'lateness
   #:outputs 2
   #:on-event
   (λ (g s e c)
     (define W (ctx-watermark c))
     (cond
       [(>= (event-te e) W) (emit! c e)]
       [(eq? policy 'drop) (count! c 'late)]
       [(eq? policy 'side) (count! c 'late) (emit! c e 1)]
       [(and (pair? policy) (eq? (car policy) 'allow))
        (cond [(>= (event-te e) (- W (cadr policy))) (count! c 'late-accepted) (emit! c e)]
              [else (count! c 'late)])])
     s)))

(define lateness-policy/c
  (or/c 'drop 'side (list/c 'allow duration?)))

(provide
 lateness-policy/c
 (contract-out
  [skew-guard-operator (-> duration? operator?)]
  [lateness-operator (-> lateness-policy/c operator?)]))

;; ===========================================================================
;; TM.timer: callbacks at an event time t, which fire when W >= t, or at a
;; processing time t. The delay operator holds each event for d and releases
;; it from such a timer.

(define (insert-by-due due x lst)
  (cond [(null? lst) (list (cons due x))]
        [(< due (car (car lst))) (cons (cons due x) lst)]
        [else (cons (car lst) (insert-by-due due x (cdr lst)))]))

(define (delay-operator d #:clock [clock 'event-time] #:key [key #f])
  (define event-clock? (eq? clock 'event-time))
  (make-operator
   'delay
   #:key key
   #:init-key (λ (g k c) '())
   #:on-event (λ (g pending e c)
                (insert-by-due (+ (if event-clock? (event-te e) (ctx-now c)) d) e pending))
   #:deadline (λ (pending) (and (not event-clock?) (pair? pending) (car (car pending))))
   #:event-deadline (λ (pending) (and event-clock? (pair? pending) (car (car pending))))
   #:on-timer
   (λ (g pending c)
     (define limit (if event-clock? (ctx-watermark c) (ctx-now c)))
     (let loop ([p pending])
       (cond [(and (pair? p) (<= (car (car p)) limit)) (emit! c (cdr (car p))) (loop (cdr p))]
             [(null? p) #f]
             [else p])))))

;; ===========================================================================
;; TM.idle: event time advances only when data arrives, so noticing that
;; nothing arrives needs a processing-time or hybrid timer. The idle operator
;; emits one Idle event per key once nothing has arrived for d: measured from
;; the last arrival (processing clock) or from the newest event time compared
;; with the clock (hybrid). New data re-arms it.

(struct idle-state (anchor notified?) #:transparent)

(define (idle-operator d #:clock [clock 'processing-time] #:key [key #f])
  (make-operator
   'idle
   #:monotone? #f
   #:key key
   #:init-key (λ (g k c) (idle-state #f #f))
   #:on-event (λ (g s e c)
                (define anchor (if (eq? clock 'hybrid) (event-te e) (ctx-now c)))
                (idle-state (if (idle-state-anchor s) (time-max anchor (idle-state-anchor s)) anchor) #f))
   #:deadline (λ (s) (and (idle-state-anchor s) (not (idle-state-notified? s)) (+ (idle-state-anchor s) d)))
   #:on-timer
   (λ (g s c)
     (cond
       [(and (not (idle-state-notified? s)) (>= (ctx-now c) (+ (idle-state-anchor s) d)))
        (emit! c (make-event 'Idle `((key . ,(ctx-key c)) (since . ,(idle-state-anchor s)) (after . ,d))
                             #:ts (idle-state-anchor s) #:te (ctx-now c) #:src 'idle))
        (idle-state (idle-state-anchor s) #t)]
       [else s]))))

(provide
 (contract-out
  [delay-operator (->* (duration?) (#:clock (or/c 'event-time 'processing-time)
                                    #:key (or/c #f (-> event? any/c)))
                       operator?)]
  [idle-operator (->* (duration?) (#:clock (or/c 'processing-time 'hybrid)
                                   #:key (or/c #f (-> event? any/c)))
                      operator?)]))
