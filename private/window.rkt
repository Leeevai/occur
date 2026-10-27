#lang racket/base
;; Windows (spec §7).
;;
;; A window assignment maps each event to the windows it belongs to; windows
;; are half-open intervals [s, e). A window operator keeps, per key, the open
;; windows with an accumulator each, and emits one event per (key, window)
;; when the window fires. The accumulator protocol is shared with the
;; aggregates of §8, which plug in as accumulators.
(require racket/contract
         racket/list
         racket/match
         "time.rkt"
         "event.rkt"
         "operator.rkt"
         "progress.rkt"
         "scheduler.rkt")

;; ===========================================================================
;; Accumulators: init -> s, add : s e -> s, result : s -> payload hash, and
;; optionally remove : s e -> s (for invertible aggregates, AG.incremental).

(struct accumulator (init add result remove) #:transparent)

(define (make-accumulator #:init init #:add add #:result result #:remove [remove #f])
  (accumulator init add result remove))

;; The default window accumulator: the multiplicity sum and the event count.
(define contents-accumulator
  (make-accumulator #:init (λ () (cons 0 0))
                    #:add (λ (s e) (cons (+ (car s) (event-n e)) (add1 (cdr s))))
                    #:remove (λ (s e) (cons (- (car s) (event-n e)) (sub1 (cdr s))))
                    #:result (λ (s) (hasheq 'count (car s) 'events (cdr s)))))

;; ===========================================================================
;; Window outputs. Each (key, window) produces one derived event with
;; ts = s and te = e and the window contents as constituents (§8 "Output").
;; An empty window carries a summary instead of constituents (DR.summary).

(define (window-output type key s e acc st contents #:ts [ts s] #:te [te e] #:extra [extra (hasheq)])
  (define payload
    (for/fold ([p (accumulator-result-of acc st)]) ([(k v) (in-hash extra)]) (hash-set p k v)))
  (define p2 (if key (hash-set payload 'key key) payload))
  (make-event type (if (null? contents) (hash-set p2 'summary (hasheq 'events 0)) p2)
              #:ts ts #:te te #:src 'window #:constituents contents))

(define (accumulator-result-of acc st) ((accumulator-result acc) st))

;; ===========================================================================
;; Grid windows: tumbling and sliding (WN.tumbling, WN.sliding)

;; A grid: the windows [start, start + size) for start = offset + k·step.
(struct grid (size step offset) #:transparent)

(define (tumbling size #:offset [offset 0]) (grid size size offset))
(define (sliding size slide #:offset [offset 0]) (grid size slide offset))

;; Windows of the grid containing time t (half-open), as a list of starts.
(define (grid-starts-at g t)
  (define size (grid-size g)) (define step (grid-step g)) (define o (grid-offset g))
  (define kmax (floor (/ (- t o) step)))
  (define kmin (add1 (floor (/ (- t o size) step))))
  (for/list ([k (in-range kmin (add1 kmax))]) (+ o (* k step))))

;; Windows of the grid intersecting [a, b] (assignment by overlap).
(define (grid-starts-overlapping g a b)
  (define size (grid-size g)) (define step (grid-step g)) (define o (grid-offset g))
  (define kmax (floor (/ (- b o) step)))
  (define kmin (add1 (floor (/ (- a o size) step))))
  (for/list ([k (in-range kmin (add1 kmax))]) (+ o (* k step))))

(define (assign g e mode t-now)
  (case mode
    [(start) (grid-starts-at g (event-ts e))]
    [(end) (grid-starts-at g (event-te e))]
    [(overlap) (grid-starts-overlapping g (event-ts e) (event-te e))]
    [(arrival) (grid-starts-at g t-now)]))

;; One window's state. fired: #f, or the event it emitted (for corrections).
(struct win (start end acc contents fired) #:transparent)

;; Per-key state of a grid window operator.
;;  windows: hash start -> win; anchor: the first window's start (#f before
;;  any event); next: the earliest grid start not yet fired (fire-empty);
;;  kwm: the keyed watermark (when windows close under it).
(struct grid-state (windows anchor next kwm) #:transparent)

;; closer: 'watermark (the node's watermark), 'clock (processing time: events
;;         are assigned by arrival time and windows fire when the clock
;;         reaches their end), or (list 'keyed mode kinds kind-fn) (the key's
;;         data-driven watermark, TM.keyed-watermark).
;;
;; An event-time window [s, e) fires when W >= e. With allowed lateness L,
;; a late event updates a fired window, emitted as a correction, until
;; W >= e + L; after that the window is dropped and later events for it are
;; counted as late. With fire-empty?, every grid window from the first one
;; fires even when it is empty (WN.empty).
(define (grid-window-operator g acc
                              #:key [key #f]
                              #:assign [mode 'start]
                              #:lateness [lateness 0]
                              #:fire-empty? [fire-empty? #f]
                              #:closer [closer 'watermark]
                              #:type [type 'Window])
  (define processing? (eq? closer 'clock))
  (define keyed? (pair? closer))
  (define size (grid-size g))
  (define step (grid-step g))
  (define (limit-of c st) (cond [processing? (ctx-now c)]
                                [keyed? (keyed-wm-value (grid-state-kwm st) (cadr closer) (caddr closer))]
                                [else (ctx-watermark c)]))
  (define (fresh-win s) (win s (+ s size) ((accumulator-init acc)) '() #f))
  (define (emit-window! c w)
    (define out (window-output type (ctx-key c) (win-start w) (win-end w) acc (win-acc w)
                               (reverse (win-contents w))))
    (if (win-fired w) (emit-correction! c (win-fired w) out) (emit! c out))
    (struct-copy win w [fired out]))
  ;; Fire every window due at the limit W, then drop windows past lateness.
  (define (fire st c)
    (define W (limit-of c st))
    ;; empty grid windows, in order, from the first one not yet fired
    (define-values (ws1 next)
      (if (and fire-empty? (grid-state-next st))
          (let loop ([ws (grid-state-windows st)] [next (grid-state-next st)])
            (cond [(<= (+ next size) W)
                   (define w (or (hash-ref ws next #f) (fresh-win next)))
                   (loop (hash-set ws next (if (win-fired w) w (emit-window! c w))) (+ next step))]
                  [else (values ws next)]))
          (values (grid-state-windows st) (grid-state-next st))))
    ;; every other due window that has not fired
    (define ws2 (for/fold ([ws ws1]) ([s (in-list (sort (hash-keys ws1) <))])
                  (define w (hash-ref ws s))
                  (if (and (not (win-fired w)) (<= (win-end w) W)) (hash-set ws s (emit-window! c w)) ws)))
    (define kept (for/hash ([(s w) (in-hash ws2)]
                            #:unless (and (win-fired w) (<= (+ (win-end w) lateness) W)))
                   (values s w)))
    (struct-copy grid-state st [windows kept] [next next]))
  (define (add-event st e c)
    (define starts (assign g e (if processing? 'arrival mode) (ctx-now c)))
    (define anchor (or (grid-state-anchor st) (and (pair? starts) (apply min starts))))
    (define W (limit-of c st))
    (define ws
      (for/fold ([ws (grid-state-windows st)]) ([s (in-list starts)])
        (define existing (hash-ref ws s #f))
        (cond
          [(< s anchor) (count! c 'before-first-window) ws]
          [(and (not existing) (not processing?) (<= (+ s size lateness) W))
           (count! c 'late) ws]
          [else
           (define w (or existing (fresh-win s)))
           (define w2 (struct-copy win w [acc ((accumulator-add acc) (win-acc w) e)]
                                   [contents (cons e (win-contents w))]))
           (hash-set ws s (if (win-fired w) (emit-window! c w2) w2))])))
    (struct-copy grid-state st [windows ws] [anchor anchor] [next (or (grid-state-next st) anchor)]))
  (define (next-deadline st)
    (define cands
      (append (for/list ([w (in-hash-values (grid-state-windows st))])
                (if (win-fired w) (+ (win-end w) lateness) (win-end w)))
              (if (and fire-empty? (grid-state-next st)) (list (+ (grid-state-next st) size)) '())))
    (and (pair? cands) (apply min cands)))
  (make-operator
   'window
   #:monotone? #f
   #:key key
   #:init-key (λ (gl k c) (grid-state (hash) #f #f empty-keyed-wm))
   #:on-event
   (λ (gl st e c)
     (define st1 (if keyed?
                     (struct-copy grid-state st
                                  [kwm (keyed-wm-observe (grid-state-kwm st) ((cadddr closer) e) (event-te e))])
                     st))
     (define st2 (add-event st1 e c))
     (if keyed? (fire st2 c) st2))
   #:event-deadline (λ (st) (and (eq? closer 'watermark) (next-deadline st)))
   #:deadline (λ (st) (and processing? (next-deadline st)))
   #:on-timer (λ (gl st c) (fire st c))))

(provide
 (contract-out
  [struct accumulator ([init procedure?] [add procedure?] [result procedure?] [remove (or/c #f procedure?)])]
  [make-accumulator (->* (#:init (-> any/c) #:add (-> any/c event? any/c) #:result (-> any/c hash?))
                         (#:remove (or/c #f (-> any/c event? any/c)))
                         accumulator?)]
  [contents-accumulator accumulator?]
  [window-output (->* (symbol? any/c time? time? accumulator? any/c (listof event?))
                      (#:ts time? #:te time? #:extra hash?)
                      event?)]
  [struct grid ([size (and/c real? positive?)] [step (and/c real? positive?)] [offset real?])]
  [tumbling (->* ((and/c real? positive?)) (#:offset real?) grid?)]
  [sliding (->* ((and/c real? positive?) (and/c real? positive?)) (#:offset real?) grid?)]
  [grid-starts-at (-> grid? time? (listof time?))]
  [grid-window-operator (->* (grid? accumulator?)
                             (#:key (or/c #f (-> event? any/c))
                              #:assign (or/c 'start 'end 'overlap)
                              #:lateness duration?
                              #:fire-empty? boolean?
                              #:closer any/c
                              #:type symbol?)
                             operator?)]))
