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
         racket/math
         "time.rkt"
         "event.rkt"
         "operator.rkt"
         "progress.rkt"
         "scheduler.rkt")

;; ===========================================================================
;; Accumulators: init -> s, add : s e -> s, result : s -> payload hash, and
;; optionally remove : s e -> s (for invertible aggregates) and combine :
;; s s -> s (for associative ones), which incremental sliding windows use
;; (AG.incremental).

(struct accumulator (init add result remove combine) #:transparent)

(define (make-accumulator #:init init #:add add #:result result #:remove [remove #f] #:combine [combine #f])
  (accumulator init add result remove combine))

;; The default window accumulator: the multiplicity sum and the event count.
(define contents-accumulator
  (make-accumulator #:init (λ () (cons 0 0))
                    #:add (λ (s e) (cons (+ (car s) (event-n e)) (add1 (cdr s))))
                    #:remove (λ (s e) (cons (- (car s) (event-n e)) (sub1 (cdr s))))
                    #:combine (λ (a b) (cons (+ (car a) (car b)) (+ (cdr a) (cdr b))))
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
    ;; At the end of the stream (W = +∞), empty windows after the last data
    ;; are not emitted: the walk stops at the end of the newest window.
    (define walk-limit
      (if (eqv? W +inf.0)
          (for/fold ([m -inf.0]) ([w (in-hash-values (grid-state-windows st))]) (time-max m (win-end w)))
          W))
    ;; empty grid windows, in order, from the first one not yet fired
    (define-values (ws1 next)
      (if (and fire-empty? (grid-state-next st))
          (let loop ([ws (grid-state-windows st)] [next (grid-state-next st)])
            (cond [(<= (+ next size) walk-limit)
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
    ;; at the end of the stream no further empty windows fire
    (struct-copy grid-state st [windows kept] [next (if (eqv? W +inf.0) #f next)]))
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
  [struct accumulator ([init procedure?] [add procedure?] [result procedure?] [remove (or/c #f procedure?)]
                        [combine (or/c #f procedure?)])]
  [make-accumulator (->* (#:init (-> any/c) #:add (-> any/c event? any/c) #:result (-> any/c hash?))
                         (#:remove (or/c #f (-> any/c event? any/c)) #:combine (or/c #f (-> any/c any/c any/c)))
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

;; ===========================================================================
;; WN.session: per key, maximal runs of events whose consecutive gaps are
;; < g; the window is [first, last + g). An arriving event may merge two
;; sessions. A session fires when the watermark reaches last + g; with
;; allowed lateness, later merges are emitted as corrections.

(struct session (first last acc contents fired) #:transparent)   ; fired: list of emitted events

(define (session-window-operator gap acc #:key [key #f] #:lateness [lateness 0] #:type [type 'Window])
  (define (end s) (+ (session-last s) gap))
  (define (emit-session! c s)
    (define out (window-output type (ctx-key c) (session-first s) (end s) acc (session-acc s)
                               (reverse (session-contents s))))
    (match (session-fired s)
      ['() (emit! c out)]
      [olds (for ([o (in-list olds)]) (emit! c (retraction o))) (emit! c out)])
    (struct-copy session s [fired (list out)]))
  (define (merge-sessions ss e)
    (define fold-acc
      (for/fold ([a ((accumulator-init acc))]) ([x (in-list (reverse (append* (map session-contents ss))))])
        ((accumulator-add acc) a x)))
    (session (apply time-min (event-ts e) (map session-first ss))
             (apply time-max (event-ts e) (map session-last ss))
             ((accumulator-add acc) fold-acc e)
             (cons e (append* (map session-contents ss)))
             (append* (map session-fired ss))))
  (make-operator
   'session-window
   #:monotone? #f
   #:key key
   #:init-key (λ (gl k c) '())
   #:on-event
   (λ (gl ss e c)
     (define t (event-ts e))
     (define W (ctx-watermark c))
     (define-values (touching others)
       (partition (λ (s) (and (< (- (session-first s) gap) t) (< t (+ (session-last s) gap)))) ss))
     (cond
       [(and (null? touching) (<= (+ t gap lateness) W)) (count! c 'late) ss]
       [(and (pair? touching) (andmap (λ (s) (pair? (session-fired s))) touching)
             (<= (+ t gap lateness) W) (andmap (λ (s) (<= (+ (end s) lateness) W)) touching))
        (count! c 'late) ss]
       [else
        (define merged (merge-sessions touching e))
        (define merged* (if (pair? (session-fired merged)) (emit-session! c merged) merged))
        (sort (cons merged* others) < #:key session-first)]))
   #:event-deadline
   (λ (ss) (define cands (for/list ([s ss]) (if (pair? (session-fired s)) (+ (end s) lateness) (end s))))
     (and (pair? cands) (apply min cands)))
   #:on-timer
   (λ (gl ss c)
     (define W (ctx-watermark c))
     (define fired (for/list ([s (in-list ss)])
                     (if (and (null? (session-fired s)) (<= (end s) W)) (emit-session! c s) s)))
     (define kept (filter (λ (s) (not (and (pair? (session-fired s)) (<= (+ (end s) lateness) W)))) fired))
     (and (pair? kept) kept))))

(provide
 (contract-out
  [session-window-operator (->* ((and/c real? positive?) accumulator?)
                                (#:key (or/c #f (-> event? any/c)) #:lateness duration? #:type symbol?)
                                operator?)]))

;; ===========================================================================
;; WN.count: the last n events, advancing every m events. A window is emitted
;; every m-th event once n events have been seen, with ts and te the minimum
;; and maximum over its contents.
;;
;; AG.incremental: an invertible accumulator is maintained with add and
;; remove as events enter and leave the window. An associative one (with
;; combine but no remove, such as min and max) uses a two-stack queue, which
;; answers the window's aggregate in amortized constant time. Any other
;; accumulator is refolded over the window.

;; The two-stack queue: elements are pushed on the back stack, which keeps
;; their running aggregate; popping takes from the front stack, whose
;; entries hold the aggregate of themselves and everything behind them,
;; refilled from the back when empty.
(struct two-stack (front back back-agg) #:transparent)   ; front: list of (elt . agg); back: list of elt

(define (two-stack-empty) (two-stack '() '() #f))
(define (two-stack-push q x lift combine)
  (two-stack (two-stack-front q) (cons x (two-stack-back q))
             (if (two-stack-back-agg q) (combine (two-stack-back-agg q) (lift x)) (lift x))))
(define (two-stack-pop q lift combine)
  (define front
    (if (null? (two-stack-front q))
        ;; move the back stack over, oldest on top, with suffix aggregates
        (for/fold ([acc '()]) ([x (in-list (two-stack-back q))])
          (cons (cons x (if (null? acc) (lift x) (combine (lift x) (cdr (car acc))))) acc))
        (two-stack-front q)))
  (if (null? (two-stack-front q))
      (two-stack (cdr front) '() #f)
      (two-stack (cdr front) (two-stack-back q) (two-stack-back-agg q))))
(define (two-stack-aggregate q combine)
  (define f (and (pair? (two-stack-front q)) (cdr (car (two-stack-front q)))))
  (define b (two-stack-back-agg q))
  (cond [(and f b) (combine f b)] [f f] [else b]))

(struct count-state (queue size seen acc) #:transparent)   ; queue: newest first

(define (count-window-operator n m acc #:key [key #f] #:type [type 'Window])
  (define remove (accumulator-remove acc))
  (define combine (accumulator-combine acc))
  (define (lift e) ((accumulator-add acc) ((accumulator-init acc)) e))
  (define mode (cond [remove 'invertible] [combine 'two-stack] [else 'refold]))
  (make-operator
   'count-window
   #:key key
   #:init-key (λ (gl k c) (count-state '() 0 0 (case mode
                                                 [(invertible) ((accumulator-init acc))]
                                                 [(two-stack) (two-stack-empty)]
                                                 [else #f])))
   #:on-event
   (λ (gl st e c)
     (define q0 (cons e (count-state-queue st)))
     (define evicted (if (> (length q0) n) (last q0) #f))
     (define q (if evicted (take q0 n) q0))
     (define seen (add1 (count-state-seen st)))
     (define a
       (case mode
         [(invertible) (let ([a1 ((accumulator-add acc) (count-state-acc st) e)])
                         (if evicted (remove a1 evicted) a1))]
         [(two-stack) (let ([q1 (two-stack-push (count-state-acc st) e lift combine)])
                        (if evicted (two-stack-pop q1 lift combine) q1))]
         [else #f]))
     (when (and (= (length q) n) (zero? (modulo seen m)))
       (define contents (reverse q))
       (define st-acc (case mode
                        [(invertible) a]
                        [(two-stack) (two-stack-aggregate a combine)]
                        [else (for/fold ([s ((accumulator-init acc))]) ([x contents]) ((accumulator-add acc) s x))]))
       (emit! c (window-output type (ctx-key c) (apply time-min (map event-ts contents))
                               (apply time-max (map event-te contents)) acc st-acc contents)))
     (count-state q (length q) seen a))))

(provide
 (contract-out
  [count-window-operator (->* (exact-positive-integer? exact-positive-integer? accumulator?)
                              (#:key (or/c #f (-> event? any/c)) #:type symbol?)
                              operator?)]
  [two-stack-empty (-> two-stack?)]
  [two-stack-push (-> two-stack? any/c procedure? procedure? two-stack?)]
  [two-stack-pop (-> two-stack? procedure? procedure? two-stack?)]
  [two-stack-aggregate (-> two-stack? procedure? any/c)]))

;; ===========================================================================
;; WN.landmark: [t0, now), growing forever. After every event at or after
;; t0, the running result is emitted with ts = t0 and te = the newest end.

(define (landmark-window-operator t0 acc #:key [key #f] #:type [type 'Window])
  (make-operator
   'landmark-window
   #:key key
   #:init-key (λ (gl k c) (list ((accumulator-init acc)) -inf.0 '()))
   #:on-event
   (λ (gl st e c)
     (cond
       [(< (event-ts e) t0) (count! c 'before-landmark) st]
       [else
        (define a ((accumulator-add acc) (car st) e))
        (define hi (time-max (cadr st) (event-te e)))
        (define contents (cons e (caddr st)))
        (emit! c (window-output type (ctx-key c) t0 hi acc a (reverse contents)))
        (list a hi contents)]))))

(provide
 (contract-out
  [landmark-window-operator (->* (time? accumulator?) (#:key (or/c #f (-> event? any/c)) #:type symbol?)
                                 operator?)]))

;; ===========================================================================
;; WN.punctuated: a window opened by an event satisfying `open?` and closed by
;; one satisfying `close?`, or by a timeout d after it opened (event time:
;; when W reaches ts(A) + d; processing time: d after the opening arrived).
;; The window's interval is [ts(A), te(B)] (or the deadline); the opening and
;; closing events are constituents, and the accumulator sees the events in
;; between. The output's `reason` is closed or timeout.

(struct punct (start deadline acc inner opener) #:transparent)

(define (punctuated-window-operator open? close? acc
                                    #:key [key #f] #:timeout [timeout #f]
                                    #:timeout-clock [tclock 'event-time] #:type [type 'Window])
  (define (finish c p te reason closer)
    (emit! c (window-output type (ctx-key c) (punct-start p) te acc (punct-acc p)
                            (append (list (punct-opener p)) (reverse (punct-inner p)) (if closer (list closer) '()))
                            #:extra (hasheq 'reason reason)))
    #f)
  (make-operator
   'punctuated-window
   #:monotone? #f
   #:key key
   #:init-key (λ (gl k c) #f)
   #:on-event
   (λ (gl p e c)
     (cond
       [(and (not p) (open? e))
        (punct (event-ts e)
               (and timeout (+ (if (eq? tclock 'processing-time) (ctx-now c) (event-ts e)) timeout))
               ((accumulator-init acc)) '() e)]
       [(not p) (count! c 'outside) p]
       [(close? e) (finish c p (event-te e) 'closed e)]
       [else (struct-copy punct p [acc ((accumulator-add acc) (punct-acc p) e)] [inner (cons e (punct-inner p))])]))
   #:event-deadline (λ (p) (and (punct? p) (eq? tclock 'event-time) (punct-deadline p)))
   #:deadline (λ (p) (and (punct? p) (eq? tclock 'processing-time) (punct-deadline p)))
   #:on-timer
   (λ (gl p c)
     (define limit (if (eq? tclock 'processing-time) (ctx-now c) (ctx-watermark c)))
     (if (and (punct? p) (punct-deadline p) (>= limit (punct-deadline p)))
         (finish c p (punct-deadline p) 'timeout #f)
         p))))

(provide
 (contract-out
  [punctuated-window-operator (->* ((-> event? any/c) (-> event? any/c) accumulator?)
                                   (#:key (or/c #f (-> event? any/c)) #:timeout (or/c #f duration?)
                                    #:timeout-clock (or/c 'event-time 'processing-time) #:type symbol?)
                                   operator?)]))

;; ===========================================================================
;; WN.since-last: per operator instance, the window opens at the first event
;; after the previous output and closes when the instance fires: when
;; `fire?` holds for the accumulated result. The operator's own output is the
;; punctuation. ts and te are the minimum and maximum over the contents.

(define (since-last-window-operator fire? acc #:key [key #f] #:type [type 'Window])
  (make-operator
   'since-last-window
   #:key key
   #:init-key (λ (gl k c) (cons ((accumulator-init acc)) '()))
   #:on-event
   (λ (gl st e c)
     (define a ((accumulator-add acc) (car st) e))
     (define contents (cons e (cdr st)))
     (cond
       [(fire? ((accumulator-result acc) a))
        (define cs (reverse contents))
        (emit! c (window-output type (ctx-key c) (apply time-min (map event-ts cs))
                                (apply time-max (map event-te cs)) acc a cs))
        (cons ((accumulator-init acc)) '())]
       [else (cons a contents)]))))

(provide
 (contract-out
  [since-last-window-operator (->* ((-> hash? any/c) accumulator?)
                                   (#:key (or/c #f (-> event? any/c)) #:type symbol?)
                                   operator?)]))

;; ===========================================================================
;; WN.calendar: windows between consecutive ticks of a calendar schedule,
;; [tick_{i−1}, tick_i).
;;
;; On the processing clock (TM.schedule), events are assigned by arrival
;; time, and a window started mid-period ends at the period's next tick: an
;; instance created at 10:20 on an hourly schedule has the first period
;; [10:00, 11:00). Ticks that came due together close one coalesced window
;; whose `windows` attribute counts them (OUT.coalesce). Calendar windows
;; fire even when empty by default (WN.empty). An unkeyed calendar window
;; starts with the run.
;;
;; On the event-time clock, events are assigned by ts and each window fires
;; when the watermark reaches its closing tick.

(struct cal-state (period-start handled acc contents) #:transparent)

(define (calendar-window-operator expr acc
                                  #:clock [clock 'processing-time]
                                  #:key [key #f]
                                  #:fire-empty? [fire-empty? #t]
                                  #:type [type 'Window])
  (define cr (if (cron? expr) expr (parse-cron expr)))
  (if (eq? clock 'processing-time)
      (processing-calendar cr acc key fire-empty? type)
      (event-calendar cr acc key fire-empty? type)))

(define (processing-calendar cr acc key fire-empty? type)
  (make-operator
   'calendar-window
   #:monotone? #f
   #:key key
   #:eager? #t
   #:lifecycle? #t
   #:init-key (λ (gl k c)
                (define now (ctx-now c))
                (cal-state (or (cron-prev cr now) now) now ((accumulator-init acc)) '()))
   #:on-event (λ (gl st e c)
                (struct-copy cal-state st [acc ((accumulator-add acc) (cal-state-acc st) e)]
                             [contents (cons e (cal-state-contents st))]))
   #:deadline (λ (st) (cron-next cr (cal-state-handled st)))
   #:on-timer
   (λ (gl st c)
     (define ticks (cron-ticks-between cr (cal-state-handled st) (ctx-now c)))
     (cond
       [(null? ticks) st]
       [else
        (define closing (last ticks))
        (when (or fire-empty? (pair? (cal-state-contents st)))
          (emit! c (window-output type (ctx-key c) (cal-state-period-start st) closing acc (cal-state-acc st)
                                  (reverse (cal-state-contents st))
                                  #:extra (hasheq 'windows (length ticks)))))
        (cal-state closing closing ((accumulator-init acc)) '())]))))

(define (event-calendar cr acc key fire-empty? type)
  ;; per key: windows hash start -> win, the anchor, and the next start to fire
  (define (window-of t) (cons (or (cron-prev cr t) -inf.0) (or (cron-next cr t) +inf.0)))
  (define (emit-win! c w)
    (emit! c (window-output type (ctx-key c) (win-start w) (win-end w) acc (win-acc w) (reverse (win-contents w)))))
  (make-operator
   'calendar-window
   #:monotone? #f
   #:key key
   #:init-key (λ (gl k c) (grid-state (hash) #f #f empty-keyed-wm))
   #:on-event
   (λ (gl st e c)
     (define w0 (window-of (event-ts e)))
     (define s (car w0))
     (define anchor (or (grid-state-anchor st) s))
     (cond
       [(or (< s anchor) (<= (cdr w0) (ctx-watermark c))) (count! c 'late) st]
       [else
        (define w (or (hash-ref (grid-state-windows st) s #f)
                      (win s (cdr w0) ((accumulator-init acc)) '() #f)))
        (struct-copy grid-state st
                     [windows (hash-set (grid-state-windows st) s
                                        (struct-copy win w [acc ((accumulator-add acc) (win-acc w) e)]
                                                     [contents (cons e (win-contents w))]))]
                     [anchor anchor] [next (or (grid-state-next st) anchor)])]))
   #:event-deadline
   (λ (st) (define next (grid-state-next st))
     (and next (cron-next cr next)))
   #:on-timer
   (λ (gl st c)
     (define W0 (ctx-watermark c))
     ;; at the end of the stream, stop after the newest window with data
     (define W (if (eqv? W0 +inf.0)
                   (for/fold ([m -inf.0]) ([w (in-hash-values (grid-state-windows st))]) (time-max m (win-end w)))
                   W0))
     (let loop ([ws (grid-state-windows st)] [next (grid-state-next st)])
       (define end (and next (cron-next cr next)))
       (cond
         [(and end (<= end W))
          (define w (hash-ref ws next #f))
          (cond [w (emit-win! c w)]
                [fire-empty? (emit-win! c (win next end ((accumulator-init acc)) '() #f))])
          (loop (hash-remove ws next) end)]
         [else (struct-copy grid-state st [windows ws] [next (if (eqv? W0 +inf.0) #f next)])])))))

(provide
 (contract-out
  [calendar-window-operator (->* ((or/c string? cron?) accumulator?)
                                 (#:clock (or/c 'processing-time 'event-time)
                                  #:key (or/c #f (-> event? any/c))
                                  #:fire-empty? boolean?
                                  #:type symbol?)
                                 operator?)]))
