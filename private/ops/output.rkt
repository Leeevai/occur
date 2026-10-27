#lang racket/base
;; Output policies that act on streams of results (spec §12.3): edge
;; triggering, rate limits, and coalescing.
(require racket/contract
         racket/list
         "../time.rkt"
         "../event.rkt"
         "../operator.rkt"
         (only-in "../scheduler.rkt" strictly-after))

;; ===========================================================================
;; OUT.coalesce: several outputs of one instance that are due together
;; become one output: its interval is [min start, max end], its
;; multiplicities are summed, it records how many were merged, and its
;; constituents are the merged outputs. Numeric attributes named in
;; #:sum are summed; other attributes come from the latest output.

(define (coalesce-events es #:count-attr [count-attr 'merged] #:sum [sum-attrs '()])
  (define latest (last es))
  (define payload
    (for/fold ([p (event-payload latest)]) ([a (in-list sum-attrs)])
      (hash-set p a (apply + (map (λ (e) (event-attr/default e a 0)) es)))))
  (make-event (event-type latest)
              (hash-set payload count-attr (length es))
              #:ts (apply time-min (map event-ts es)) #:te (apply time-max (map event-te es))
              #:n (apply + (map event-n es)) #:src 'coalesce #:constituents es))

;; Outputs arriving at one processing instant for one key are held until
;; processing time moves past that instant (or the script ends), then
;; emitted as one, stamped with that instant.
(define (coalesce-operator #:key [key #f] #:count-attr [count-attr 'merged] #:sum [sum-attrs '()])
  (make-operator
   'coalesce
   #:key key
   #:init-key (λ (g k c) '())
   #:on-event (λ (g held e c)
                (if (null? held) (list (ctx-now c) e) (append held (list e))))
   #:deadline (λ (held) (and (pair? held) (strictly-after (car held))))
   #:on-timer (λ (g held c)
                (define es (cdr held))
                (cond [(null? (cdr es)) (emit! c (car es)) #f]
                      [else (emit! c (coalesce-events es #:count-attr count-attr #:sum sum-attrs)) #f]))))

;; ===========================================================================
;; OUT.rate-limit (Δ, mode): per instance, at most one output per Δ of
;; processing time since the previous output. In mode drop, outputs inside
;; the cap are discarded and counted. In mode defer, the output waits until
;; the cap ends and outputs that arrive meanwhile join it, so nothing is
;; lost (merged with #:merge, coalescing by default).

(struct rl (last held) #:transparent)   ; last: time of the previous output or #f; held: list

(define (rate-limit-operator Δ #:mode [mode 'defer] #:key [key #f]
                             #:merge [merge (λ (es) (if (null? (cdr es)) (car es) (coalesce-events es)))])
  (make-operator
   'rate-limit
   #:key key
   #:init-key (λ (g k c) (rl #f '()))
   #:on-event
   (λ (g st e c)
     (define now (ctx-now c))
     (cond
       [(or (not (rl-last st)) (>= now (+ (rl-last st) Δ)))
        (if (null? (rl-held st))
            (begin (emit! c e) (rl now '()))
            (begin (emit! c (merge (append (rl-held st) (list e)))) (rl now '())))]
       [(eq? mode 'drop) (count! c 'rate-limited) st]
       [else (rl (rl-last st) (append (rl-held st) (list e)))]))
   #:deadline (λ (st) (and (pair? (rl-held st)) (+ (rl-last st) Δ)))
   #:on-timer
   (λ (g st c)
     (cond [(and (pair? (rl-held st)) (>= (ctx-now c) (+ (rl-last st) Δ)))
            (emit! c (merge (rl-held st)))
            (rl (ctx-now c) '())]
           [else st]))))

;; ===========================================================================
;; OUT.edge: report only transitions of a condition, entering it and
;; optionally leaving it, never while it persists. Events for which enter?
;; holds enter the condition, events for which leave? holds leave it; each
;; is reported only when it changes the state, the others are counted.

(define (edge-operator #:enter? enter? #:leave? [leave? (λ (e) #f)] #:report-leave? [report-leave? #t]
                       #:key [key #f] #:initially [initially #f])
  (make-operator
   'edge
   #:monotone? #f
   #:key key
   #:init-key (λ (g k c) initially)
   #:on-event
   (λ (g inside? e c)
     (cond
       [(and (not inside?) (enter? e)) (emit! c e) #t]
       [(and inside? (leave? e)) (when report-leave? (emit! c e)) (or initially #f)]
       [else (count! c 'persisting) inside?]))))

(provide
 (contract-out
  [coalesce-events (->* ((non-empty-listof event?)) (#:count-attr symbol? #:sum (listof symbol?)) event?)]
  [coalesce-operator (->* () (#:key (or/c #f (-> event? any/c)) #:count-attr symbol? #:sum (listof symbol?)) operator?)]
  [rate-limit-operator (->* (duration?) (#:mode (or/c 'drop 'defer) #:key (or/c #f (-> event? any/c))
                                         #:merge (-> (non-empty-listof event?) event?))
                            operator?)]
  [edge-operator (->* (#:enter? (-> event? any/c))
                      (#:leave? (-> event? any/c) #:report-leave? boolean? #:key (or/c #f (-> event? any/c))
                       #:initially boolean?)
                      operator?)]))
