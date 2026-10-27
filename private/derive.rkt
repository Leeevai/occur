#lang racket/base
;; Derivation, closure, and action (spec §13).
(require racket/contract
         racket/list
         racket/string
         file/sha1
         "time.rkt"
         "event.rkt"
         "operator.rkt"
         "dataflow.rkt")

;; ===========================================================================
;; DR.summary: instead of its constituents, an output may carry a summary:
;; the interval it covers, the multiplicity sum, the number of windows or
;; ticks merged, and the contributing sub-streams and kinds (AG.sources). A
;; consumer that needs the data re-reads it (ES.reference).

(define (make-summary #:interval iv #:samples samples #:merged [merged 1] #:sources [sources '()])
  (hasheq 'interval (list (interval-start iv) (interval-end iv)) 'samples samples 'merged merged 'sources sources))

;; A summarized output: a derived event with a summary and no constituents.
(define (summarized-output type payload #:ts ts #:te te #:samples samples #:merged [merged 1]
                           #:sources [sources '()] #:src [src #f])
  (make-event type (hash-set (->hash payload) 'summary
                             (make-summary #:interval (interval ts te) #:samples samples #:merged merged
                                           #:sources sources))
              #:ts ts #:te te #:n 1 #:src src))

;; Summarize a derived event: keep its interval, replace its constituents
;; with a summary of them.
(define (summarize e #:sources-of [sources-of #f])
  (define cs (event-constituents e))
  (event-update e
                #:payload (hash-set (event-payload e) 'summary
                                    (make-summary #:interval (event-interval e)
                                                  #:samples (apply + 0 (map event-n cs))
                                                  #:merged (event-attr/default e 'merged 1)
                                                  #:sources (if sources-of (remove-duplicates (map sources-of cs)) '())))
                #:constituents '()))

(define (event-summary e) (event-attr/default e 'summary #f))

(define (->hash v) (if (hash? v) v (for/hasheq ([p (in-list v)]) (values (car p) (cdr p)))))

;; ===========================================================================
;; DR.output-identity: an output's deduplication identity is derived from
;; its content: operator instance, key, reason, covered interval, and
;; multiplicity, not the emission time. A retried or replayed emission has
;; the same identity. An output that covers no new data also includes its
;; emission time, so that two of them are not mistaken for one.

(define (output-identity #:instance instance #:key key #:reason reason #:interval iv #:samples samples
                         #:emitted-at [emitted-at #f] #:no-new-data? [no-new-data? #f])
  (define fields (list instance key reason (interval-start iv) (interval-end iv) samples
                       (if no-new-data? emitted-at 'none)))
  (sha1 (open-input-bytes (string->bytes/utf-8 (format "~s" fields)))))

;; The default identity of any event: its type, payload (without an id
;; attribute), interval, and multiplicity.
(define (event-identity e)
  (or (event-attr/default e 'identity #f)
      (sha1 (open-input-bytes
             (string->bytes/utf-8
              (format "~s" (list (event-type e)
                                 (sort (for/list ([(k v) (in-hash (event-payload e))] #:unless (memq k '(identity emitted-at)))
                                         (cons k v))
                                       symbol<? #:key car)
                                 (event-ts e) (event-te e) (event-n e))))))))

;; Downstream deduplication: drop outputs whose identity has been seen.
(define (dedupe-outputs outputs #:identity [identity event-identity])
  (let loop ([os outputs] [seen (hash)] [acc '()])
    (cond [(null? os) (reverse acc)]
          [else
           (define item (output-item (car os)))
           (cond [(not (event? item)) (loop (cdr os) seen (cons (car os) acc))]
                 [else (define id (identity item))
                       (if (hash-ref seen id #f)
                           (loop (cdr os) seen acc)
                           (loop (cdr os) (hash-set seen id #t) (cons (car os) acc)))])])))

(define (identity-dedupe-operator #:identity [identity event-identity])
  (make-operator
   'dedupe-outputs
   #:init-global (λ () #f)
   #:init-key (λ (g k c) (hash))
   #:on-event (λ (g seen e c)
                (define id (identity e))
                (cond [(hash-ref seen id #f) (count! c 'duplicates) seen]
                      [else (emit! c e) (hash-set seen id #t)]))))

;; ===========================================================================
;; DR.ordered: outputs are delivered grouped by key, in order within each
;; key, so that a consumer handles one key's outputs one at a time. The
;; runner delivers each step's outputs this way; delivery-groups splits a
;; list of outputs into the per-key sequences a consumer sees.

(define (delivery-groups outputs)
  (define keys (remove-duplicates (map output-key outputs)))
  (for/list ([k (in-list keys)])
    (cons k (filter (λ (o) (equal? (output-key o) k)) outputs))))

;; ===========================================================================
;; DR.act: a side effect such as an alert, a service call, or a view update.
;; An action is not an event unless it emits one (#:completion, for example
;; ActionCompleted, with C = {e}).

(define (action-operator proc #:completion [completion #f])
  (stateless-operator
   'act
   (λ (e c)
     (define result (proc e))
     (when completion
       (emit! c (make-event completion `((result . ,result)) #:ts (event-ts e) #:te (event-te e)
                            #:src 'act #:constituents (list e)))))))

;; ===========================================================================
;; DR.view: a per-key state derived continuously from events, queried on
;; demand (view-ref) or exported on a schedule, but not itself an event
;; unless it is emitted. An export every `period` of processing time emits a
;; View event per key with the key's current projection.

(struct view-state (state last-arrival newest next-export) #:transparent)

;; With #:lifecycle? #f the view ignores entity lifecycle signals, as a view
;; keyed by something other than the entity (such as a sensor) must.
(define (view-operator #:key [key #f] #:init init #:update update
                       #:project [project (λ (s now arrival newest) s)]
                       #:export-every [period #f] #:type [type 'View]
                       #:lifecycle? [lifecycle? #t])
  (make-operator
   'view
   #:key key
   #:lifecycle? lifecycle?
   #:init-key (λ (g k c) (view-state (init) #f -inf.0
                                     (and period (+ (* period (floor (/ (ctx-now c) period))) period))))
   #:on-event (λ (g st e c)
                (struct-copy view-state st [state (update (view-state-state st) e (ctx-now c))]
                             [last-arrival (ctx-now c)] [newest (time-max (view-state-newest st) (event-te e))]))
   #:deadline (λ (st) (view-state-next-export st))
   #:on-timer
   (λ (g st c)
     (define now (ctx-now c))
     (cond
       [(and period (>= now (view-state-next-export st)))
        (define v (project (view-state-state st) now (view-state-last-arrival st) (view-state-newest st)))
        (emit! c (make-event type `((key . ,(ctx-key c)) (view . ,v)) #:at now #:src 'view))
        (struct-copy view-state st [next-export (+ (* period (floor (/ now period))) period)])]
       [else st]))))

;; Query a view after (or during) a run: the projected state of one key.
(define (view-ref result node key #:project [project (λ (s now arrival newest) s)])
  (define st (hash-ref (cdr (hash-ref (run-result-states result) node)) key #f))
  (and st (project (view-state-state st) (run-result-now result) (view-state-last-arrival st) (view-state-newest st))))

;; Health by arrival age (now − last arrival) and data age (now − newest
;; event time): live, late (arriving, but with old data), quiet, or silent.
(define (classify-health #:now now #:last-arrival last-arrival #:newest newest
                         #:live live #:silent silent #:stale stale)
  (cond
    [(not last-arrival) 'silent]
    [(> (- now last-arrival) silent) 'silent]
    [(> (- now last-arrival) live) 'quiet]
    [(> (- now newest) stale) 'late]
    [else 'live]))

(provide
 (contract-out
  [make-summary (->* (#:interval interval? #:samples exact-nonnegative-integer?)
                     (#:merged exact-nonnegative-integer? #:sources list?) hash?)]
  [summarized-output (->* (symbol? (or/c hash? list?) #:ts time? #:te time? #:samples exact-nonnegative-integer?)
                          (#:merged exact-nonnegative-integer? #:sources list? #:src any/c) event?)]
  [summarize (->* (event?) (#:sources-of (or/c #f (-> event? any/c))) event?)]
  [event-summary (-> event? (or/c #f hash?))]
  [output-identity (->* (#:instance any/c #:key any/c #:reason any/c #:interval interval?
                         #:samples exact-nonnegative-integer?)
                        (#:emitted-at (or/c #f time?) #:no-new-data? boolean?)
                        string?)]
  [event-identity (-> event? string?)]
  [dedupe-outputs (->* ((listof output?)) (#:identity (-> event? any/c)) (listof output?))]
  [identity-dedupe-operator (->* () (#:identity (-> event? any/c)) operator?)]
  [delivery-groups (-> (listof output?) (listof (cons/c any/c (listof output?))))]
  [action-operator (->* ((-> event? any/c)) (#:completion (or/c #f symbol?)) operator?)]
  [view-operator (->* (#:init (-> any/c) #:update (-> any/c event? time? any/c))
                      (#:key (or/c #f (-> event? any/c)) #:project procedure?
                       #:export-every (or/c #f (and/c real? positive?)) #:type symbol?
                       #:lifecycle? boolean?)
                      operator?)]
  [view-ref (->* (run-result? symbol? any/c) (#:project procedure?) any/c)]
  [classify-health (-> #:now time? #:last-arrival (or/c #f time?) #:newest time?
                       #:live duration? #:silent duration? #:stale duration?
                       (or/c 'live 'late 'quiet 'silent))]))

;; The summarize stage: every event replaced by its summarized form.
(define (map-like-summarize)
  (stateless-operator 'summarize (λ (e c) (emit! c (summarize e)))))

(provide
 (contract-out
  [map-like-summarize (-> operator?)]))
