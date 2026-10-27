#lang racket/base
;; ES.operator: operators as nodes of a push-based dataflow graph.
;;
;; An operator with n inputs maps input histories to output events. Here an
;; operator is a bundle of callbacks over explicit, immutable state:
;;
;;   init-global : -> g                       state shared by the node
;;   init-key    : g key ctx -> s             per-key state, created lazily
;;   on-event    : g s event ctx -> s         an event arrives on (ctx-port ctx)
;;   on-timer    : g s ctx -> s               a deadline fired; (ctx-clock ctx) says which
;;   deadline    : s -> (or/c #f time)        next processing-time deadline
;;   event-deadline : s -> (or/c #f time)     next event-time deadline (fires when W >= t)
;;   on-watermark : g keys W ctx -> (values g keys)    optional
;;   on-control  : g keys msg ctx -> (values g keys)   optional, broadcast control
;;   on-signal   : g s-or-#f signal ctx -> s-or-#f     optional, in-band lifecycle signals
;;   on-retract  : g s event ctx -> s                  optional, ⊖e arrived
;;
;; The runner partitions every operator by its key function, so each key's
;; state is isolated (ES.keyed) and operators are written for one partition.
;; Callbacks never mutate state; they return the new state and emit items
;; through the context. Because all state is immutable, a checkpoint is a
;; set of references (TM.checkpoint).
(require racket/contract
         "time.rkt"
         "event.rkt")

;; Items that flow along edges: events, retractions ⊖e, and signals.
(struct retraction (event) #:transparent)
(struct signal (kind key time data) #:transparent)
(define (item? v) (or (event? v) (retraction? v) (signal? v)))

(struct operator (name arity outputs monotone? key lifecycle? config?
                  init-global init-key on-event on-timer deadline event-deadline
                  on-watermark on-control on-signal on-retract
                  judge coalesce instance-of)
  #:transparent)

(define (pass-through g s e ctx) (emit! ctx e) s)

(define (make-operator name
                       #:arity [arity 1]
                       #:outputs [outputs 1]
                       #:monotone? [monotone? #t]
                       #:key [key #f]
                       #:lifecycle? [lifecycle? #f]
                       #:config? [config? #f]
                       #:init-global [init-global (λ () #f)]
                       #:init-key [init-key (λ (g k ctx) #f)]
                       #:on-event [on-event pass-through]
                       #:on-timer [on-timer (λ (g s ctx) s)]
                       #:deadline [deadline (λ (s) #f)]
                       #:event-deadline [event-deadline (λ (s) #f)]
                       #:on-watermark [on-watermark #f]
                       #:on-control [on-control #f]
                       #:on-signal [on-signal #f]
                       #:on-retract [on-retract #f]
                       #:judge [judge #f]
                       #:coalesce [coalesce #f]
                       #:instance-of [instance-of #f])
  (operator name arity outputs monotone? key lifecycle? config?
            init-global init-key on-event on-timer deadline event-deadline
            on-watermark on-control on-signal on-retract judge coalesce instance-of))

;; A stateless operator: proc receives the event and the context and emits.
(define (stateless-operator name proc #:monotone? [monotone? #t] #:outputs [outputs 1]
                            #:arity [arity 1])
  (make-operator name #:monotone? monotone? #:outputs outputs #:arity arity
                 #:on-event (λ (g s e ctx) (proc e ctx) s)))

;; ---------------------------------------------------------------------------
;; The callback context. The runner creates one per callback and reads the
;; accumulated emissions, counter increments, and reports afterwards.

(struct ctx (node key port clock now watermark
                  [emitted #:mutable] [counts #:mutable] [reports #:mutable]))

(define (make-ctx #:node node #:key key #:port [port 0] #:clock [clock #f]
                  #:now now #:watermark watermark)
  (ctx node key port clock now watermark '() '() '()))

(define (emit! c item [port 0])
  (set-ctx-emitted! c (cons (cons port item) (ctx-emitted c))))
(define (count! c name [n 1])
  (set-ctx-counts! c (cons (cons name n) (ctx-counts c))))
(define (report! c datum)
  (set-ctx-reports! c (cons datum (ctx-reports c))))

(define (ctx-emissions c) (reverse (ctx-emitted c)))
(define (ctx-count-list c) (reverse (ctx-counts c)))
(define (ctx-report-list c) (reverse (ctx-reports c)))

(provide
 (contract-out
  [struct retraction ([event event?])]
  [struct signal ([kind symbol?] [key any/c] [time (or/c #f time?)] [data any/c])]
  [item? (-> any/c boolean?)]
  [struct operator ([name symbol?] [arity exact-positive-integer?] [outputs exact-positive-integer?]
                    [monotone? boolean?] [key (or/c #f procedure?)] [lifecycle? boolean?]
                    [config? boolean?]
                    [init-global procedure?] [init-key procedure?] [on-event procedure?]
                    [on-timer procedure?] [deadline procedure?] [event-deadline procedure?]
                    [on-watermark (or/c #f procedure?)] [on-control (or/c #f procedure?)]
                    [on-signal (or/c #f procedure?)] [on-retract (or/c #f procedure?)]
                    [judge (or/c #f procedure?)] [coalesce (or/c #f procedure?)]
                    [instance-of (or/c #f procedure?)])]
  [make-operator (->* (symbol?)
                      (#:arity exact-positive-integer? #:outputs exact-positive-integer?
                       #:monotone? boolean? #:key (or/c #f (-> event? any/c))
                       #:lifecycle? boolean? #:config? boolean?
                       #:init-global (-> any/c)
                       #:init-key (-> any/c any/c ctx? any/c)
                       #:on-event (-> any/c any/c event? ctx? any/c)
                       #:on-timer (-> any/c any/c ctx? any/c)
                       #:deadline (-> any/c (or/c #f time?))
                       #:event-deadline (-> any/c (or/c #f time?))
                       #:on-watermark (or/c #f (-> any/c hash? time? ctx? (values any/c hash?)))
                       #:on-control (or/c #f (-> any/c hash? any/c ctx? (values any/c hash?)))
                       #:on-signal (or/c #f (-> any/c any/c signal? ctx? any/c))
                       #:on-retract (or/c #f (-> any/c any/c event? ctx? any/c))
                       #:judge (or/c #f (-> any/c hash? event? any/c))
                       #:coalesce (or/c #f (-> (non-empty-listof event?) event?))
                       #:instance-of (or/c #f (-> event? any/c)))
                      operator?)]
  [stateless-operator (->* (symbol? (-> event? ctx? any))
                           (#:monotone? boolean? #:outputs exact-positive-integer?
                            #:arity exact-positive-integer?)
                           operator?)]
  [ctx? (-> any/c boolean?)]
  [make-ctx (->* (#:node symbol? #:key any/c #:now time? #:watermark time?)
                 (#:port exact-nonnegative-integer? #:clock (or/c #f symbol?))
                 ctx?)]
  [ctx-node (-> ctx? symbol?)]
  [ctx-key (-> ctx? any/c)]
  [ctx-port (-> ctx? exact-nonnegative-integer?)]
  [ctx-clock (-> ctx? (or/c #f symbol?))]
  [ctx-now (-> ctx? time?)]
  [ctx-watermark (-> ctx? time?)]
  [emit! (->* (ctx? item?) (exact-nonnegative-integer?) void?)]
  [count! (->* (ctx? symbol?) (exact-integer?) void?)]
  [report! (-> ctx? any/c void?)]
  [ctx-emissions (-> ctx? (listof (cons/c exact-nonnegative-integer? item?)))]
  [ctx-count-list (-> ctx? (listof (cons/c symbol? exact-integer?)))]
  [ctx-report-list (-> ctx? list?)]))
