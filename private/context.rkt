#lang racket/base
;; Contexts (spec §7, after Etzion and Niblett): the scope in which an
;; operator evaluates. Windows are one kind of context.
;;
;; Most contexts are assigners: per key, they map each event to the context
;; instances it belongs to, and a context operator emits one derived copy of
;; the event per instance with a `context` attribute naming it. Downstream
;; stages key by `context` to evaluate per instance. Composite contexts
;; intersect assigners. The state and entity contexts are gates: they decide
;; from other inputs (a state stream, a directory) whether events pass.
(require racket/contract
         racket/list
         racket/math
         "time.rkt"
         "event.rkt"
         "operator.rkt"
         "dataflow.rkt")

;; ===========================================================================
;; Assigners: init -> s; assign : s e ctx -> (values s (listof instance-id))

(struct assigner (name init assign) #:transparent)

(define (stateless-assigner name f) (assigner name (λ () #f) (λ (s e c) (values s (f e)))))

;; Tag e with a context instance: a derived event with C = {e}.
(define (in-context e id)
  (make-event (event-type e) (hash-set (event-payload e) 'context id)
              #:ts (event-ts e) #:te (event-te e) #:n (event-n e) #:src 'context
              #:constituents (list e)))

(define (context-operator a #:key [key #f])
  (make-operator
   (string->symbol (format "context:~a" (assigner-name a)))
   #:key key
   #:init-key (λ (g k c) ((assigner-init a)))
   #:on-event (λ (g s e c)
                (define-values (s2 ids) ((assigner-assign a) s e c))
                (when (null? ids) (count! c 'outside))
                (for ([id (in-list ids)]) (emit! c (in-context e id)))
                s2)))

;; ===========================================================================
;; CX.temporal: a fixed interval, an event interval (opened on A, closed on B
;; or a timeout), a sliding fixed interval, or a sliding event interval.

(define (fixed-interval-context s e)
  (stateless-assigner 'fixed-interval
                      (λ (ev) (if (and (<= s (event-ts ev)) (< (event-ts ev) e)) (list (list 'fixed s e)) '()))))

;; Opened by an event satisfying open?, closed (inclusively) by one satisfying
;; close? or once the event's time passes ts(A) + timeout. The instance is
;; named after the opening event.
(define (event-interval-context open? close? #:timeout [timeout #f])
  (assigner 'event-interval
            (λ () #f)
            (λ (open-ev e c)
              (define (expired?) (and open-ev timeout (> (event-ts e) (+ (event-ts open-ev) timeout))))
              (define current (if (expired?) #f open-ev))
              (cond
                [(and (not current) (open? e)) (values e (list (list 'interval (event-id e))))]
                [(not current) (values #f '())]
                [(close? e) (values #f (list (list 'interval (event-id current))))]
                [else (values current (list (list 'interval (event-id current))))]))))

;; Sliding fixed intervals [kσ, kσ + Δ), like sliding windows.
(define (sliding-interval-context size slide)
  (stateless-assigner 'sliding-interval
                      (λ (ev)
                        (define t (event-ts ev))
                        (for/list ([k (in-range (add1 (floor (/ (- t size) slide))) (add1 (floor (/ t slide))))])
                          (list 'sliding (* k slide) (+ (* k slide) size))))))

;; Each event satisfying open? opens its own instance lasting `duration`;
;; an event belongs to every instance open at its time.
(define (sliding-event-interval-context open? duration)
  (assigner 'sliding-event-interval
            (λ () '())
            (λ (opens e c)
              (define t (event-ts e))
              (define live (filter (λ (o) (< t (+ (event-ts o) duration))) opens))
              (define opens2 (if (open? e) (append live (list e)) live))
              (values opens2 (for/list ([o (in-list opens2)] #:when (<= (event-ts o) t))
                               (list 'sliding-event (event-id o)))))))

;; ===========================================================================
;; CX.segmentation: partition by attribute values; key-by is a special case.

(define (segmentation-context attrs)
  (stateless-assigner 'segmentation
                      (λ (ev) (list (cons 'segment (map (λ (a) (event-attr ev a)) attrs))))))

;; ===========================================================================
;; CX.spatial: within distance r of a fixed location, of an entity, or of an
;; event. Locations are planar (docs/decisions/0007-contexts.md).

(define (distance a b)
  (sqrt (+ (sqr (- (location-x a) (location-x b))) (sqr (- (location-y a) (location-y b))))))

(define (fixed-location-context anchor r #:location [loc-attr 'location])
  (stateless-assigner 'fixed-location
                      (λ (ev) (if (<= (distance (event-attr ev loc-attr) anchor) r)
                                  (list (list 'near anchor r))
                                  '()))))

;; Entities' locations come from a lookup (id -> location or #f) evaluated at
;; each event; the event belongs to the instance of every entity within r.
(define (entity-distance-context locate entities r #:location [loc-attr 'location])
  (stateless-assigner 'entity-distance
                      (λ (ev)
                        (for/list ([id (in-list (entities))]
                                   #:when (let ([l (locate id)])
                                            (and l (<= (distance (event-attr ev loc-attr) l) r))))
                          (list 'near-entity id)))))

;; Events satisfying anchor? open an instance at their location, lasting
;; `duration`; a later event within r of it belongs to it.
(define (event-distance-context anchor? r duration #:location [loc-attr 'location])
  (assigner 'event-distance
            (λ () '())
            (λ (anchors e c)
              (define t (event-ts e))
              (define live (filter (λ (a) (< t (+ (event-ts a) duration))) anchors))
              (define ids (for/list ([a (in-list live)]
                                     #:when (<= (distance (event-attr e loc-attr) (event-attr a loc-attr)) r))
                            (list 'near-event (event-id a))))
              (if (anchor? e)
                  (values (append live (list e)) (append ids (list (list 'near-event (event-id e)))))
                  (values live ids)))))

;; ===========================================================================
;; CX.composite: the intersection of contexts, for example per customer ×
;; per hour. Instance ids are the lists of the parts' ids.

(define (composite-context . parts)
  (assigner 'composite
            (λ () (map (λ (a) ((assigner-init a))) parts))
            (λ (states e c)
              (define-values (states2 id-lists)
                (for/lists (ss ids) ([a (in-list parts)] [s (in-list states)])
                  ((assigner-assign a) s e c)))
              (values states2
                      (let loop ([lists id-lists])
                        (if (null? lists) '(())
                            (for*/list ([x (in-list (car lists))] [rest (in-list (loop (cdr lists)))])
                              (cons x rest))))))))

(provide
 (contract-out
  [struct assigner ([name symbol?] [init procedure?] [assign procedure?])]
  [stateless-assigner (-> symbol? (-> event? list?) assigner?)]
  [in-context (-> event? any/c event?)]
  [context-operator (->* (assigner?) (#:key (or/c #f (-> event? any/c))) operator?)]
  [fixed-interval-context (-> time? time? assigner?)]
  [event-interval-context (->* ((-> event? any/c) (-> event? any/c)) (#:timeout (or/c #f duration?)) assigner?)]
  [sliding-interval-context (-> (and/c real? positive?) (and/c real? positive?) assigner?)]
  [sliding-event-interval-context (-> (-> event? any/c) duration? assigner?)]
  [segmentation-context (-> (listof symbol?) assigner?)]
  [distance (-> location? location? real?)]
  [fixed-location-context (->* (location? real?) (#:location symbol?) assigner?)]
  [entity-distance-context (->* ((-> any/c (or/c #f location?)) (-> list?) real?) (#:location symbol?) assigner?)]
  [event-distance-context (->* ((-> event? any/c) real? duration?) (#:location symbol?) assigner?)]
  [composite-context (->* () () #:rest (listof assigner?) assigner?)]))

;; ===========================================================================
;; CX.state: the operator is active only while an external state holds. Data
;; arrives on port 0; state events arrive on port 1 and set the state to
;; (active? e). While the state holds, data events pass, tagged with the
;; episode (the id of the state event that began it); otherwise they are
;; counted and dropped.

(define (state-context-operator active? #:key [key #f])
  (make-operator
   'context:state
   #:arity 2
   #:key key
   ;; 'inactive, or (list 'active id) with the id of the episode's opening event
   #:init-key (λ (g k c) 'inactive)
   #:on-event (λ (g st e c)
                (cond
                  [(= (ctx-port c) 1)
                   (cond [(not (active? e)) 'inactive]
                         [(pair? st) st]                       ; already active: same episode
                         [else (list 'active (event-id e))])]
                  [(pair? st) (emit! c (in-context e (list 'state (cadr st)))) st]
                  [else (count! c 'inactive) st]))))

;; ===========================================================================
;; CX.entity and CX.activation: the entity lifecycle.
;;
;; The set of keys comes from a reference directory, not only from the
;; data. A directory push replaces the directory whole or patches it per
;; entity. An entity listed as active is activated at once, before it sends
;; anything: a lifecycle signal `activate` flows downstream carrying the
;; activation instant, the moment the context instance begins (CX.activation).
;; An entity marked inactive (deprecated) has its state dropped (signal
;; `drop`), and its later events are ignored and counted. An entity removed
;; from the directory has its state dropped; if it sends data again it is
;; activated afresh. Entities unknown to the directory are activated by their
;; first event.

(struct directory-msg (name entries mode) #:transparent)   ; entries: hash key -> 'active | 'inactive | 'removed

(define (directory-entries->hash entries)
  (cond [(hash? entries) entries]
        [else (for/hash ([p (in-list entries)]) (values (car p) (cdr p)))]))

(define (directory-push name entries #:mode [mode 'replace] #:at [t #f])
  (control (directory-msg name (directory-entries->hash entries) mode) #:at t))

(define (entity-context-operator directory-name #:key key)
  (define (activate! c k) (emit! c (signal 'activate k (ctx-now c) #f)))
  (make-operator
   'context:entity
   #:key key
   #:monotone? #f
   #:init-global (λ () (hash))                    ; key -> 'active | 'inactive
   #:init-key (λ (g k c) 'new)                    ; 'new, or the activation instant
   #:on-event
   (λ (dir s e c)
     (define k (ctx-key c))
     (cond
       [(eq? (hash-ref dir k #f) 'inactive) (count! c 'ignored) s]
       [else
        (define s2 (if (eq? s 'new) (begin (activate! c k) (ctx-now c)) s))
        (emit! c e)
        s2]))
   #:on-control
   (λ (dir keys msg c)
     (cond
       [(and (directory-msg? msg) (equal? (directory-msg-name msg) directory-name))
        (define entries (directory-msg-entries msg))
        (define new-dir
          (if (eq? (directory-msg-mode msg) 'replace)
              (for/hash ([(k v) (in-hash entries)] #:unless (eq? v 'removed)) (values k v))
              (for/fold ([d dir]) ([(k v) (in-hash entries)])
                (if (eq? v 'removed) (hash-remove d k) (hash-set d k v)))))
        (define all-keys (remove-duplicates (append (sort-keys (hash-keys dir)) (sort-keys (hash-keys new-dir)))))
        (define keys2
          (for/fold ([ks keys]) ([k (in-list all-keys)])
            (define old (hash-ref dir k #f))
            (define new (hash-ref new-dir k #f))
            (cond
              [(and (eq? new 'active) (not (eq? old 'active)))
               (activate! c k)
               (hash-set ks k (ctx-now c))]
              [(and (eq? new 'inactive) (not (eq? old 'inactive)))
               (emit! c (signal 'drop k (ctx-now c) 'deprecated))
               (hash-remove ks k)]
              [(and (not new) old)
               (emit! c (signal 'drop k (ctx-now c) 'removed))
               (hash-remove ks k)]
              [else ks])))
        (values new-dir keys2)]
       [else (values dir keys)]))))

(define (sort-keys ks) (sort ks (λ (a b) (string<? (format "~s" a) (format "~s" b)))))

;; Observe activations: every `activate` signal becomes an Activated event
;; at the activation instant.
(define activations-operator
  (make-operator
   'activations
   #:on-event (λ (g s e c) s)
   #:on-signal (λ (g s sig c)
                 (when (eq? (signal-kind sig) 'activate)
                   (emit! c (make-event 'Activated `((key . ,(signal-key sig)))
                                        #:at (signal-time sig) #:src 'activation)))
                 s)))

(provide
 (contract-out
  [state-context-operator (->* ((-> event? any/c)) (#:key (or/c #f (-> event? any/c))) operator?)]
  [struct directory-msg ([name any/c] [entries hash?] [mode (or/c 'replace 'patch)])]
  [directory-push (->* (any/c (or/c hash? list?)) (#:mode (or/c 'replace 'patch) #:at (or/c #f time?))
                      step:control?)]
  [entity-context-operator (-> any/c #:key (-> event? any/c) operator?)]
  [activations-operator operator?]))

;; A declared directory: a name and a loader for its entries, a list of
;; (entity . status) with status active, inactive (or deprecated), or removed.
(struct directory-decl (name loader) #:transparent)

(define (normalize-status v)
  (case (if (string? v) (string->symbol v) v)
    [(active) 'active]
    [(inactive deprecated) 'inactive]
    [(removed deleted) 'removed]
    [else (raise-arguments-error 'directory "unknown entity status" "status" v)]))

(define (directory-entries d)
  (for/list ([p (in-list ((directory-decl-loader d)))]) (cons (car p) (normalize-status (cdr p)))))

(provide
 (contract-out
  [struct directory-decl ([name any/c] [loader (-> list?)])]
  [normalize-status (-> (or/c symbol? string?) (or/c 'active 'inactive 'removed))]
  [directory-entries (-> directory-decl? list?)]))
