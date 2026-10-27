#lang racket/base
;; The event space (spec §2): types and schemas, event instances, batch
;; events and shares, reference payloads, composite events, and the inductive
;; definition of the space with abstraction levels.
(require racket/contract
         racket/list
         racket/format
         "time.rkt")

;; ===========================================================================
;; ES.type: event types and schemas
;;
;; A schema is an ordered list of (attribute . domain) pairs with distinct
;; attribute names. A domain is one of the base domain symbols, a record
;; domain, or a list domain. A type is primitive (produced by sources
;; outside the engine) or derived (produced by an operator).

(struct location (x y) #:transparent)
(struct record-domain (fields) #:transparent)   ; fields: (listof (cons symbol domain))
(struct list-domain (element) #:transparent)

(define base-domains '(Integer Real String Boolean Time Duration Location Any))

(define (domain? d)
  (cond [(symbol? d) (and (memq d base-domains) #t)]
        [(record-domain? d) (fields? (record-domain-fields d))]
        [(list-domain? d) (domain? (list-domain-element d))]
        [else #f]))

(define (fields? fs)
  (and (list? fs)
       (andmap (λ (f) (and (pair? f) (symbol? (car f)) (domain? (cdr f)))) fs)
       (not (check-duplicates (map car fs)))))

(define (domain-value? d v)
  (cond
    [(eq? d 'Integer) (exact-integer? v)]
    [(eq? d 'Real) (real? v)]
    [(eq? d 'String) (string? v)]
    [(eq? d 'Boolean) (boolean? v)]
    [(eq? d 'Time) (time? v)]
    [(eq? d 'Duration) (duration? v)]
    [(eq? d 'Location) (location? v)]
    [(eq? d 'Any) #t]
    [(record-domain? d)
     (and (hash? v)
          (for/and ([f (in-list (record-domain-fields d))])
            (and (hash-has-key? v (car f)) (domain-value? (cdr f) (hash-ref v (car f))))))]
    [(list-domain? d)
     (and (list? v) (andmap (λ (x) (domain-value? (list-domain-element d) x)) v))]
    [else #f]))

(define (domain->string d)
  (cond [(symbol? d) (symbol->string d)]
        [(record-domain? d)
         (format "(Record ~a)"
                 (apply string-append
                        (add-between (for/list ([f (record-domain-fields d)])
                                       (format "[~a : ~a]" (car f) (domain->string (cdr f))))
                                     " ")))]
        [(list-domain? d) (format "(Listof ~a)" (domain->string (list-domain-element d)))]))

(struct etype (name fields kind) #:transparent)

(define (event-type? v) (etype? v))
(define (event-type-name t) (etype-name t))
(define (event-type-fields t) (etype-fields t))
(define (event-type-kind t) (etype-kind t))
(define (event-type-attributes t) (map car (etype-fields t)))
(define (event-type-primitive? t) (eq? (etype-kind t) 'primitive))
(define (event-type-derived? t) (eq? (etype-kind t) 'derived))

(define (make-event-type name fields #:derived? [derived? #f])
  (define dup (check-duplicates (map car fields)))
  (when dup
    (raise-arguments-error 'make-event-type "duplicate attribute name in schema"
                           "type" name "attribute" dup))
  (for ([f (in-list fields)])
    (unless (domain? (cdr f))
      (raise-arguments-error 'make-event-type "unknown value domain"
                             "type" name "attribute" (car f) "domain" (cdr f))))
  (etype name fields (if derived? 'derived 'primitive)))

(define (event-type-domain t attr)
  (define f (assq attr (etype-fields t)))
  (and f (cdr f)))

;; payload-problem : event-type payload -> (or/c #f string?)
;; Returns #f when the payload v is in D_τ, else a description of the problem.
(define (payload-problem t v)
  (cond
    [(not (hash? v)) "payload is not a hash"]
    [else
     (or (for/or ([f (in-list (etype-fields t))])
           (cond [(not (hash-has-key? v (car f)))
                  (format "missing attribute ~a" (car f))]
                 [(not (domain-value? (cdr f) (hash-ref v (car f))))
                  (format "attribute ~a: ~e is not a ~a" (car f) (hash-ref v (car f))
                          (domain->string (cdr f)))]
                 [else #f]))
         (for/or ([k (in-hash-keys v)])
           (and (not (assq k (etype-fields t)))
                (format "unknown attribute ~a" k))))]))

(define (payload-valid? t v) (not (payload-problem t v)))

;; A type registry is the finite set T of type names with their schemas.
(define (make-type-registry types)
  (for/hasheq ([t (in-list types)]) (values (etype-name t) t)))
(define (type-registry? v) (and (hash? v) (for/and ([t (in-hash-values v)]) (etype? t))))

(provide
 base-domains
 (contract-out
  [struct location ([x real?] [y real?])]
  [struct record-domain ([fields (listof (cons/c symbol? any/c))])]
  [struct list-domain ([element any/c])]
  [domain? (-> any/c boolean?)]
  [domain-value? (-> domain? any/c boolean?)]
  [domain->string (-> domain? string?)]
  [make-event-type (->* (symbol? (listof (cons/c symbol? any/c))) (#:derived? boolean?) event-type?)]
  [event-type? (-> any/c boolean?)]
  [event-type-name (-> event-type? symbol?)]
  [event-type-fields (-> event-type? (listof (cons/c symbol? domain?)))]
  [event-type-kind (-> event-type? (or/c 'primitive 'derived))]
  [event-type-attributes (-> event-type? (listof symbol?))]
  [event-type-primitive? (-> event-type? boolean?)]
  [event-type-derived? (-> event-type? boolean?)]
  [event-type-domain (-> event-type? symbol? (or/c domain? #f))]
  [payload-problem (-> event-type? any/c (or/c #f string?))]
  [payload-valid? (-> event-type? any/c boolean?)]
  [make-type-registry (-> (listof event-type?) type-registry?)]
  [type-registry? (-> any/c boolean?)]))

;; ===========================================================================
;; ES.event: event instances  e = ⟨id, τ, v, ts, te, n, src, C⟩
;;
;; `type` is the type name τ, `payload` the immutable hash v, [ts, te] the
;; occurrence interval in event time, `n` the multiplicity, `src` the
;; producer (a source name or an operator name), and `constituents` the list
;; C of events this one was derived from (empty for primitive events).
;; Arrival time arr(e) is deliberately not a field: it belongs to the runner.

(struct event (id type payload ts te n src constituents)
  #:transparent
  #:guard (λ (id type payload ts te n src cs name)
            (unless (and (time? ts) (time? te) (<= ts te))
              (raise-arguments-error name "event interval must satisfy ts <= te"
                                     "id" id "ts" ts "te" te))
            (unless (exact-nonnegative-integer? n)
              (raise-arguments-error name "multiplicity must be a natural number" "id" id "n" n))
            (unless (and (hash? payload) (immutable? payload))
              (raise-arguments-error name "payload must be an immutable hash" "id" id))
            (values id type payload ts te n src cs))
  #:methods gen:custom-write
  [(define (write-proc e port mode)
     (fprintf port "#<event ~a ~a [~a, ~a]~a~a>"
              (event-id e) (event-type e)
              (time->string (event-ts e)) (time->string (event-te e))
              (if (= (event-n e) 1) "" (format " n=~a" (event-n e)))
              (if (zero? (hash-count (event-payload e)))
                  ""
                  (apply string-append
                         (for/list ([(k v) (in-hash (event-payload e))])
                           (format " ~a=~v" k v))))))])

;; Deterministic identifiers. Every id is a string made of a prefix and a
;; counter; the runner installs its own counter so that runs are
;; reproducible (docs/decisions/0002-deterministic-ids.md).
(define current-id-source (make-parameter (box 0)))
(define current-id-prefix (make-parameter "e"))

(define (fresh-id [prefix (current-id-prefix)])
  (define b (current-id-source))
  (set-box! b (add1 (unbox b)))
  (string-append prefix (number->string (unbox b))))

(define (call-with-fresh-ids thunk #:start [start 0])
  (parameterize ([current-id-source (box start)]) (thunk)))

(define (->payload v)
  (cond [(and (hash? v) (immutable? v) (hash-eq? v)) v]
        [(hash? v) (for/hasheq ([(k x) (in-hash v)]) (values k x))]
        [(list? v) (for/hasheq ([p (in-list v)]) (values (car p) (cdr p)))]
        [else (raise-argument-error 'make-event "(or/c hash? (listof pair?))" v)]))

(define (make-event type payload
                    #:id [id #f]
                    #:at [at #f]
                    #:ts [ts at]
                    #:te [te ts]
                    #:n [n 1]
                    #:src [src #f]
                    #:constituents [cs '()])
  (unless ts
    (raise-arguments-error 'make-event "an event needs a time: give #:at or #:ts" "type" type))
  (event (or id (fresh-id)) type (->payload payload) ts te n src cs))

(define (event-attr e a)
  (hash-ref (event-payload e) a
            (λ () (raise-arguments-error 'event-attr "event has no such attribute"
                                         "attribute" a "event" e))))
(define (event-attr/default e a default) (hash-ref (event-payload e) a default))
(define (event-has-attr? e a) (hash-has-key? (event-payload e) a))
(define (event-point? e) (= (event-ts e) (event-te e)))
(define (event-t e)
  (unless (event-point? e)
    (raise-arguments-error 'event-t "t(e) is defined only for point events" "event" e))
  (event-ts e))
(define (event-interval e) (interval (event-ts e) (event-te e)))
(define (event-primitive? e) (null? (event-constituents e)))

(define (event-update e
                      #:id [id (event-id e)]
                      #:type [type (event-type e)]
                      #:payload [payload (event-payload e)]
                      #:ts [ts (event-ts e)]
                      #:te [te (event-te e)]
                      #:n [n (event-n e)]
                      #:src [src (event-src e)]
                      #:constituents [cs (event-constituents e)])
  (event id type (->payload payload) ts te n src cs))

(define (event-set-attr e a v)
  (event-update e #:payload (hash-set (event-payload e) a v)))

(provide
 (contract-out
  [struct event ([id any/c] [type symbol?] [payload (and/c hash? immutable?)]
                 [ts time?] [te time?] [n exact-nonnegative-integer?]
                 [src any/c] [constituents (listof event?)])]
  [current-id-source (parameter/c box?)]
  [current-id-prefix (parameter/c string?)]
  [fresh-id (->* () (string?) string?)]
  [call-with-fresh-ids (->* ((-> any)) (#:start exact-nonnegative-integer?) any)]
  [make-event (->* (symbol? (or/c hash? (listof pair?)))
                   (#:id any/c #:at (or/c #f time?) #:ts (or/c #f time?) #:te (or/c #f time?)
                    #:n exact-nonnegative-integer? #:src any/c #:constituents (listof event?))
                   event?)]
  [event-attr (-> event? symbol? any/c)]
  [event-attr/default (-> event? symbol? any/c any/c)]
  [event-has-attr? (-> event? symbol? boolean?)]
  [event-point? (-> event? boolean?)]
  [event-t (-> event? time?)]
  [event-interval (-> event? interval?)]
  [event-primitive? (-> event? boolean?)]
  [event-update (->* (event?)
                     (#:id any/c #:type symbol? #:payload (or/c hash? (listof pair?))
                      #:ts time? #:te time? #:n exact-nonnegative-integer? #:src any/c
                      #:constituents (listof event?))
                     event?)]
  [event-set-attr (-> event? symbol? any/c event?)]))

;; ===========================================================================
;; ES.batch: batch events and shares
;;
;; A batch event stands for n(e) >= 0 samples spread evenly over [ts, te].
;; share(e, b) is the number of those samples lying strictly past boundary b:
;;
;;   0                                                   if n = 0 or te <= b
;;   n                                                   if ts > b
;;   min(n, max(1, round(n · (te − b) / (te − ts))))     otherwise
;;
;; where round is round-half-to-even (Racket's `round`). In the last case
;; ts <= b < te, so te − ts > 0.

(define (make-batch-event type payload #:ts ts #:te te #:n n
                          #:id [id #f] #:src [src #f])
  (make-event type payload #:id id #:ts ts #:te te #:n n #:src src))

(define (batch-share e b)
  (define n (event-n e))
  (define ts (event-ts e))
  (define te (event-te e))
  (cond [(or (zero? n) (<= te b)) 0]
        [(> ts b) n]
        [else (min n (max 1 (inexact->exact (round (* n (/ (- te b) (- te ts)))))))]))

;; The interval of the share past b: [max(ts, b), te].
(define (batch-share-interval e b)
  (interval (time-max (event-ts e) b) (event-te e)))

(provide
 (contract-out
  [make-batch-event (->* (symbol? (or/c hash? (listof pair?))
                                  #:ts time? #:te time? #:n exact-nonnegative-integer?)
                         (#:id any/c #:src any/c)
                         event?)]
  [batch-share (-> event? time? exact-nonnegative-integer?)]
  [batch-share-interval (-> event? time? interval?)]))

;; ===========================================================================
;; ES.reference: reference payloads
;;
;; Instead of the data, an event may carry a pointer (a data-ref) naming a
;; sub-stream and an interval, plus a small summary such as the newest
;; value. Counting, windowing, and timing need only (ts, te, n) and the
;; summary; a consumer that needs the values reads them from a store.

(struct data-ref (substream interval summary) #:transparent)

(define (make-reference-event type
                              #:substream substream
                              #:ts ts #:te [te ts] #:n [n 1]
                              #:summary [summary (hasheq)]
                              #:payload [extra '()]
                              #:id [id #f] #:src [src #f])
  (make-event type
              (hash-set (->payload extra) 'ref (data-ref substream (interval ts te) (->payload summary)))
              #:id id #:ts ts #:te te #:n n #:src src))

(define (event-reference e)
  (define r (hash-ref (event-payload e) 'ref #f))
  (and (data-ref? r) r))
(define (reference-event? e) (and (event-reference e) #t))

;; A store holds, per sub-stream, samples (time . value) in time order.
(struct store (samples) #:transparent)
(define empty-store (store (hash)))

(define (store-append st substream samples)
  (define old (hash-ref (store-samples st) substream '()))
  (store (hash-set (store-samples st) substream
                   (sort (append old samples) < #:key car))))

(define (store-read st ref)
  (define iv (data-ref-interval ref))
  (for/list ([s (in-list (hash-ref (store-samples st) (data-ref-substream ref) '()))]
             #:when (interval-contains? iv (car s)))
    s))

(provide
 empty-store
 (contract-out
  [struct data-ref ([substream any/c] [interval interval?] [summary hash?])]
  [make-reference-event (->* (symbol? #:substream any/c #:ts time?)
                             (#:te time? #:n exact-nonnegative-integer? #:summary (or/c hash? list?)
                              #:payload (or/c hash? list?) #:id any/c #:src any/c)
                             event?)]
  [event-reference (-> event? (or/c data-ref? #f))]
  [reference-event? (-> event? boolean?)]
  [store? (-> any/c boolean?)]
  [store-append (-> store? any/c (listof (cons/c time? any/c)) store?)]
  [store-read (-> store? data-ref? (listof (cons/c time? any/c)))]))
