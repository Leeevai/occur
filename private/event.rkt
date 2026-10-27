#lang racket/base
;; The event space (spec §2): types and schemas, event instances, batch
;; events and shares, reference payloads, composite events, and the inductive
;; definition of the space with abstraction levels.
(require racket/contract
         racket/list
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
