#lang racket/base
;; Single-event operations (spec §4). Filter returns the events it receives;
;; every other operation derives events whose constituent set is C = {e}.
(require racket/contract
         "../time.rkt"
         "../event.rkt"
         "../operator.rkt")

;; Keep only events of the given types (a type-based OP.filter).
(define (select-types-operator types)
  (stateless-operator 'select-types
                      (λ (e c) (when (memq (event-type e) types) (emit! c e)))))

(provide
 (contract-out
  [select-types-operator (-> (listof symbol?) operator?)]))

;; ---------------------------------------------------------------------------
;; OP.filter: σ_p keeps e if and only if p(e) holds. The events are passed
;; through unchanged (same id, no new provenance).

(define (filter-operator pred #:name [name 'filter])
  (stateless-operator name (λ (e c) (when (pred e) (emit! c e)))))

;; A derived event whose constituent set is C = {e}: by default it inherits
;; e's interval and multiplicity.
(define (derive-from e type payload #:src src
                     #:ts [ts (event-ts e)] #:te [te (event-te e)] #:n [n (event-n e)])
  (make-event type payload #:ts ts #:te te #:n n #:src src #:constituents (list e)))

(provide
 (contract-out
  [filter-operator (->* ((-> event? any/c)) (#:name symbol?) operator?)]
  [derive-from (->* (event? symbol? (or/c hash? (listof pair?)) #:src any/c)
                    (#:ts time? #:te time? #:n exact-nonnegative-integer?)
                    event?)]))

;; ---------------------------------------------------------------------------
;; OP.project: π_A restricts the payload to the attributes A ⊆ schema(τ),
;; giving the type τ restricted to A (named #:type, or τ itself).

(define (project-operator attrs #:type [type #f])
  (stateless-operator
   'project
   (λ (e c)
     (for ([a (in-list attrs)])
       (unless (event-has-attr? e a)
         (raise-arguments-error 'project "the event has no such attribute" "attribute" a "event" e)))
     (emit! c (derive-from e (or type (event-type e))
                           (for/hasheq ([a (in-list attrs)]) (values a (event-attr e a)))
                           #:src 'project)))))

(provide
 (contract-out
  [project-operator (->* ((listof symbol?)) (#:type (or/c #f symbol?)) operator?)]))

;; ---------------------------------------------------------------------------
;; OP.map: μ_f translates e ↦ f(e), possibly into a new type. f returns the
;; new payload; timestamps and multiplicity are inherited unless set.

(define (map-operator f #:type [type #f] #:ts [tsf #f] #:te [tef #f] #:n [nf #f])
  (stateless-operator
   'map
   (λ (e c)
     (define ts (if tsf (tsf e) (event-ts e)))
     (define te (if tef (tef e) (if tsf ts (event-te e))))
     (emit! c (derive-from e (or type (event-type e)) (f e)
                           #:ts ts #:te te #:n (if nf (nf e) (event-n e)) #:src 'map)))))

(provide
 (contract-out
  [map-operator (->* ((-> event? (or/c hash? (listof pair?))))
                     (#:type (or/c #f symbol?)
                      #:ts (or/c #f (-> event? time?))
                      #:te (or/c #f (-> event? time?))
                      #:n (or/c #f (-> event? exact-nonnegative-integer?)))
                     operator?)]))
