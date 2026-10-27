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
