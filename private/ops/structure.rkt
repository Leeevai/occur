#lang racket/base
;; Stream-structure operations (spec §5).
(require racket/contract
         racket/list
         "../time.rkt"
         "../event.rkt"
         "../operator.rkt")

;; ---------------------------------------------------------------------------
;; ST.merge: S₁ ∪ … ∪ Sₙ, interleaved by arrival. The events pass unchanged.

(define (merge-operator n)
  (make-operator 'merge #:arity n))

;; ---------------------------------------------------------------------------
;; ST.key-by: partition by κ. The node passes events through unchanged; the
;; runner keeps one partition per key, and downstream stateful operators are
;; built with the same κ. Re-keying by another attribute is the same
;; operation with a different κ.

(define (key-by-operator κ)
  (make-operator 'key-by #:key κ))

(define (attribute-key-fn attr) (λ (e) (event-attr e attr)))

(provide
 (contract-out
  [merge-operator (-> exact-positive-integer? operator?)]
  [key-by-operator (-> (-> event? any/c) operator?)]
  [attribute-key-fn (-> symbol? (-> event? any/c))]))
