#lang racket/base
;; Single-event operations (spec §4). Filter returns the events it receives;
;; every other operation derives events whose constituent set is C = {e}.
(require racket/contract
         "../event.rkt"
         "../operator.rkt")

;; Keep only events of the given types (a type-based OP.filter).
(define (select-types-operator types)
  (stateless-operator 'select-types
                      (λ (e c) (when (memq (event-type e) types) (emit! c e)))))

(provide
 (contract-out
  [select-types-operator (-> (listof symbol?) operator?)]))
