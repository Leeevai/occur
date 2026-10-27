#lang racket/base
;; Pattern policies (spec §12): selection strategies, consumption, output,
;; skipping, and evaluation timing.
(require racket/contract
         racket/list
         "../time.rkt"
         "../event.rkt"
         "ast.rkt"
         "reference.rkt"
         "nfa.rkt")

;; ===========================================================================
;; §12.1 Selection strategies (decision 0012)
;;
;; A strategy decides which events may be skipped between consecutive steps.
;; Every event in a match records the leaf it was taken for and the end
;; point (te . index) of the step before it, or none at the start of the
;; pattern. A strategy is a condition on each event that follows a step:
;;   SEL.strict     its global arrival index is the next one after the step
;;   SEL.partition  its index within the key's partition is the next one
;;   SEL.next       no event qualifying for the same leaf (type, predicate,
;;                  and ts > te(step)) arrived between the step and it
;;   SEL.any        no condition
;; After ACT (index −1), "next" means the instance's first event. On a single
;; key the match sets nest: strict ⊆ partition ⊆ next ⊆ any.

(define (qualifies? leaf e)
  (and (or (not (p:primitive-type leaf)) (eq? (event-type e) (p:primitive-type leaf)))
       (or (not (p:primitive-pred leaf)) ((p:primitive-pred leaf) e))))

(define (pred-pindex m pred index pindex)
  (define gi (cdr pred))
  (if (< gi 0)
      -1
      (for/first ([e (in-list (pmatch-events m))] #:when (= (hash-ref index (event-id e)) gi))
        (hash-ref pindex (event-id e)))))

(define ((strategy-check strategy) m index pindex qual-of)
  (for/and ([u (in-list (pmatch-leaves m))] #:when (leaf-use-pred u))
    (define e (leaf-use-event u))
    (define pred (leaf-use-pred u))
    (define gi (hash-ref index (event-id e)))
    (case strategy
      [(strict) (if (< (cdr pred) 0)
                    (= (hash-ref pindex (event-id e)) 0)
                    (= gi (add1 (cdr pred))))]
      [(partition) (= (hash-ref pindex (event-id e)) (add1 (pred-pindex m pred index pindex)))]
      [(next) (not (for/or ([e2 (in-list (qual-of (leaf-use-leaf u)))])
                     (define g2 (hash-ref index (event-id e2) #f))
                     (and g2 (< (cdr pred) g2 gi) (> (event-ts e2) (car pred)))))]
      [else #t])))

(for ([s '(strict partition next)]) (register-selection-check! s (strategy-check s)))

;; The oracle's version: filter ⟦P⟧(H) by a strategy, with H's order as the
;; arrival order (one key, so global and partition indices coincide).
(define (select-matches ms strategy history)
  (define index (for/hash ([e (in-list history)] [i (in-naturals)]) (values (event-id e) i)))
  (define check (strategy-check strategy))
  (filter (λ (m) (check m index index (λ (leaf) (filter (λ (e) (qualifies? leaf e)) history)))) ms))

(define selection-strategies '(strict partition next any))

(provide
 selection-strategies
 (contract-out
  [qualifies? (-> p:primitive? event? boolean?)]
  [select-matches (-> (listof pmatch?) (or/c 'strict 'partition 'next 'any) (listof event?) (listof pmatch?))]))
