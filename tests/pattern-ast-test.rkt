#lang racket/base
(require rackunit
         "util.rkt"
         "../private/pattern/ast.rkt")

(define a (prim 'Swipe #:bind 'a #:when (λ (e) #t)))
(define b (prim 'Swipe #:bind 'b))
(define v (prim 'Verified))

(test-case "the AST mirrors the grammar of §11.2"
  (define card-testing (within (where (neg-between a v (kleene+ b)) (λ (bs) #t)) 600))
  (check-true (p:within? card-testing))
  (check-equal? (pattern-bindings card-testing) '((a . single) (b . list)))
  (check-equal? (map p:primitive-type (pattern-leaves card-testing)) '(Swipe Verified Swipe))
  (check-false (monotone? card-testing))
  (check-true (monotone? (seq a (kleene+ b))))
  (check-false (unbounded? card-testing))
  (check-true (unbounded? (seq a (kleene+ b))))
  (check-false (eq? (p:primitive-id a) (p:primitive-id b))))

(test-case "nullability and well-formedness"
  (check-true (nullable? act))
  (check-true (nullable? (kleene* b)))
  (check-false (nullable? (kleene+ b)))
  (check-true (nullable? (repeat b 0 3)))
  (check-false (nullable? (any-of 1 a b)))
  (check-equal? (pattern-problem (neg-between a v (kleene* b))) "the neighbours of a negation must not be nullable")
  (check-equal? (pattern-problem (kleene+ (kleene* b))) "a repeated pattern must not be nullable")
  (check-equal? (pattern-problem (repeat b 3 2)) "a repetition's minimum exceeds its maximum")
  (check-equal? (pattern-problem (any-of 3 a b)) "ANY(k; …) needs 1 <= k <= the number of patterns")
  (check-false (pattern-problem (seq act (absence a v 10)))))

(test-case "cumulative aperiodic bodies bind lists"
  (check-equal? (pattern-bindings (aperiodic* a b (prim 'End #:bind 'c))) '((a . single) (b . list) (c . single)))
  (check-equal? (pattern-bindings (aperiodic a b (prim 'End #:bind 'c))) '((a . single) (b . single))))
