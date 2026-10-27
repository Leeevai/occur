#lang racket/base
(require rackunit
         rackcheck
         "util.rkt"
         "../private/event.rkt"
         "../private/relations.rkt")

(define (ev id ts te . payload) (make-event 'X payload #:id id #:ts ts #:te te))

(spec-test 'ES.precedes "a ≺ b iff te(a) < ts(b); otherwise the events are concurrent"
  (define a (ev "a" 0 2))
  (define b (ev "b" 3 4))
  (define c (ev "c" 2 5))
  (check-true (precedes? a b))
  (check-false (precedes? b a))
  (check-false (precedes? a c))  ; te(a) = ts(c) is not strict precedence
  (check-true (concurrent? a c))
  (check-false (concurrent? a b)))

(define gen:ev
  (gen:let ([ts (gen:integer-in 0 20)] [len (gen:integer-in 0 5)])
    (ev (fresh-id) ts (+ ts len))))

(spec-test 'ES.precedes "property: ≺ is irreflexive, asymmetric, and transitive"
  (check-property
   (make-config #:tests (trials))
   (property ([a gen:ev] [b gen:ev] [c gen:ev])
     (check-false (precedes? a a))
     (check-false (and (precedes? a b) (precedes? b a)))
     (when (and (precedes? a b) (precedes? b c))
       (check-true (precedes? a c))))))

(spec-test 'ES.membership "membership and its transitive closure form the event hierarchy"
  (define e1 (ev "e1" 1 1))
  (define e2 (ev "e2" 2 2))
  (define f (make-composite 'F '() (list e1 e2) #:id "f"))
  (define g (make-composite 'G '() (list f) #:id "g"))
  (check-true (constituent-of? e1 f))
  (check-false (constituent-of? e1 g))
  (check-true (member*? e1 g))
  (check-equal? (map event-id (provenance g)) '("f" "e1" "e2"))
  (check-false (member*? g e1)))

(spec-test 'ES.causality "recorded causes come from b.cause = a.id; closure is transitive"
  (define a (ev "a" 1 1))
  (define b (ev "b" 2 2 '(cause . "a")))
  (define c (ev "c" 5 5 '(cause . "b")))
  (define g (causality-from-events (list a b c)))
  (check-true (causes? g a b))
  (check-true (causes? g a c))
  (check-false (causes? g c a))
  (check-equal? (causality-problems g (list a b c)) '()))

(spec-test 'ES.causality "inferred causality, and violations of order and time are reported"
  (define a (ev "a" 1 1))
  (define b (ev "b" 2 2))
  (define g (causality-from-events (list a b) #:infer (λ (x y) (< (event-ts x) (event-ts y)))))
  (check-true (causes? g a b))
  ;; temporal order does not imply causality
  (check-false (causes? (causality-from-events (list a b)) a b))
  (define bad (make-causality '(("b" . "a"))))
  (check-equal? (causality-problems bad (list a b)) '("b → a but ts(b) > ts(a)"))
  (define cyc (make-causality '(("a" . "b") ("b" . "a"))))
  (check-equal? (length (filter (λ (s) (regexp-match? #rx"cycle" s)) (causality-problems cyc (list a b))))
                2))

(spec-test '(ES.precedes ES.membership ES.causality) "the event cloud collects ≺, →, and ∈"
  (define e1 (ev "e1" 1 1))
  (define e2 (ev "e2" 3 3 '(cause . "e1")))
  (define f (make-composite 'F '() (list e1 e2) #:id "f"))
  (define cloud (make-event-cloud (list e1 e2 f)))
  (check-equal? (event-cloud-precedence cloud) '(("e1" . "e2")))
  (check-equal? (event-cloud-membership cloud) '(("e1" . "f") ("e2" . "f")))
  (check-true (causes? (event-cloud-causality cloud) e1 e2)))
