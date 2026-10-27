#lang racket/base
;; The reference oracle transcribes §11.3; each operator is tested on small
;; histories whose matches can be checked by hand.
(require rackunit
         racket/list
         "util.rkt"
         "../private/event.rkt"
         "../private/pattern/ast.rkt"
         "../private/pattern/reference.rkt")

(define (ev id type t [payload '()] #:te [te t]) (make-event type payload #:id id #:ts t #:te te))
(define (ids ms) (map pmatch-ids ms))
;; the matches as a set of id lists, for comparisons where order is not the point
(define (id-set ms) (sort (map pmatch-ids ms) (λ (a b) (string<? (format "~a" a) (format "~a" b)))))
(define (set-of . xs) (sort xs (λ (a b) (string<? (format "~a" a) (format "~a" b)))))
(define A (prim 'A #:bind 'a))
(define B (prim 'B #:bind 'b))
(define C (prim 'C #:bind 'c))

(define H (list (ev "a1" 'A 1) (ev "b1" 'B 2) (ev "a2" 'A 3) (ev "b2" 'B 4) (ev "c1" 'C 5)))

(spec-test 'PT.primitive "⟦x:τ[p]⟧ is every event of type τ satisfying p, bound to x"
  (check-equal? (ids (denote A H)) '(("a1") ("a2")))
  (check-equal? (ids (denote (prim 'A #:when (λ (e) (> (event-ts e) 2))) H)) '(("a2")))
  (define m (car (denote A H)))
  (check-equal? (event-id (hash-ref (pmatch-bindings m) 'a)) "a1")
  (check-equal? (list (pmatch-ts m) (pmatch-te m)) '(1 1))
  (check-equal? (length (denote (prim #f) H)) 5))

(spec-test 'PT.activation "⟦ACT⟧ is the empty match at the activation instant"
  (define ms (denote act H #:activation 0))
  (check-equal? (ids ms) '(()))
  (check-equal? (list (pmatch-ts (car ms)) (pmatch-te (car ms))) '(0 0))
  (check-equal? (denote act H) '()))

(spec-test 'PT.sequence "⟦P₁ ; P₂⟧ pairs matches with te(m₁) < ts(m₂); equal times are not in sequence"
  (check-equal? (ids (denote (seq A B) H)) '(("a1" "b1") ("a1" "b2") ("a2" "b2")))
  (check-equal? (ids (denote (seq A B C) H)) '(("a1" "b1" "c1") ("a1" "b2" "c1") ("a2" "b2" "c1")))
  (check-equal? (denote (seq A B) (list (ev "a" 'A 5) (ev "b" 'B 5))) '())
  ;; interval events: te(m₁) < ts(m₂)
  (check-equal? (ids (denote (seq A B) (list (ev "a" 'A 1 #:te 4) (ev "b" 'B 3) (ev "b'" 'B 5))))
                '(("a" "b'")))
  (define m (car (denote (seq A B) H)))
  (check-equal? (list (pmatch-ts m) (pmatch-te m)) '(1 2))
  (check-equal? (sort (hash-keys (pmatch-bindings m)) symbol<?) '(a b)))

(spec-test 'PT.conjunction "⟦P₁ ∧ P₂⟧ unions disjoint matches in any order"
  (check-equal? (id-set (denote (conj A B) H)) (set-of '("a1" "b1") '("b1" "a2") '("a1" "b2") '("a2" "b2")))
  ;; the same type on both sides pairs distinct events, bound either way round
  (define both (denote (conj A (prim 'A #:bind 'x)) H))
  (check-equal? (ids both) '(("a1" "a2") ("a1" "a2")))
  (check-equal? (sort (map (λ (m) (event-id (hash-ref (pmatch-bindings m) 'a))) both) string<?) '("a1" "a2")))

(spec-test 'PT.disjunction "⟦P₁ ∨ P₂⟧ is the union; the other side's bindings are absent"
  (define ms (denote (disj A C) H))
  (check-equal? (ids ms) '(("a1") ("a2") ("c1")))
  (check-false (hash-has-key? (pmatch-bindings (last ms)) 'a)))

(spec-test 'PT.negation "the forbidden region of ¬Q is only the open gap between the neighbours"
  (define V (prim 'V))
  (define H2 (list (ev "a1" 'A 1) (ev "b1" 'B 2) (ev "v" 'V 3) (ev "b2" 'B 4)))
  (check-equal? (ids (denote (neg-between A V B) H2)) '(("a1" "b1")))
  ;; a Q at the gap's boundary does not block (the gap is open)
  (check-equal? (ids (denote (neg-between A V B) (list (ev "a" 'A 1) (ev "v" 'V 1) (ev "b" 'B 2))))
                '(("a" "b"))))

(spec-test 'PT.absence "⟦P ; ¬Q within Δ⟧ keeps the matches not followed by Q within Δ, ending at the deadline"
  (define X (prim 'X #:bind 'x))
  (define Hx (list (ev "x1" 'X 0) (ev "x2" 'X 5) (ev "x3" 'X 20)))
  (define ms (denote (absence X (prim 'X) 10) Hx))
  (check-equal? (ids ms) '(("x2") ("x3")))
  (check-equal? (map (λ (m) (list (pmatch-ts m) (pmatch-te m))) ms) '((5 15) (20 30)))
  ;; the window is closed on the right: Q exactly at te + Δ blocks
  (check-equal? (ids (denote (absence X (prim 'X) 5) (list (ev "x1" 'X 0) (ev "x2" 'X 5)))) '(("x2")))
  ;; P may be ACT: total silence since activation
  (check-equal? (map (λ (m) (pmatch-te m)) (denote (absence act (prim 'X) 600) '() #:activation 0)) '(600))
  (check-equal? (denote (absence act (prim 'X) 600) (list (ev "x" 'X 100)) #:activation 0) '()))

(spec-test 'PT.span-absence "unless forbids Q anywhere strictly inside the whole match"
  (define V (prim 'V))
  (define H2 (list (ev "a1" 'A 1) (ev "b1" 'B 2) (ev "v" 'V 3) (ev "b2" 'B 4)))
  (check-equal? (ids (denote (span-absence (seq A B) V) H2)) '(("a1" "b1")))
  ;; Q's own events inside the match do not count against it
  (check-equal? (ids (denote (span-absence (seq A (prim 'V #:bind 'v) B) V) H2)) '(("a1" "v" "b2"))))
