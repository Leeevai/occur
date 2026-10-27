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

(spec-test 'PT.kleene-plus "⟦B+⟧ is every time-ordered non-empty run, bound as a list"
  (check-equal? (ids (denote (kleene+ B) H)) '(("b1") ("b1" "b2") ("b2")))
  (define long (findf (λ (m) (= 2 (length (pmatch-events m)))) (denote (kleene+ B) H)))
  (check-equal? (map event-id (hash-ref (pmatch-bindings long) 'b)) '("b1" "b2")))

(spec-test 'PT.kleene-star "⟦B*⟧ adds the empty match, meaningful inside a sequence"
  (check-equal? (ids (denote (kleene* B) H)) '(("b1") ("b1" "b2") ("b2") ()))
  ;; A ; B* ; C includes A ; C, with b bound to the empty list
  (define ms (denote (seq A (kleene* B) C) H))
  (check-equal? (id-set ms) (set-of '("a1" "c1") '("a1" "b1" "c1") '("a1" "b1" "b2" "c1") '("a1" "b2" "c1")
                                    '("a2" "c1") '("a2" "b2" "c1")))
  (check-equal? (hash-ref (pmatch-bindings (findf (λ (m) (= 2 (length (pmatch-events m)))) ms)) 'b) '()))

(spec-test 'PT.repeat "⟦P{n,m}⟧ bounds the number of repetitions"
  (define Hb (for/list ([i 4]) (ev (format "b~a" i) 'B i)))
  ;; runs of 2 or 3 of four ordered events: C(4,2) + C(4,3)
  (check-equal? (sort (map length (map pmatch-events (denote (repeat B 2 3) Hb))) <) '(2 2 2 2 2 2 3 3 3 3))
  (check-equal? (length (denote (repeat B 3) Hb)) 4)
  (check-equal? (length (denote (repeat B 1 +inf.0) Hb)) 15))

(spec-test 'PT.any "ANY(k; …) takes k of the n patterns, pairwise disjoint"
  (define ms (denote (any-of 2 A B C) H))
  (check-equal? (length ms) (+ 4 2 2))         ; A∧B: 4, A∧C: 2, B∧C: 2
  (check-equal? (map pmatch-key (denote (any-of 1 A C) H)) (map pmatch-key (denote (disj A C) H)))
  (check-equal? (map pmatch-key (denote (any-of 2 A B) H)) (map pmatch-key (denote (conj A B) H))))

(spec-test 'PT.within "⟦P within Δ⟧ bounds the span of a match"
  (check-equal? (ids (denote (within (seq A B) 1) H)) '(("a1" "b1") ("a2" "b2")))
  (check-equal? (ids (denote (within (seq A B) 3) H)) '(("a1" "b1") ("a1" "b2") ("a2" "b2"))))

(spec-test 'PT.where "θ filters matches by their bindings, including Kleene lists"
  (define Hv (list (ev "a" 'A 1 '((card . "c1"))) (ev "b" 'B 2 '((card . "c1") (amount . 600)))
                   (ev "b'" 'B 3 '((card . "c2") (amount . 900)))))
  (define same-card (λ (bs) (equal? (event-attr (hash-ref bs 'a) 'card) (event-attr (hash-ref bs 'b) 'card))))
  (check-equal? (ids (denote (where (seq A B) same-card) Hv)) '(("a" "b")))
  (define big (λ (bs) (> (apply + (map (λ (b) (event-attr b 'amount)) (hash-ref bs 'b))) 1000)))
  (check-equal? (ids (denote (where (kleene+ B) big) Hv)) '(("b" "b'"))))

(spec-test '(PT.aperiodic PT.aperiodic-cumulative) "APER reports each event inside an open interval; APER* reports them all at the close"
  (define S (prim 'S #:bind 's)) (define X (prim 'X #:bind 'x)) (define E (prim 'E #:bind 'e))
  (define Hs (list (ev "x0" 'X 0) (ev "s1" 'S 1) (ev "x1" 'X 2) (ev "x2" 'X 3) (ev "e1" 'E 4) (ev "x3" 'X 5)
                   (ev "s2" 'S 6) (ev "x4" 'X 7)))
  (check-equal? (ids (denote (aperiodic S X E) Hs)) '(("s1" "x1") ("s1" "x2") ("s2" "x4")))
  (define cum (denote (aperiodic* S X E) Hs))
  (check-equal? (ids cum) '(("s1" "x1" "x2" "e1")))         ; s2 never closes
  (check-equal? (map event-id (hash-ref (pmatch-bindings (car cum)) 'x)) '("x1" "x2"))
  ;; a close with nothing inside still reports the interval
  (check-equal? (ids (denote (aperiodic* S X E) (list (ev "s" 'S 1) (ev "e" 'E 2)))) '(("s" "e"))))

(spec-test '(PT.periodic PT.periodic-cumulative) "PER ticks every Δ after the opener until the close; PER* reports the ticks at the close"
  (define S (prim 'S #:bind 's)) (define E (prim 'E #:bind 'e))
  (define Hp (list (ev "s" 'S 0) (ev "e" 'E 35)))
  (check-equal? (map pmatch-te (denote (periodic S 10 E) Hp)) '(10 20 30))
  (define cum (denote (periodic* S 10 E) Hp))
  (check-equal? (ids cum) '(("s" "e")))
  (check-equal? (hash-ref (pmatch-bindings (car cum)) 'ticks) '(10 20 30))
  ;; a tick at the close's start is not before it
  (check-equal? (map pmatch-te (denote (periodic S 10 E) (list (ev "s" 'S 0) (ev "e" 'E 30)))) '(10 20))
  ;; without a close, ticks run up to the horizon
  (check-equal? (map pmatch-te (denote (periodic S 10 E) (list (ev "s" 'S 0)) #:horizon 25)) '(10 20)))

;; ---------------------------------------------------------------------------
;; Spec §15 under the most permissive semantics (SEL.any, CON.unrestricted,
;; OUT.every): the oracle reproduces the any/every cells of traces A, A′, B.

(define (swipe id t amount) (ev id 'Swipe (* 60 t) `((card . "c1") (amount . ,amount))))
(define (verified id t) (ev id 'Verified (* 60 t) '((card . "c1"))))
(define (amounts bs) (map (λ (b) (event-attr b 'amount)) bs))
(define (increasing? xs) (or (null? xs) (null? (cdr xs)) (and (< (car xs) (cadr xs)) (increasing? (cdr xs)))))

(define (card-testing #:negation [mode 'between])
  (define a (prim 'Swipe #:bind 'a #:when (λ (e) (< (event-attr e 'amount) 5))))
  (define b (kleene+ (prim 'Swipe #:bind 'b #:when (λ (e) (> (event-attr e 'amount) 500)))))
  (define body (if (eq? mode 'between) (neg-between a (prim 'Verified) b) (seq a b)))
  (define p (within (where body (λ (bs) (increasing? (amounts (hash-ref bs 'b))))) (* 60 10)))
  (if (eq? mode 'span) (span-absence p (prim 'Verified)) p))

(define trace-a (list (swipe "e1" 1 2) (swipe "e2" 2 600) (swipe "e3" 3 50) (swipe "e4" 4 900)))

(spec-test '(PT.sequence PT.negation PT.kleene-plus PT.where PT.within)
           "trace A (§15): under skip-till-any-match every increasing subset of {e2, e4} follows e1"
  (check-equal? (id-set (denote (card-testing) trace-a))
                (set-of '("e1" "e2") '("e1" "e4") '("e1" "e2" "e4"))))

(spec-test '(PT.negation PT.span-absence) "trace A′ (§15): the scope of ¬Verified versus unless Verified"
  (define a-prime (append trace-a (list (verified "v" 3.5))))
  (check-equal? (id-set (denote (card-testing) a-prime)) (set-of '("e1" "e2") '("e1" "e2" "e4")))
  (check-equal? (id-set (denote (card-testing #:negation 'span) a-prime)) (set-of '("e1" "e2"))))

(spec-test 'PT.negation "trace B (§15): a verification between the swipes prevents every match"
  (define trace-b (list (ev "s1" 'Swipe 60 '((card . "c2") (amount . 1))) (ev "v" 'Verified 120 '((card . "c2")))
                        (ev "s2" 'Swipe 180 '((card . "c2") (amount . 800)))))
  (check-equal? (denote (card-testing) trace-b) '()))
