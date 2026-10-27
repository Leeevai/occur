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
