#lang racket/base
(require rackunit
         racket/list
         "util.rkt"
         "../private/event.rkt"
         "../private/dataflow.rkt"
         "../private/pattern/ast.rkt"
         "../private/pattern/reference.rkt"
         "../private/pattern/operators.rkt")

(define (ev id type t k) (make-event type `((k . ,k)) #:id id #:at t))

(spec-test '(ES.keyed PT.sequence) "the reference operator evaluates a pattern per key at the end of the stream"
  (define p (seq (prim 'A #:bind 'a) (prim 'B #:bind 'b)))
  (define op (reference-pattern-operator p #:key (λ (e) (event-attr e 'k))))
  (define r (run-operator op (list (list (ev "a1" 'A 1 "x") (ev "a2" 'A 2 "y") (ev "b1" 'B 3 "x") (ev "b2" 'B 4 "y")))))
  (define out (run-result-events r))
  (check-equal? (map (λ (e) (map event-id (event-constituents e))) out) '(("a1" "b1") ("a2" "b2")))
  (check-equal? (event-attr (car out) 'a) "a1")
  (check-equal? (event-type (car out)) 'Match))

(spec-test '(PT.activation ES.space) "a match without events derives a summarized event at the deadline"
  (define op (reference-pattern-operator (absence act (prim 'X) 600) #:key (λ (e) 'dev)))
  (define r (run-operator op (list (list (ev "x" 'Y 5 "d")))))
  (define e (car (run-result-events r)))
  (check-equal? (list (event-ts e) (event-te e)) '(5 605))
  (check-true (event-has-attr? e 'summary)))

(test-case "ill-formed patterns are rejected"
  (check-exn #rx"nullable" (λ () (reference-pattern-operator (neg-between act (prim 'X) (prim 'Y))))))
