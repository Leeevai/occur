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

(require "../private/pattern/nfa.rkt" "pattern-gen.rkt" rackcheck)

(define (operator-matches op events)
  (sort (map (λ (e) (sort (map event-id (event-constituents e)) string<?)) (run-result-events (run-operator op (list events))))
        (λ (a b) (string<? (format "~a" a) (format "~a" b)))))

(spec-test '(ES.keyed SEL.any) "the incremental operator agrees with the reference operator per key"
  (check-property
   (make-config #:tests (trials))
   (property ([seed (gen:integer-in 0 1000000)] [h gen:point-history])
     (define p (random-pattern seed))
     (define keyed (for/list ([e h] [i (in-naturals)]) (event-set-attr e 'k (if (even? i) "x" "y"))))
     (define κ (λ (e) (event-attr e 'k)))
     (check-equal? (operator-matches (incremental-pattern-operator p #:key κ) keyed)
                   (operator-matches (reference-pattern-operator p #:key κ) keyed)))))

(spec-test '(PT.silence TM.idle) "a hybrid absence fires on the processing clock with no data at all"
  (define op (incremental-pattern-operator (absence act (prim 'X) 600 #:clock 'hybrid) #:key (λ (e) 'dev)))
  (define src (make-source 'in))
  (define n (make-node 'p op (list src) #:sink? #t))
  ;; a non-X event activates the key at 100; nothing else arrives
  (define r (run (make-program (list n)) (list (arrive 100 'in (ev "y" 'Y 100 "d")) (advance-to 2000))
                 #:final-watermark? #f))
  (check-equal? (map output-at (run-result-outputs r)) '(700)))
