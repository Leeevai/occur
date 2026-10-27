#lang racket/base
(require rackunit
         "util.rkt"
         "../private/event.rkt"
         "../private/dataflow.rkt"
         "../private/ops/stateless.rkt")

(define (ev id type t) (make-event type '() #:id id #:at t))

(spec-test 'OP.filter "selecting by type keeps the identical events of the chosen types"
  (define in (list (ev "a" 'Swipe 1) (ev "b" 'Verified 2) (ev "c" 'Swipe 3)))
  (define out (run-result-events (run-operator (select-types-operator '(Swipe)) (list in))))
  (check-equal? (map event-id out) '("a" "c"))
  (check-eq? (car out) (car in)))

(define (swipe id t amount) (make-event 'Swipe `((card . "c1") (amount . ,amount)) #:id id #:at t))
(define (run1 op events) (run-result-events (run-operator op (list events))))

(spec-test 'OP.filter "σ_p keeps exactly the events satisfying p, unchanged"
  (define in (list (swipe "a" 1 2) (swipe "b" 2 1500) (swipe "c" 3 999)))
  (define out (run1 (filter-operator (λ (e) (> (event-attr e 'amount) 1000))) in))
  (check-equal? (map event-id out) '("b"))
  (check-eq? (car out) (cadr in))
  (check-true (event-primitive? (car out))))

(spec-test 'OP.filter "derive-from builds C = {e} and inherits the interval and multiplicity"
  (define e (make-event 'R '((v . 1)) #:id "r" #:ts 2 #:te 5 #:n 4))
  (define d (derive-from e 'R2 '((w . 2)) #:src 'test))
  (check-equal? (map event-id (event-constituents d)) '("r"))
  (check-equal? (list (event-ts d) (event-te d) (event-n d) (event-src d)) '(2 5 4 test)))

(spec-test 'OP.project "π_A keeps only the attributes A and records C = {e}"
  (define e (make-event 'Swipe '((card . "c1") (amount . 5) (merchant . "m")) #:id "s" #:ts 1 #:te 2 #:n 3))
  (define out (run1 (project-operator '(card amount)) (list e)))
  (check-equal? (length out) 1)
  (define p (car out))
  (check-equal? (event-payload p) (hasheq 'card "c1" 'amount 5))
  (check-equal? (event-type p) 'Swipe)
  (check-equal? (map event-id (event-constituents p)) '("s"))
  (check-equal? (list (event-ts p) (event-te p) (event-n p)) '(1 2 3))
  (check-equal? (event-type (car (run1 (project-operator '(card) #:type 'CardOnly) (list e)))) 'CardOnly)
  (check-exn #rx"no such attribute" (λ () (run1 (project-operator '(zip)) (list e)))))
