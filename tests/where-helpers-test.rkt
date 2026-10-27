#lang racket/base
(require rackunit
         "util.rkt"
         "../private/event.rkt"
         "../private/pattern/where-helpers.rkt")

(define bs (list (make-event 'B '((amount . 600)) #:at 60 #:n 2) (make-event 'B '((amount . 900)) #:ts 120 #:te 180)))

(spec-test 'PT.where "where clauses read like the spec: count, sum, span, increasing over Kleene lists"
  (check-equal? (with-where-helpers here (count bs)) 3)            ; the multiplicity sum
  (check-equal? (with-where-helpers here (count '(1 2 3))) 3)
  (check-equal? (with-where-helpers here (sum '(600 900))) 1500)
  (check-equal? (with-where-helpers here (span bs)) 120)
  (check-true (with-where-helpers here (increasing '(600 900))))
  (check-equal? (with-where-helpers here (list (max '(1 5 2)) (max 1 5 2) (min '(4 3)) (avg '(1 2)))) '(5 5 3 3/2)))
