#lang racket/base
(require rackunit
         "util.rkt"
         "../private/event.rkt")

(define e1 (make-event 'Swipe '((amount . 2)) #:id "e1" #:at 1))
(define e2 (make-event 'Swipe '((amount . 600)) #:id "e2" #:ts 2 #:te 3))
(define e4 (make-event 'Swipe '((amount . 900)) #:id "e4" #:at 4 #:n 1))

(spec-test 'ES.composite "the default interval is the hull of the constituents and n = 1"
  (define c (make-composite 'SuspectedFraud '((n . 2)) (list e1 e2 e4) #:src 'card-testing))
  (check-equal? (event-ts c) 1)
  (check-equal? (event-te c) 4)
  (check-equal? (event-n c) 1)
  (check-equal? (map event-id (event-constituents c)) '("e1" "e2" "e4"))
  (check-false (event-primitive? c)))

(spec-test 'ES.composite "documented exceptions override the interval explicitly"
  ;; an absence detection ends at its deadline, not at its last constituent
  (define d (make-composite 'Silence '() (list e1) #:te 601))
  (check-equal? (list (event-ts d) (event-te d)) '(1 601))
  (check-exn #rx"at least one constituent" (λ () (make-composite 'X '() '()))))
