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
