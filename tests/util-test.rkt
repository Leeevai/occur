#lang racket/base
(require rackunit "util.rkt")

(test-case "spec-ids->string renders one or several IDs"
  (check-equal? (spec-ids->string 'PT.sequence) "PT.sequence")
  (check-equal? (spec-ids->string '(PT.sequence PT.within)) "PT.sequence PT.within"))

(test-case "trials defaults to at least 1000"
  (unless (getenv "OCCUR_TRIALS")
    (check-true (>= (trials) 1000))))
