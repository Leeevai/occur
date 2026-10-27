#lang racket/base
(require rackunit
         rackcheck
         "util.rkt"
         "../private/time.rkt")

(spec-test 'ES.time "time points are non-NaN extended reals; durations are non-negative"
  (check-true (time? 0))
  (check-true (time? 3/2))
  (check-true (time? -inf-time))
  (check-true (time? +inf-time))
  (check-false (time? +nan.0))
  (check-false (time? "10:00"))
  (check-true (duration? 0))
  (check-false (duration? -1))
  (check-true (duration? +inf.0)))

(spec-test 'ES.time "duration constructors are in seconds and preserve exactness"
  (check-equal? (milliseconds 1500) 3/2)
  (check-equal? (minutes 10) 600)
  (check-equal? (hours 1) 3600)
  (check-equal? (days 1) 86400)
  (check-equal? (weeks 1) 604800)
  (check-true (exact? (minutes 1/2))))

(spec-test 'ES.time "intervals require start <= end; points are degenerate intervals"
  (check-exn exn:fail:contract? (λ () (interval 5 4)))
  (define p (make-point 7))
  (check-true (interval-point? p))
  (check-equal? (interval-length p) 0)
  (check-equal? (interval-length (interval 2 10)) 8)
  (check-true (interval-contains? (interval 2 10) 2))
  (check-true (interval-contains? (interval 2 10) 10))
  (check-false (interval-contains? (interval 2 10) 11))
  (check-equal? (interval-hull (interval 2 3) (interval 5 9) (make-point 1)) (interval 1 9))
  (check-equal? (interval-intersect (interval 2 6) (interval 4 9)) (interval 4 6))
  (check-false (interval-intersect (interval 2 3) (interval 4 9)))
  (check-equal? (interval-length (interval -inf.0 -inf.0)) 0))

(spec-test '(ES.time TM.clock) "the three clocks are event time, processing time, and hybrid"
  (check-equal? clocks '(event-time processing-time hybrid))
  (check-true (clock? 'hybrid))
  (check-false (clock? 'wall-clock)))

(spec-test 'ES.time "UTC calendar conversion round-trips"
  (check-equal? (utc 1970 1 1) 0)
  (check-equal? (iso8601->seconds "2026-01-05T10:20:00Z") (utc 2026 1 5 10 20 0))
  (check-equal? (seconds->iso8601 (utc 2026 1 5 10 20 0)) "2026-01-05T10:20:00Z")
  (check-equal? (seconds->iso8601 (+ (utc 2000 2 29 23 59 59) 1/2)) "2000-02-29T23:59:59.500Z")
  (define-values (y m d h mi s wd) (utc-fields (utc 2026 10 26 6 0 0)))
  (check-equal? (list y m d h mi s wd) '(2026 10 26 6 0 0 1)) ; a Monday
  (check-equal? (time->string -inf.0) "-inf"))

(spec-test 'ES.time "property: days-from-civil and civil-from-days are inverse"
  (check-property
   (make-config #:tests (trials))
   (property ([z (gen:integer-in -800000 800000)])
     (define-values (y m d) (civil-from-days z))
     (check-equal? (days-from-civil y m d) z))))
