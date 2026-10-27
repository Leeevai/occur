#lang occur
;; A library: a type and a helper, and nothing to run.
(event-type Ping [dev : String])
(define (ping t dev) (Ping #:at t [dev dev]))
(provide Ping ping)
