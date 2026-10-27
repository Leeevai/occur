#lang occur
;; Trace R1 (spec §16.7): accumulate with a deferring rate limit.
;; Rule: accumulate(N = 3, S = 0), min_interval = 60, no max_delay. Times are
;; processing time a and sample intervals in event time, both in seconds.
(require "../../lib/reflex.rkt")

(define-devices devices ["d1" watched])
(define-reflex #:arrivals arrivals #:devices devices #:contributions contributions #:triggers triggers)

(reflex-rule threshold-check
  #:condition (accumulate #:min-samples 3 #:min-span-s 0)
  #:min-interval-s 60
  #:target "threshold-evaluator"
  #:applies-to ("d1"))

;; a write at a of n load samples over [first, last] by d1's sensor
(define (load a first last n)
  (data-arrival a (sensor-write #:owner "d1" #:sensor "d1-load" #:type "load" #:n n #:first first #:last last)))

(script #:final-watermark? #f
 (at 0 arrivals (load 0 0 0 1))
 (at 10 arrivals (load 10 10 10 1))
 (at 20 arrivals (load 20 20 20 1))   ; 3 pending: samples at 20, window [0, 20]; capped until 80
 (at 30 arrivals (load 30 25 30 2))
 (at 40 arrivals (load 40 40 40 1))   ; ready, but capped
 (at 50 arrivals (load 50 50 50 1))   ; capped
 (advance-to 100))                    ; at 80: samples, window [25, 50], samples 4
