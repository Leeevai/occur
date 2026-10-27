#lang occur
;; Trace R5 (spec §16.7): a schedule with missed ticks. Rule: cron("0 * * * *").
;; The run starts at 10:20 UTC, when the rule's instance is created, so its
;; first tick is 11:00 and its first period starts at 10:00. The evaluator
;; is suspended from 10:50 to 13:10.
(require "../../lib/reflex.rkt")

(define-devices devices ["d1" watched])
(define-reflex #:arrivals arrivals #:devices devices #:contributions contributions #:triggers triggers)

(reflex-rule hourly
  #:condition (cron "0 * * * *")
  #:target "hourly-report"
  #:applies-to ("d1"))

(define (at-time h m) (utc 2026 1 5 h m))
(define (write a first last n)
  (data-arrival a (sensor-write #:owner "d1" #:sensor "d1-load" #:type "load" #:n n #:first first #:last last)))

(script #:start (at-time 10 20) #:final-watermark? #f
 (at (at-time 10 30) arrivals (write (at-time 10 30) (at-time 10 25) (at-time 10 30) 2))
 (at (at-time 10 45) arrivals (write (at-time 10 45) (at-time 10 40) (at-time 10 45) 3))
 (suspend (at-time 10 50) (at-time 13 10))
 ;; at 13:10: one schedule output, window [10:00, 13:00], windows 3, samples 5
 ;; at 14:00: window [13:00, 14:00], windows 1, samples 0 (an empty window still fires)
 (advance-to (at-time 14 30)))
