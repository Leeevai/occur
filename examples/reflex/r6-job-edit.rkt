#lang occur
;; Trace R6 (spec §16.7), job-only edit. Rule: accumulate(N = 3),
;; min_interval = 0, params {threshold: 50}. The device has one pending
;; contribution at [500, 500] when, at a = 1000, the rule's params become
;; {threshold: 40}: only the job's configuration changed.
(require "../../lib/reflex.rkt")

(define-devices devices ["d1" watched])
(define-reflex #:arrivals arrivals #:devices devices #:contributions contributions #:triggers triggers)

;; two versions of the rule "threshold-check", in the rule source "alarms"
(reflex-rule threshold-check
  #:condition (accumulate #:min-samples 3) #:min-interval-s 0
  #:target "threshold-evaluator" #:params ([threshold 50]) #:applies-to ("d1")
  #:in-source)
(reflex-rule threshold-check/edited #:id "threshold-check"
  #:condition (accumulate #:min-samples 3) #:min-interval-s 0
  #:target "threshold-evaluator" #:params ([threshold 40]) #:applies-to ("d1")
  #:in-source)

(rule-source alarms threshold-check)
(rule-source alarms/edited #:at-start? #f threshold-check/edited)

(define (write a t)
  (data-arrival a (sensor-write #:owner "d1" #:sensor "d1-load" #:type "load" #:n 1 #:first t)))

(script #:final-watermark? #f
 (at 500 arrivals (write 500 500))
 (advance-to 1000)
 (rule-source-push 'alarms alarms/edited)   ; config at 1000: window [500, 500], samples 0, params {threshold: 40}
 (at 1010 arrivals (write 1010 510))
 (at 1020 arrivals (write 1020 520)))       ; samples: window [500, 520], samples 3
