#lang occur
;; Rule sources converge (spec §14 RL.converge, §16.2): replacing a source
;; with identical content changes nothing and emits nothing, so a service
;; that syncs the same rules again and again is harmless. Here the rule
;; source is pushed again at 15, between two contributions.
(require "../../lib/reflex.rkt")

(define-devices devices ["d1" watched])
(define-reflex #:arrivals arrivals #:devices devices #:contributions contributions #:triggers triggers)

(reflex-rule pairs #:condition (accumulate #:min-samples 2)
  #:target "pair-job" #:applies-to ("d1") #:in-source)
(rule-source alarms pairs)

(define (write a t)
  (data-arrival a (sensor-write #:owner "d1" #:sensor "d1-load" #:type "load" #:n 1 #:first t)))

(script #:final-watermark? #f
 (at 10 arrivals (write 10 10))
 (rule-source-push 'alarms alarms)   ; the same source again: no edit, no output
 (at 20 arrivals (write 20 20)))     ; samples at 20, both contributions counted
