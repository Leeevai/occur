#lang occur
;; A rule edit from a script (RL.rejudge; trace R6 of §16.7 with the core
;; forms): two versions of the rule "threshold", the second pushed at 1000.
(event-type Arrival [owner : String] [sensor : String] [kind : String])

(source arrivals #:types (Arrival) #:key owner #:substream sensor #:kind kind)
(define (reading ts) (Arrival #:ts ts #:te ts #:n 1 [owner "d1"] [sensor "s1"] [kind "load"]))

(rule-template threshold #:condition (pt:accumulate #:count 3)
  #:params ([threshold 50]) #:applies-to ("d1") #:in-source)
(rule-template threshold/edited #:id "threshold" #:condition (pt:accumulate #:count 3)
  #:params ([threshold 40]) #:applies-to ("d1") #:in-source)

(rule-source alarms threshold)
(rule-source alarms/edited #:at-start? #f threshold/edited)

(define-stream triggers (from arrivals) (rules))

(script #:final-watermark? #f
 (at 500 arrivals (reading 500))
 (advance-to 1000)
 (rule-source-push 'alarms alarms/edited)    ; a job-only edit: params only
 (at 1010 arrivals (reading 510))
 (at 1020 arrivals (reading 520)))
