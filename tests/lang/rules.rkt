#lang occur
;; Rules as data (spec §14) with the core condition forms.
(event-type Arrival [owner : String] [sensor : String] [kind : String])

(source arrivals #:types (Arrival) #:key owner #:substream sensor #:kind kind #:merge high-water-mark)

(define (reading owner ts te n) (Arrival #:ts ts #:te te #:n n [owner owner] [sensor (string-append owner "-s")] [kind "load"]))

;; trace R1 (§16.7): accumulate(N = 3, S = 0) with a deferring 60 s rate limit
(rule-template threshold-check
  #:condition (pt:accumulate #:count 3 #:span 0)
  #:rate-limit (60 defer)
  #:target "threshold-evaluator"
  #:params ([threshold 50])
  #:applies-to ("d1"))

(rule-template quiet
  #:condition (pt:silence #:after 1000)
  #:target "connectivity"
  #:applies-to none)

(rule-source alarms
  (bind threshold-check #:on "d2" #:key "cfg-41" #:count 2 #:params ([threshold 40]))
  (bind quiet #:on "d1" #:also ("d2") #:key "cfg-9" #:after 500))

(define-stream triggers (from arrivals) (rules))

(script
 (at 0 arrivals (reading "d1" 0 0 1))
 (at 10 arrivals (reading "d1" 10 10 1))
 (at 20 arrivals (reading "d1" 20 20 1))
 (at 30 arrivals (reading "d1" 25 30 2))
 (at 40 arrivals (reading "d1" 40 40 1))
 (at 50 arrivals (reading "d1" 50 50 1))
 (at 55 arrivals (reading "d2" 55 55 1))
 (at 56 arrivals (reading "d2" 56 56 1))
 (advance-to 100))
