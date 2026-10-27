#lang occur
;; Trace R6 (spec §16.7), semantic edit. Rule: accumulate(N = 3) with two
;; pending contributions, [500, 500] and [510, 510], when, at a = 1000, N
;; becomes 2. Two devices run the same edit: d-continue's rule carries its
;; progress (on_update: continue) and d-restart's does not (restart).
(require "../../lib/reflex.rkt")

(define-devices devices ["d-continue" watched] ["d-restart" watched])
(define-reflex #:arrivals arrivals #:devices devices #:contributions contributions #:triggers triggers)

(reflex-rule carry
  #:condition (accumulate #:min-samples 3) #:on-update continue
  #:target "continue-job" #:applies-to ("d-continue") #:in-source)
(reflex-rule carry/edited #:id "carry"
  #:condition (accumulate #:min-samples 2) #:on-update continue
  #:target "continue-job" #:applies-to ("d-continue") #:in-source)
(reflex-rule fresh
  #:condition (accumulate #:min-samples 3) #:on-update restart
  #:target "restart-job" #:applies-to ("d-restart") #:in-source)
(reflex-rule fresh/edited #:id "fresh"
  #:condition (accumulate #:min-samples 2) #:on-update restart
  #:target "restart-job" #:applies-to ("d-restart") #:in-source)

(rule-source alarms carry fresh)
(rule-source alarms/edited #:at-start? #f carry/edited fresh/edited)

(define (write a device t)
  (data-arrival a (sensor-write #:owner device #:sensor (string-append device "-load") #:type "load" #:n 1 #:first t)))

(script #:final-watermark? #f
 (at 500 arrivals (write 500 "d-continue" 500))
 (at 500 arrivals (write 500 "d-restart" 500))
 (at 510 arrivals (write 510 "d-continue" 510))
 (at 510 arrivals (write 510 "d-restart" 510))
 (advance-to 1000)
 ;; continue: samples at once, window [500, 510], samples 2; restart: nothing
 (rule-source-push 'alarms alarms/edited)
 ;; restart waits for two new contributions
 (at 1100 arrivals (write 1100 "d-restart" 1100))
 (at 1110 arrivals (write 1110 "d-restart" 1110)))
