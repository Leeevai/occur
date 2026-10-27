#lang occur
;; A small fleet on Reflex (spec §16): two pumps and a tank, watched from
;; midnight UTC, and one deprecated device. A gateway's storage writes
;; arrive as data (fleet-arrivals.jsonl): one write may list sensors of
;; several devices, and each sensor counts for the device that owns it.
;; Rules arrive as data too: templates and bindings declared here, and a
;; second rule source read from fleet-rules.jsonl.
(require "../../lib/reflex.rkt")

(run-options #:start (utc 2026 1 5 0 0) #:final-watermark? #f)

;; §16.1 The device directory: watched devices are active from the start;
;; the deprecated one's arrivals are ignored and counted.
(define-devices devices ["pump-1" watched] ["pump-2" watched] ["tank-1" watched] ["old-1" deprecated])

(define-reflex #:arrivals arrivals #:from (jsonl "fleet-arrivals.jsonl")
               #:devices devices #:contributions contributions #:triggers triggers)

;; §16.2 Rules. A pressure check applies only where it is bound: once to
;; both pumps (fan-out) under the binding key "standard", and once more to
;; pump-2 under "strict", with a lower threshold and fewer samples.
(reflex-rule pressure-check
  #:condition (accumulate #:min-samples 10 #:min-span-s 60)
  #:watch ("pressure")
  #:max-delay-s 900
  #:min-interval-s 300
  #:target "pressure-evaluator" #:params ([threshold 50])
  #:applies-to none)

;; the tank's 15-minute report runs once level and temperature both cover
;; its window
(reflex-rule tank-report
  #:condition (window #:window-s 900 #:offset-s 0 #:require-all #t)
  #:watch ("level" "temperature")
  #:target "tank-report" #:applies-to ("tank-1"))

;; every device: connectivity after 30 quiet minutes, and a nightly summary
(reflex-rule offline
  #:condition (silence #:after-s 1800 #:emit-recovery #t)
  #:target "connectivity")
(reflex-rule nightly
  #:condition (cron "0 2 * * *")
  #:target "nightly-summary")

(rule-source pumps
  (bind pressure-check #:on "pump-1" #:also ("pump-2") #:key "standard")
  (bind pressure-check #:on "pump-2" #:key "strict" #:min-samples 5 #:params ([threshold 40])))

;; A second source: a vibration rule bound to pump-1, a rule that claims
;; the id pressure-check (refused: it belongs to the templates), and a
;; binding to a rule that does not exist (dangling).
(rule-source vendor (jsonl "fleet-rules.jsonl"))

;; §16.3 Health, per device and per sensor, exported every minute: see
;; examples/reflex/health.rkt.
