#lang occur
;; Recovery (spec §16.4). A restart restores the last checkpoint and re-reads
;; what arrived after it, so the triggers of that interval repeat with the
;; same identity, and downstream deduplication drops them. A cold start
;; has no checkpoint: it replays all retained input with processing-time
;; deadlines suspended, holds the triggers back, and releases them,
;; coalesced to one per instance, once the first rules push has landed;
;; each is judged against the instances then in force.
(require "../../lib/reflex.rkt")

(define-devices devices ["d1" watched])
(define-reflex #:arrivals arrivals #:devices devices #:contributions contributions #:triggers triggers)

(reflex-rule pairs #:condition (accumulate #:min-samples 2)
  #:target "pair-job" #:applies-to ("d1") #:in-source)
(reflex-rule quiet #:condition (silence #:after-s 100)
  #:target "connectivity" #:applies-to ("d1") #:in-source)

(rule-source alarms pairs quiet)
;; after the cold start, the rules pushed first no longer include quiet
(rule-source alarms/after-restart #:at-start? #f pairs)

(define (write a t)
  (data-arrival a (sensor-write #:owner "d1" #:sensor "d1-load" #:type "load" #:n 1 #:first t)))

(script #:final-watermark? #f
 (at 10 arrivals (write 10 10))
 (at 20 arrivals (write 20 20))      ; pairs: samples at 20, [10, 20]
 (checkpoint)
 (at 30 arrivals (write 30 30))
 (at 40 arrivals (write 40 40))      ; pairs: samples at 40, [30, 40]
 (restore)                           ; restart: back to the checkpoint; 30 and 40 are re-read
 (at 50 arrivals (write 50 50))
 (advance-to 200)                    ; quiet: silence at 150, [50, 150]
 (cold-start)                        ; no checkpoint: replay everything, triggers held
 (rule-source-push 'alarms alarms/after-restart))   ; released: one per instance, quiet's dropped
