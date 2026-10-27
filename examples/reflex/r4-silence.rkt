#lang occur
;; Trace R4 (spec §16.7): silence counted from activation, with recovery.
;; Rule: silence(Δ = 600, emit_recovery). The device is activated at a = 0,
;; when the directory lists it, and sends nothing at first.
(require "../../lib/reflex.rkt")

(define-devices devices ["d1" watched])     ; activated at 0
(define-reflex #:arrivals arrivals #:devices devices #:contributions contributions #:triggers triggers)

(reflex-rule connectivity
  #:condition (silence #:after-s 600 #:emit-recovery #t)
  #:target "connectivity-check"
  #:applies-to ("d1"))

(define (write a first last n)
  (data-arrival a (sensor-write #:owner "d1" #:sensor "d1-load" #:type "load" #:n n #:first first #:last last)))

(script #:final-watermark? #f
 ;; at 600 (the timer, 0 + 600): silence, window [none, 600]
 (at 900 arrivals (write 900 880 890 2))      ; recovered, window [none, 890]
 ;; at 1490 (the timer, 890 + 600 on the hybrid clock): silence, window [890, 1490]
 (at 1600 arrivals (write 1600 1595 1600 1))  ; recovered, window [890, 1600]
 (advance-to 1700))
