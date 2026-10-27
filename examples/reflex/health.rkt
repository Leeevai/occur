#lang occur
;; Fleet health (spec §16.3): per device and per sensor, by arrival age and
;; data age, exported every minute. Thresholds here: quiet after 2 minutes
;; without arrivals, silent after 5, and late when arriving but with data
;; over a minute old.
(require "../../lib/reflex.rkt")

(define-devices devices ["d1" watched] ["d2" watched])
(define-reflex #:arrivals arrivals #:devices devices #:contributions contributions #:triggers triggers)

(define-fleet-health device-health #:from contributions #:per device #:live 120 #:silent 300 #:stale 60)
(define-fleet-health sensor-health #:from contributions #:per sensor #:live 120 #:silent 300 #:stale 60)

(define (write a sensor type first last)
  (data-arrival a (sensor-write #:owner "d1" #:sensor sensor #:type type #:n 1 #:first first #:last last)))

(script #:final-watermark? #f
 (at 30 arrivals (write 30 "d1-load" "load" 25 30))           ; fresh data
 (at 90 arrivals (write 90 "d1-position" "position" 0 0))     ; arriving, but with old data
 (advance-to 360))                                            ; d2 never sends anything
