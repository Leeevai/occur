#lang occur
;; Trace R2 (spec §16.7): the high-water-mark merge, one sensor. Each
;; arrival contributes only its share past the sensor's high-water mark;
;; the rest is counted late. A duplicate write and a stale one contribute
;; nothing, so replays converge.
(require "../../lib/reflex.rkt")

(define-devices devices ["d1" watched])
(define-reflex #:arrivals arrivals #:devices devices #:contributions contributions #:triggers triggers)

;; a write at a of n samples over [first, last] by d1's one sensor
(define (write a first last n)
  (data-arrival a (sensor-write #:owner "d1" #:sensor "d1-load" #:type "load" #:n n #:first first #:last last)))

(script #:final-watermark? #f
 (at 100 arrivals (write 100 90 100 10))   ; hw −∞ → 100: contribution [90, 100], 10
 (at 105 arrivals (write 105 95 105 10))   ; hw 100 → 105: share round(10 · 5/10) = 5, [100, 105]; late 5
 (at 106 arrivals (write 106 95 105 10))   ; the duplicate: nothing new; late 15
 (at 107 arrivals (write 107 80 90 10)))   ; stale: nothing new; late 25
