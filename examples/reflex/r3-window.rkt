#lang occur
;; Trace R3 (spec §16.7): a window rule with require_all, window(w = 60,
;; o = 0) on the kinds {load, position}. Windows close under the device's
;; keyed watermark W_all, the minimum over the watched kinds; a second rule,
;; identical but without require_all, closes them under W_any instead.
(require "../../lib/reflex.rkt")

(define-devices devices ["d1" watched])
(define-reflex #:arrivals arrivals #:devices devices #:contributions contributions #:triggers triggers)

(reflex-rule every-kind
  #:condition (window #:window-s 60 #:offset-s 0 #:require-all #t)
  #:watch ("load" "position")
  #:target "all-kinds-job"
  #:applies-to ("d1"))

(reflex-rule any-kind
  #:condition (window #:window-s 60 #:offset-s 0 #:require-all #f)
  #:watch ("load" "position")
  #:target "any-kind-job"
  #:applies-to ("d1"))

;; a write at step a of n samples of one kind over [first, last]
(define (write a type first last n)
  (data-arrival a (sensor-write #:owner "d1" #:sensor (string-append "d1-" type) #:type type
                                #:n n #:first first #:last last)))

(script #:final-watermark? #f
 (at 1 arrivals (write 1 "load" 0 50 50))          ; W_all −∞ (no position yet); grid anchored at 0
 (at 2 arrivals (write 2 "position" 0 70 70))      ; W_all 50; W_any 70 closes [0, 60) for any-kind
 (at 3 arrivals (write 3 "load" 51 130 80))        ; W_all 70: window [0, 60], samples 200
 (at 4 arrivals (write 4 "position" 71 250 180))   ; W_all 130: window [60, 120], samples 180
 (at 5 arrivals (write 5 "load" 131 300 170)))     ; W_all 250: window [120, 240], windows 2, samples 170
