#lang occur
;; Allen relations in stage expressions (spec §10): alarms during a shift.
(event-type Alarm [code : String])

(define shift (interval (hours 8) (hours 17)))

(source alarms #:types (Alarm)
  #:from (events (Alarm #:at (hours 7) [code "a"]) (Alarm #:at (hours 8) [code "b"])
                 (Alarm #:at (hours 12) [code "c"]) (Alarm #:at (hours 17) [code "d"])))

(define-stream in-shift (from alarms) (filter (during? it shift)))
(define-stream at-edges (from alarms) (filter (memq (allen-relation it shift) '(starts finishes))))
