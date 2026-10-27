#lang occur
;; key-by with sub-streams and kinds (ES.substream): writes re-keyed by the
;; owning device, merged per sensor, and watched by measurement type.
(event-type Write [owner : String] [sensor : String] [type : String])

(source writes #:types (Write))
(define (write t sensor type) (Write #:at t [owner "d1"] [sensor sensor] [type type]))

(define-stream contributions (from writes)
  (key-by owner #:substream sensor #:kind type)
  (monotonic-merge))

(rule-template loads #:condition (pt:accumulate #:count 2) #:watch ("load"))
(define-stream triggers (from contributions) (rules))

(script
 (at 1 writes (write 1 "s1" "load"))
 (at 2 writes (write 2 "s2" "temperature"))   ; not a watched kind
 (at 3 writes (write 3 "s1" "load"))          ; the second load: a trigger
 (at 4 writes (write 2 "s1" "load")))         ; behind s1's high-water mark
