#lang occur
;; Checkpoint, restore, and cold-start replay in a script (spec §6, §16.4).
(event-type Arrival [owner : String] [sensor : String] [kind : String])

(source arrivals #:types (Arrival) #:key owner #:substream sensor #:kind kind #:merge high-water-mark)
(define (reading ts n) (Arrival #:ts ts #:te ts #:n n [owner "d1"] [sensor "s1"] [kind "load"]))

(rule-template enough #:condition (pt:accumulate #:count 2) #:applies-to none)
(rule-source alarms (bind enough #:on "d1"))

(define-stream triggers (from arrivals) (rules))

(script
 (at 1 arrivals (reading 1 1))
 (at 2 arrivals (reading 2 1))        ; trigger 1
 (checkpoint)
 (at 3 arrivals (reading 3 1))
 (at 4 arrivals (reading 4 1))        ; trigger 2
 (restore)                            ; crash: back to the checkpoint, re-read 3 and 4
 (at 5 arrivals (reading 5 2))        ; trigger 3
 (cold-start)                         ; crash without a checkpoint: replay everything
 (rule-source-push 'alarms alarms)    ; the first rules push after the restart
 (at 6 arrivals (reading 6 2)))       ; trigger 4
