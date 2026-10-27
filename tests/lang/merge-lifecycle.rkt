#lang occur
;; A high-water-mark merge behind a device directory forgets a removed
;; device's marks (ST.monotonic-merge, CX.entity).
(event-type Reading [owner : String] [sensor : String])

(directory devices #:from (entries ["d1" active]))
(source readings #:types (Reading) #:key owner #:substream sensor #:directory devices #:merge high-water-mark)
(define-stream contributions (from readings))

(define (reading ts te n) (Reading #:ts ts #:te te #:n n [owner "d1"] [sensor "s1"]))

(script
 (at 1 readings (reading 90 100 10))
 (directory-push 'devices '() #:at 2)                   ; d1 removed: its state is dropped
 (directory-push 'devices '(("d1" . active)) #:at 3)    ; listed again: activated afresh
 (at 4 readings (reading 80 90 10)))                    ; new again, from −∞
