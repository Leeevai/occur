#lang occur
;; Time and progress forms (spec §6), driven by an explicit script.
(event-type Reading [owner : String] [sensor : String] [kind : String])
(event-type Swipe [card : String])
(event-type Heartbeat)

(define (reading kind ts te n) (Reading #:ts ts #:te te #:n n [owner "d1"] [sensor kind] [kind kind]))

(source arrivals #:types (Reading)
  #:key owner #:substream sensor #:kind kind #:merge high-water-mark #:skew-guard (hours 1))
(source swipes #:types (Swipe Heartbeat) #:watermark (heartbeat Heartbeat))

;; trace R3's watermarks, plus one far-future arrival that the guard drops
(define-stream progress (from arrivals) (keyed-watermark #:kinds ("load" "position") #:mode all))
(define-stream on-time (from swipes Swipe) (lateness #:policy side #:side-output late-swipes))
(define-stream delayed (from swipes Swipe) (delay 5))
(define-stream quiet (from arrivals) (idle (minutes 10)))
(define-schedule hourly "0 * * * *")

(script
 (at 0 arrivals (reading "load" 0 50 50))
 (at 1 arrivals (reading "position" 0 70 70))
 (at 2 arrivals (reading "load" 51 130 80))
 (at 3 arrivals (reading "load" 999999 999999 1))
 (at 10 swipes (Swipe #:at 10 [card "a"]))
 (at 20 swipes (Heartbeat #:at 100))
 (at 30 swipes (Swipe #:at 40 [card "late"]))
 (advance-to 3700))
