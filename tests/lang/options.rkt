#lang occur
;; Script options: the simulated clock starts at 10:20 UTC, and the run ends
;; without a final watermark, so the open window stays open.
(event-type Reading [dev : String])
(source readings #:types (Reading) #:key dev #:watermark ascending)

(define-schedule hourly "0 * * * *")
(define-stream per-minute (from readings) (window (tumbling #:size 60)))

(script #:start (utc 2026 1 5 10 20) #:final-watermark? #f
 (at (utc 2026 1 5 10 30) readings (Reading #:at (utc 2026 1 5 10 30) [dev "d1"]))
 (advance-to (utc 2026 1 5 12 0)))
