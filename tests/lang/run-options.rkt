#lang occur
;; run-options: the clock starts at 10:20 UTC while the module runs on its
;; source's own data.
(event-type Reading [dev : String])
(source readings #:types (Reading) #:key dev
  #:from (events (Reading #:at (utc 2026 1 5 10 30) [dev "d1"]) (Reading #:at (utc 2026 1 5 12 5) [dev "d1"])))
(define-schedule hourly "0 * * * *")
(define-stream all-readings (from readings))
(run-options #:start (utc 2026 1 5 10 20))
