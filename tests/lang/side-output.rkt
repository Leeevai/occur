#lang occur
;; A side output from a stage that is not the last (TM.lateness): the late
;; event goes to its own stream while the pipeline continues.
(event-type Swipe [card : String])
(source swipes #:types (Swipe) #:watermark ascending
  #:from (events (Swipe #:at 10 #:id "a" [card "c"]) (Swipe #:at 20 #:id "b" [card "c"])
                 (arrival 21 (Swipe #:at 5 #:id "late" [card "c"]))))
(define-stream on-time (from swipes) (lateness #:policy side #:side-output late-swipes) (filter #t))
