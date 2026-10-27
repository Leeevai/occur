#lang occur
;; A minimal program: two types, an inline source, and a pass-through stream.
(event-type Swipe [card : String] [amount : Real])
(event-type Verified [card : String])

(source feed
  #:types (Swipe Verified)
  #:from (events (Swipe #:at 60 #:id "e1" [card "c1"] [amount 2])
                 (arrival 240 (Verified #:at 210 #:id "v1" [card "c1"]))
                 (Swipe #:at 120 #:id "e2" [card "c1"] [amount 600])))

(define-stream all-events (from feed))
(define-stream swipes (from feed Swipe))
