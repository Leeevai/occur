#lang occur
;; A JSON Lines source and an explicit script with a simulated clock.
(event-type Swipe [card : String] [amount : Real])
(event-type Verified [card : String])

(source feed #:types (Swipe Verified) #:from (jsonl "../data/swipes.jsonl")
  #:watermark (bounded-delay 30))

(define-stream verified (from feed Verified))

(script
 (at 5 feed (Verified #:at 1 #:id "v1" [card "c9"]))
 (advance-to 50)
 (watermark feed 40)
 (at 60 feed (Swipe #:at 55 #:id "s1" [card "c9"] [amount 3]))
 (suspend 70 90))
