#lang occur
;; A retraction in the data cancels an event and what was derived from it.
(event-type Swipe [card : String] [amount : Real])
(event-type Charge [card : String] [cents : Integer] #:derived)

(source feed #:types (Swipe)
  #:from (events (Swipe #:at 1 #:id "s1" [card "c1"] [amount 5])
                 (Swipe #:at 2 #:id "s2" [card "c2"] [amount 7])
                 (arrival 3 (retraction-of "s1"))))

(define-stream charges (from feed) (map Charge [card card] [cents (* 100 amount)]))
