#lang occur
;; Aggregation stages (spec §8), as in the sketch of the language:
;; per-card spending over sliding windows.
(event-type Swipe [card : String] [amount : Real])

(source feed #:types (Swipe)
  #:from (events (Swipe #:at 30 [card "c1"] [amount 10])
                 (Swipe #:at 90 [card "c1"] [amount 30])
                 (Swipe #:at 100 [card "c2"] [amount 5])
                 (Swipe #:at 400 [card "c1"] [amount 50])))

(define-stream spend
  (from feed Swipe)
  (key-by card)
  (window (sliding #:size (minutes 10) #:slide (minutes 5)))
  (aggregate [total (sum amount)] [n (count)] [p95 (percentile amount 0.95)]))

(define-stream running (from feed) (key-by card) (aggregate [total (sum amount)] [hi (max amount)]))

(define-stream stats
  (from feed)
  (window (tumbling #:size (hours 1)))
  (aggregate [mean (avg amount)] [sd (stddev amount)] [cards (count-distinct card)] [top (top-k amount 2)]
             [busiest (mode card)] [middle (median amount)] [first-card (first card)] [last-card (last card)]
             [biggest (argmax amount)] [span (span)] [approx-cards (approx-distinct card)]))
