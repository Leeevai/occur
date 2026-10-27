#lang occur
;; examples/card-testing.rkt on the reference oracle (#:engine reference):
;; the same sources and patterns, so the acceptance test can check that the
;; incremental engine and the oracle agree on every trace of §15.
(event-type Swipe [card : String] [amount : Real])
(event-type Verified [card : String])
(event-type SuspectedFraud [card : String] [n : Integer] [total : Real] #:derived)

(define-syntax-rule (card-testing name source selection output)
  (define-pattern name #:from source #:within (minutes 10) #:engine reference
    #:selection selection #:consumption unrestricted #:output output
    (seq (a : Swipe (< amount 5))
         (absent Verified)
         (kleene+ (b : Swipe (> amount 500))))
    #:where (increasing b.amount)
    #:emit (SuspectedFraud [card a.card] [n (count b)] [total (sum b.amount)])))

(source trace-a #:types (Swipe Verified) #:key card #:watermark ascending
  #:from (events (Swipe #:at (minutes 1) #:id "e1" [card "c1"] [amount 2])
                 (Swipe #:at (minutes 2) #:id "e2" [card "c1"] [amount 600])
                 (Swipe #:at (minutes 3) #:id "e3" [card "c1"] [amount 50])
                 (Swipe #:at (minutes 4) #:id "e4" [card "c1"] [amount 900])))
(card-testing strict-every      trace-a strict    every)
(card-testing strict-maximal    trace-a strict    maximal)
(card-testing partition-every   trace-a partition every)
(card-testing partition-maximal trace-a partition maximal)
(card-testing next-every        trace-a next      every)
(card-testing next-maximal      trace-a next      maximal)
(card-testing any-every         trace-a any       every)
(card-testing any-maximal       trace-a any       maximal)

(source trace-a-prime #:types (Swipe Verified) #:key card #:watermark ascending
  #:from (events (Swipe #:at (minutes 1) #:id "e1" [card "c1"] [amount 2])
                 (Swipe #:at (minutes 2) #:id "e2" [card "c1"] [amount 600])
                 (Swipe #:at (minutes 3) #:id "e3" [card "c1"] [amount 50])
                 (Verified #:at (minutes 7/2) #:id "v1" [card "c1"])
                 (Swipe #:at (minutes 4) #:id "e4" [card "c1"] [amount 900])))
(card-testing gap-negation trace-a-prime any every)
(define-pattern span-negation #:from trace-a-prime #:within (minutes 10) #:engine reference
  #:selection any #:consumption unrestricted #:output every
  (unless (seq (a : Swipe (< amount 5))
               (kleene+ (b : Swipe (> amount 500))))
          Verified)
  #:where (increasing b.amount)
  #:emit (SuspectedFraud [card a.card] [n (count b)] [total (sum b.amount)]))

(source trace-b #:types (Swipe Verified) #:key card #:watermark ascending
  #:from (events (Swipe #:at (minutes 1) #:id "f1" [card "c2"] [amount 1])
                 (Verified #:at (minutes 2) #:id "f2" [card "c2"])
                 (Swipe #:at (minutes 3) #:id "f3" [card "c2"] [amount 800])))
(card-testing b-strict    trace-b strict    every)
(card-testing b-partition trace-b partition every)
(card-testing b-next      trace-b next      every)
(card-testing b-any       trace-b any       every)
