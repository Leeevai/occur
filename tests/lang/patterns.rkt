#lang occur
;; define-pattern (spec §11): the card-testing pattern of §15 under the most
;; permissive semantics, plus one pattern per operator family.
(event-type Swipe [card : String] [amount : Real])
(event-type Verified [card : String])
(event-type SuspectedFraud [card : String] [n : Integer] [total : Real] #:derived)

(source feed #:types (Swipe Verified) #:key card
  #:from (events (Swipe #:at (minutes 1) #:id "e1" [card "c1"] [amount 2])
                 (Swipe #:at (minutes 2) #:id "e2" [card "c1"] [amount 600])
                 (Swipe #:at (minutes 3) #:id "e3" [card "c1"] [amount 50])
                 (Swipe #:at (minutes 4) #:id "e4" [card "c1"] [amount 900])))

(define-pattern card-testing
  #:from feed #:within (minutes 10)
  (seq (a : Swipe (< amount 5))
       (absent Verified)
       (kleene+ (b : Swipe (> amount 500))))
  #:where (increasing b.amount)
  #:emit (SuspectedFraud [card a.card] [n (count b)] [total (sum b.amount)]))

(define-pattern big-pair #:from feed #:within (minutes 5)
  (and (x : Swipe (> amount 100)) (y : Swipe (> amount 100)))
  #:where (string<? x.card "z"))

(define-pattern small-or-verified #:from feed (or (s : Swipe (< amount 10)) (v : Verified)))
