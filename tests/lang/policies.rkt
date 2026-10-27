#lang occur
;; §12 policies through #lang occur.
(event-type Swipe [card : String] [amount : Real])
(event-type Verified [card : String])
(event-type A [k : String])
(event-type B [k : String])
(event-type X [k : String])
(event-type State [k : String] [state : String])

(source feed #:types (Swipe Verified) #:key card #:watermark ascending
  #:from (events (Swipe #:at (minutes 1) #:id "e1" [card "c1"] [amount 2])
                 (Swipe #:at (minutes 2) #:id "e2" [card "c1"] [amount 600])
                 (Swipe #:at (minutes 3) #:id "e3" [card "c1"] [amount 50])
                 (Swipe #:at (minutes 4) #:id "e4" [card "c1"] [amount 900])))

(define-pattern card-testing-next
  #:from feed #:within (minutes 10) #:selection skip-till-next-match #:output maximal
  (seq (a : Swipe (< amount 5)) (absent Verified) (kleene+ (b : Swipe (> amount 500))))
  #:where (increasing b.amount))

(define-pattern card-testing-strict
  #:from feed #:within (minutes 10) #:selection strict
  (seq (a : Swipe (< amount 5)) (absent Verified) (kleene+ (b : Swipe (> amount 500)))))

(source ab #:types (A B) #:key k
  #:from (events (A #:at 1 #:id "a1" [k "x"]) (A #:at 2 #:id "a2" [k "x"])
                 (B #:at 3 #:id "b1" [k "x"]) (B #:at 4 #:id "b2" [k "x"])))
(define-pattern paired #:from ab #:consumption chronicle (seq (a : A) (b : B)))
(define-pattern latest #:from ab #:consumption recent (seq (a : A) (b : B)))
(define-pattern firsts #:from ab #:output first #:within 10 (seq (a : A) (kleene+ (b : B))))
(define-pattern skipping #:from ab #:skip past-last #:within 10 (seq (kleene+ (a : A)) (b : B)))

(source xs #:types (X) #:key k
  #:from (events (arrival 0 (X #:at 0 #:id "x0" [k "d"])) (arrival 1 (X #:at 1 #:id "x1" [k "d"]))
                 (arrival 40 (X #:at 40 #:id "x2" [k "d"]))))
(define-pattern three
  #:from xs #:within 100 #:selection next #:output first #:consumption cumulative
  #:timeout-partial (10 processing-time) #:retain (timeout)
  (kleene+ (b : X))
  #:where (>= (count b) 3))

(source states #:types (State) #:key k
  #:from (events (State #:at 1 [k "d"] [state "silent"]) (State #:at 2 [k "d"] [state "silent"])
                 (State #:at 3 [k "d"] [state "live"]) (State #:at 4 [k "d"] [state "silent"])))
(define-stream transitions (from states) (edge #:enter (equal? state "silent") #:leave (equal? state "live")))
(define-stream capped (from states) (rate-limit 2 #:mode drop))
(define-stream together (from states) (coalesce))
