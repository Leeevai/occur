#lang occur
;; Spec §15, worked example: card testing.
;;
;; A tiny charge is followed by escalating large charges, with no customer
;; verification in between:
;;
;;   key-by card
;;     ( a:Swipe[amount < 5] ; ¬Verified ; (b:Swipe[amount > 500])+ )
;;     where increasing(b.amount)
;;     within 10 min
;;   derive SuspectedFraud(card = a.card, n = count(b), total = sum(b.amount))
;;
;; Run it with `racket examples/card-testing.rkt`. Each output line starts
;; with the processing time and the name of the pattern that produced it.

(event-type Swipe [card : String] [amount : Real])
(event-type Verified [card : String])
(event-type SuspectedFraud [card : String] [n : Integer] [total : Real] #:derived)

;; The pattern, for one selection strategy and one output policy. The
;; source is keyed by card, so each card is matched on its own.
(define-syntax-rule (card-testing name source selection output)
  (define-pattern name #:from source #:within (minutes 10)
    #:selection selection #:consumption unrestricted #:output output
    (seq (a : Swipe (< amount 5))
         (absent Verified)
         (kleene+ (b : Swipe (> amount 500))))
    #:where (increasing b.amount)
    #:emit (SuspectedFraud [card a.card] [n (count b)] [total (sum b.amount)])))

;; ---------------------------------------------------------------------------
;; Trace A: card c1, times in minutes. e1 is the only possible a; e3 (50)
;; qualifies for neither binding.

(source trace-a #:types (Swipe Verified) #:key card #:watermark ascending
  #:from (events (Swipe #:at (minutes 1) #:id "e1" [card "c1"] [amount 2])
                 (Swipe #:at (minutes 2) #:id "e2" [card "c1"] [amount 600])
                 (Swipe #:at (minutes 3) #:id "e3" [card "c1"] [amount 50])
                 (Swipe #:at (minutes 4) #:id "e4" [card "c1"] [amount 900])))

;; One pattern per cell of the table in §15; the comment is the cell.
(card-testing strict-every      trace-a strict    every)   ; {e1, e2}
(card-testing strict-maximal    trace-a strict    maximal) ; {e1, e2}
(card-testing partition-every   trace-a partition every)   ; {e1, e2}
(card-testing partition-maximal trace-a partition maximal) ; {e1, e2}
(card-testing next-every        trace-a next      every)   ; {e1, e2}, {e1, e2, e4}
(card-testing next-maximal      trace-a next      maximal) ; {e1, e2, e4}
(card-testing any-every         trace-a any       every)   ; {e1, e2}, {e1, e4}, {e1, e2, e4}
(card-testing any-maximal       trace-a any       maximal) ; {e1, e2, e4}

;; Under strict contiguity e3 breaks the Kleene run, so the match ends at
;; e2. Under skip-till-next-match e3 is skipped and the run extends greedily
;; to e4; {e1, e4} is never formed, because e2 was taken deterministically.
;; Under skip-till-any-match b is any time-ordered, increasing subset of
;; {e2, e4}.
;;
;; The maximal match derives SuspectedFraud(card = c1, n = 2, total = 1500)
;; with ts = 1, te = 4, and C = {e1, e2, e4}. Maximal output is deferred: it
;; is emitted once the watermark passes 11 minutes (ts = 1 plus the bound),
;; when no further extension is possible.

;; ---------------------------------------------------------------------------
;; Trace A′, negation scope: trace A plus Verified(c1) at 3.5 minutes, under
;; skip-till-any-match with every output.

(source trace-a-prime #:types (Swipe Verified) #:key card #:watermark ascending
  #:from (events (Swipe #:at (minutes 1) #:id "e1" [card "c1"] [amount 2])
                 (Swipe #:at (minutes 2) #:id "e2" [card "c1"] [amount 600])
                 (Swipe #:at (minutes 3) #:id "e3" [card "c1"] [amount 50])
                 (Verified #:at (minutes 7/2) #:id "v1" [card "c1"])
                 (Swipe #:at (minutes 4) #:id "e4" [card "c1"] [amount 900])))

;; ¬Verified between a and b+: the forbidden gap runs from te(a) to the
;; first b. {e1, e4} is blocked, because its gap (1, 4) contains 3.5.
(card-testing gap-negation trace-a-prime any every)  ; {e1, e2}, {e1, e2, e4}

;; unless Verified over the whole pattern: a match is blocked when its span
;; contains a Verified, so only {e1, e2}, whose span (1, 2) does not contain
;; 3.5, survives.
(define-pattern span-negation #:from trace-a-prime #:within (minutes 10)
  #:selection any #:consumption unrestricted #:output every
  (unless (seq (a : Swipe (< amount 5))
               (kleene+ (b : Swipe (> amount 500))))
          Verified)
  #:where (increasing b.amount)
  #:emit (SuspectedFraud [card a.card] [n (count b)] [total (sum b.amount)]))  ; {e1, e2}
