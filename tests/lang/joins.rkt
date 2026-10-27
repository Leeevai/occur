#lang occur
;; Joins as stages (spec §9).
(event-type Order [account : String] [amount : Real])
(event-type Payment [account : String] [paid : Real])
(event-type Tier [account : String] [tier : String])

(source orders #:types (Order)
  #:from (events (Order #:at 1 [account "a"] [amount 5]) (Order #:at 2 [account "b"] [amount 7])
                 (Order #:at 30 [account "a"] [amount 9])))
(source payments #:types (Payment)
  #:from (events (Payment #:at 3 [account "a"] [paid 5]) (Payment #:at 31 [account "a"] [paid 1])))
(source tiers #:types (Tier)
  #:from (events (arrival 0 (Tier #:at 0 [account "a"] [tier "gold"]))
                 (arrival 20 (Tier #:at 20 [account "a"] [tier "platinum"]))))

(define-stream settled (from orders)
  (join payments #:kind window #:window (tumbling #:size 10) #:key account
        #:on (= left.amount right.paid)))
(define-stream nearby (from orders)
  (join payments #:kind interval #:lower 0 #:upper 5 #:key account
        #:select ([account left.account] [gap (- right.paid left.amount)])))
(define-stream unpaid (from orders)
  (join payments #:kind window #:window (tumbling #:size 10) #:key account #:outer left #:type Unpaid))
(define-stream with-tier (from orders) (join tiers #:kind table #:key account))
(define-stream tier-then (from orders) (join tiers #:kind as-of #:key account #:outer left))
