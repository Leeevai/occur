#lang occur
;; Payment fraud: one program over card payments, from ingestion to alerts.
;; Times are minutes past midnight; the comments give the outputs.
;;
;; Run it with `racket examples/fraud.rkt`; tests/fraud-test.rkt checks it.

(event-type Payment [card : String] [merchant : String] [amount : Real] [country : String]
                    [channel : String] [ref : String])
(event-type Basket [card : String] [items : (Listof (Record [sku : String] [price : Real]))])
(event-type Item [card : String] [sku : String] [price : Real] #:derived)
(event-type Cents [card : String] [cents : Integer] #:derived)

(event-type Login [card : String] [country : String])
(event-type Decline [card : String] [reason : String])
(event-type ThreeDS [card : String])
(event-type CardInfo [card : String] [holder : String] [home : String])
(event-type Limit [card : String] [limit : Real])

(define (pay t id card merchant amount #:country [country "PT"] #:channel [channel "web"] #:ref [ref id])
  (Payment #:at (minutes t) #:id id [card card] [merchant merchant] [amount amount] [country country]
           [channel channel] [ref ref]))

;; ---------------------------------------------------------------------------
;; Ingestion (§4, §5, §6)

;; Card payments arrive from the web gateway, in event-time order or nearly:
;; the watermark trails the newest payment by two minutes.
(source web #:types (Payment) #:watermark (bounded-delay (minutes 2))
  #:from (events (pay 1 "p1" "c1" "m-shoes" 2)
                 (pay 2 "p2" "c1" "m-shoes" 600)
                 (pay 3 "p3" "c1" "m-books" 50)
                 (pay 4 "p4" "c1" "m-shoes" 900)
                 ;; the gateway delivers p4's webhook twice
                 (arrival (minutes 9/2) (pay 4 "p4-again" "c1" "m-shoes" 900 #:ref "p4"))
                 (pay 10 "p5" "c2" "m-tv" 1200 #:country "US")
                 (pay 12 "p6" "c2" "m-food" 30 #:country "US")
                 ;; p8 is stamped 8 minutes but arrives at 12: behind the watermark
                 (arrival (minutes 12) (pay 8 "p8" "c3" "m-food" 12 #:country "FR"))
                 (pay 20 "p7" "c3" "m-jewels" 2500 #:country "FR")
                 (pay 25 "p9" "c1" "m-bar" 80 #:country "ES")
                 (pay 26 "p10" "c1" "m-watches" 1500 #:country "ES")
                 ;; a chargeback: p7 never happened
                 (arrival (minutes 40) (retraction-of "p7"))))

;; Payments at the till arrive on their own source.
(source till #:types (Payment)
  #:from (events (pay 5 "q1" "c2" "m-food" 40 #:country "US" #:channel "pos")
                 (pay 30 "q2" "c3" "m-food" 15 #:country "FR" #:channel "pos")
                 (pay 33 "q3" "c3" "m-cafe" 22 #:country "FR" #:channel "pos")))

;; Baskets list what was bought.
(source baskets #:types (Basket)
  #:from (events (Basket #:at (minutes 26) #:id "b1" [card "c1"]
                         [items (list (hasheq 'sku "watch" 'price 1400) (hasheq 'sku "strap" 'price 100))])))

;; ST.merge: both channels, interleaved by arrival
(define-stream payments (merge web till))

;; ST.dedupe: a redelivered webhook carries the same ref within the hour
(define-stream unique (from web) (dedupe #:key ref #:horizon (hours 1)))

;; ST.reorder: in event-time order, released by the watermark; TM.lateness:
;; what arrives behind the watermark goes to a side stream
(define-stream ordered (from web) (lateness #:policy side #:side-output late-payments) (reorder))

;; ST.throttle: one payment in three, for a manual audit
(define-stream audit-sample (from payments) (throttle #:every 3))

;; OP.filter, OP.project, OP.map: large payments, reduced and in cents
(define-stream large (from payments) (filter (>= amount 1000)))
(define-stream slim (from large) (project card amount))
(define-stream in-cents (from large) (map Cents [card card] [cents (* 100 amount)]))

;; OP.enrich: merchant categories from a lookup table; unknown merchants kept
(define categories (hash "m-shoes" (hasheq 'category "fashion") "m-watches" (hasheq 'category "luxury")
                         "m-jewels" (hasheq 'category "luxury")))
(define-stream categorized (from payments)
  (enrich #:key merchant #:with categories #:adds ([category : String]) #:missing keep))

;; OP.split: one event per item bought
(define-stream items (from baskets)
  (split Item #:for ([i items]) [card card] [sku (hash-ref i 'sku)] [price (hash-ref i 'price)]
         #:ts (event-ts it)))

;; OP.route: web and till payments go their own way
(define-stream by-channel (from payments)
  (route [online (equal? channel "web")] #:otherwise in-person))

;; TM.timer: a payment settles five minutes after it is made
(define-stream settled (from unique) (delay (minutes 5)))

;; ---------------------------------------------------------------------------
;; Spending per card (§7, §8)

;; WN.sliding: each card's last 30 minutes, every 10 minutes
(define-stream spend-30m (from unique)
  (key-by card)
  (window (sliding #:size (minutes 30) #:slide (minutes 10)))
  (aggregate [payments (count)] [total (sum amount)] [mean (avg amount)]
             [smallest (min amount)] [largest (max amount)] [spread (stddev amount)]))

;; WN.session: a card's bursts of activity, separated by 10 quiet minutes
(define-stream sessions (from unique)
  (key-by card)
  (window (session #:gap (minutes 10)))
  (aggregate [payments (events)] [first-merchant (first merchant)] [last-merchant (last merchant)]
             [merchants (collect merchant)] [biggest (argmax amount)] [smallest (argmin amount)]))

;; WN.count with AG.incremental: after each payment, the last three, kept by
;; adding the new one and removing the oldest
(define-stream last-3 (from unique)
  (window (count #:size 3 #:step 1))
  (aggregate [total (sum amount)] [variance (variance amount)]))

;; holistic and approximate aggregates over the whole day's payments
(define-stream daily (from unique)
  (window (tumbling #:size (hours 24)))
  (aggregate [median (median amount)] [p90 (percentile amount 0.9)] [cards (count-distinct card)]
             [usual (mode merchant)] [top-2 (top-k amount 2)] [approx-cards (approx-distinct card)]))

;; ---------------------------------------------------------------------------
;; Context from other streams (§9)

(source logins #:types (Login)
  #:from (events (Login #:at (minutes 0) #:id "l1" [card "c1"] [country "PT"])
                 (Login #:at (minutes 9) #:id "l2" [card "c2"] [country "US"])
                 ;; c1 logs in from Brazil, then pays in Spain
                 (Login #:at (minutes 24) #:id "l3" [card "c1"] [country "BR"])))
(source declines #:types (Decline)
  #:from (events (Decline #:at (minutes 13) #:id "d1" [card "c2"] [reason "cvv"])
                 (Decline #:at (minutes 14) #:id "d2" [card "c2"] [reason "cvv"])
                 (Decline #:at (minutes 15) #:id "d3" [card "c2"] [reason "cvv"])
                 (Decline #:at (minutes 16) #:id "d4" [card "c2"] [reason "funds"])))
(source confirmations #:types (ThreeDS)
  #:from (events (ThreeDS #:at (minutes 21) #:id "s1" [card "c3"])))
(source cards #:types (CardInfo)
  #:from (events (CardInfo #:at 0 #:id "k1" [card "c1"] [holder "Ana"] [home "PT"])
                 (CardInfo #:at 0 #:id "k2" [card "c2"] [holder "Ben"] [home "US"])
                 (CardInfo #:at 0 #:id "k3" [card "c3"] [holder "Cy"] [home "FR"])))
(source limits #:types (Limit)
  #:from (events (Limit #:at 0 #:id "c1-1000" [card "c1"] [limit 1000])
                 (Limit #:at 0 #:id "c2-1000" [card "c2"] [limit 1000])
                 (Limit #:at 0 #:id "c3-3000" [card "c3"] [limit 3000])
                 (Limit #:at (minutes 20) #:id "c1-2000" [card "c1"] [limit 2000])))

;; JN.window: a payment and a login of the same card in the same quarter
;; hour, from different countries
(define-stream far-logins (from unique)
  (join logins #:kind window #:window (tumbling #:size (minutes 15)) #:key card
        #:on (not (equal? left.country right.country))
        #:select ([card left.card] [amount left.amount] [paid-in left.country] [logged-in right.country])))

;; JN.outer: web payments whose quarter hour has no login at all; a left
;; outer join keeps them, with no login country
(define-stream no-login (from unique)
  (join logins #:kind window #:window (tumbling #:size (minutes 15)) #:key card #:outer left
        #:select ([card left.card] [amount left.amount] [login-country right.country]))
  (filter (not login-country)))

;; JN.interval: a decline of the same card within ten minutes after a payment
(define-stream then-declined (from unique)
  (join declines #:kind interval #:lower 0 #:upper (minutes 10) #:key card
        #:select ([card left.card] [amount left.amount] [reason right.reason])))

;; JN.table: every payment with its card holder
(define-stream with-holder (from payments) (join cards #:kind table #:key card))

;; JN.as-of: every payment against the card's limit at the time it was made
(define-stream against-limit (from unique)
  (join limits #:kind as-of #:key card
        #:select ([card left.card] [amount left.amount] [limit right.limit])))
(define-stream over-limit (from against-limit) (filter (> amount limit)))


;; ---------------------------------------------------------------------------
;; Patterns (§11, §12)

(event-type Alert [card : String] [reason : String] #:derived)

;; everything a card does, in one stream: payments, declines, logins, and
;; 3-D Secure confirmations
(define-stream activity (merge unique till declines logins confirmations))

;; PT.absence: a large payment with no 3-D Secure confirmation within two
;; minutes (p5 and p10; c3 confirmed p7)
(define-pattern unconfirmed #:from activity #:key card
  (seq (p : Payment (>= amount 1000)) (absent ThreeDS #:within (minutes 2)))
  #:emit (Alert [card p.card] [reason "unconfirmed"]))

;; PT.conjunction: a login and a large payment of the same card, in either
;; order, within five minutes, from different countries
(define-pattern login-and-payment #:from activity #:key card #:within (minutes 5)
  (and (l : Login) (p : Payment (>= amount 1000)))
  #:where (not (equal? l.country p.country))
  #:emit (Alert [card p.card] [reason "far-login"]))

;; PT.disjunction: a decline for lack of funds, or a payment over 2000
(define-pattern funds-or-huge #:from activity #:key card
  (or (d : Decline (equal? reason "funds")) (p : Payment (> amount 2000))))

;; PT.any: two of three signals within a quarter hour
(define-pattern two-signals #:from activity #:key card #:within (minutes 15) #:output first
  (any 2 (p : Payment (>= amount 1000)) (l : Login (equal? country "BR")) (d : Decline)))

;; PT.kleene-star: a small payment, any declines, then a large payment
(define-pattern small-then-large #:from activity #:key card #:within (minutes 30)
  #:selection next #:output maximal
  (seq (a : Payment (< amount 50)) (kleene* (d : Decline)) (b : Payment (>= amount 1000))))

;; PT.repeat: three to five declines within ten minutes, reported once
(define-pattern decline-burst #:from activity #:key card #:within (minutes 10)
  #:output first #:skip past-last
  (repeat (d : Decline) 3 5))

;; PT.threshold: three payments within half an hour whose total crosses 2000
(define-pattern big-spree #:from activity #:key card #:within (minutes 30)
  #:selection next #:output first
  (repeat (p : Payment) 3)
  #:where (crosses? (sum p.amount) '> 2000))

;; Consumption (CON.*): which login explains a large payment
(define-syntax-rule (login-then-large name consumption)
  (define-pattern name #:from activity #:key card #:within (minutes 30) #:consumption consumption
    (seq (l : Login) (p : Payment (>= amount 1000)))))
(login-then-large any-login unrestricted)   ; c1: l1 and l3 with p10
(login-then-large latest-login recent)      ; c1: only l3, the most recent
(login-then-large oldest-login chronicle)   ; c1: only l1, the oldest, once
(login-then-large every-login continuous)   ; c1: both, consumed together

;; Output (OUT.first, OUT.last): after a small payment, the large ones
(define-pattern first-large #:from activity #:key card #:within (minutes 30) #:output first
  (seq (a : Payment (< amount 100)) (kleene+ (b : Payment (>= amount 500)))))
(define-pattern last-large #:from activity #:key card #:within (minutes 30) #:output last
  (seq (a : Payment (< amount 100)) (kleene+ (b : Payment (>= amount 500)))))

;; Skipping (SKIP.*): runs of declines, under each after-match skip strategy
(define-syntax-rule (decline-runs name skip)
  (define-pattern name #:from activity #:key card #:within (minutes 10) #:skip skip
    (kleene+ (d : Decline)) #:where (>= (count d) 2)))
(decline-runs runs-no-skip no-skip)
(decline-runs runs-to-next to-next)
(decline-runs runs-past-last past-last)
(decline-runs runs-to-first (to-first d))
(decline-runs runs-to-last (to-last d))

;; EVAL.immediate: a payment followed by a decline is reported as soon as
;; the decline arrives, without waiting for the watermark
(define-pattern declined-soon-after #:from activity #:key card #:within (minutes 5) #:eval immediate
  (seq (p : Payment) (d : Decline)))

;; CON.retain with OUT.timeout-partial: five declines make a lock-out; a run
;; that stops short is reported after ten minutes, and keeps counting
(define-pattern lockout #:from activity #:key card #:within (hours 1)
  #:selection next #:output first
  #:timeout-partial ((minutes 10) processing-time) #:retain (timeout)
  (kleene+ (d : Decline))
  #:where (>= (count d) 5))

;; ---------------------------------------------------------------------------
;; Derivation (§13)

(event-type Escalation [card : String] [reasons : Any] #:derived)

;; DR.closure: alerts are events too, and a pattern reads them: two alerts
;; of different reasons for one card within an hour escalate
(define-stream alerts (merge unconfirmed login-and-payment))
(define-pattern escalations #:from alerts #:key card #:within (hours 1)
  (and (a : Alert) (b : Alert))
  #:where (string<? a.reason b.reason)
  #:emit (Escalation [card a.card] [reasons (list a.reason b.reason)]))

;; DR.act: every escalation notifies the fraud desk; each notification is
;; recorded as an event
(define notified (box '()))
(act notify #:on escalations
  #:do (begin (set-box! notified (cons card (unbox notified))) 'sent)
  #:completion Notified)

;; DR.cycle-guard: a review of each escalation (level 3) is fed back and
;; reviewed once more (level 4); the cycle guard stops the loop there
(define-stream reviews #:recursive 4 (from escalations) (feedback reviews) (map [reviewed #t]))

(provide notified)
