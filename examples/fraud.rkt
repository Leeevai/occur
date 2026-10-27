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
