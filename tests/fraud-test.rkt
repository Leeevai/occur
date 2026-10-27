#lang racket/base
;; examples/fraud.rkt: card payments from ingestion to alerts.
(require rackunit
         racket/list
         "util.rkt"
         "../main.rkt"
         (prefix-in fraud: "../examples/fraud.rkt"))

(define r (fraud:occur-run))
(define (outs node) (filter (λ (o) (eq? (output-node o) node)) (run-result-outputs r)))
(define (events node) (filter event? (map output-item (outs node))))
(define (ids node) (map event-id (events node)))
(define (retracted node)
  (for/list ([o (outs node)] #:when (retraction? (output-item o))) (event-id (retraction-event (output-item o)))))
(define (attrs node . as) (for/list ([e (events node)]) (map (λ (a) (event-attr/default e a #f)) as)))
(define (m n) (minutes n))

;; --- ingestion -------------------------------------------------------------

(spec-test '(ST.merge ES.stream ES.operator) "both channels merge in arrival order"
  (check-equal? (ids 'payments) '("p1" "p2" "p3" "p4" "p4-again" "q1" "p5" "p6" "p8" "p7" "p9" "p10" "q2" "q3")))

(spec-test 'ST.dedupe "a redelivered webhook is dropped"
  (check-false (member "p4-again" (ids 'unique)))
  (check-equal? (run-result-counter r 'unique 'duplicates) 1))

(spec-test '(TM.lateness ST.reorder TM.event-time TM.watermark) "late payments go to a side stream; the rest leave in event-time order"
  (check-equal? (ids 'late-payments) '("p8"))
  (check-equal? (map event-ts (events 'ordered)) (sort (map event-ts (events 'ordered)) <)))

(spec-test 'ST.throttle "one payment in three is sampled"
  (check-equal? (ids 'audit-sample) '("p3" "q1" "p8" "p10")))

(spec-test '(OP.filter OP.project OP.map) "large payments, projected and in cents"
  (check-equal? (ids 'large) '("p5" "p7" "p10"))
  (check-equal? (map event-payload (events 'slim))
                (list (hasheq 'card "c2" 'amount 1200) (hasheq 'card "c3" 'amount 2500) (hasheq 'card "c1" 'amount 1500)))
  (check-equal? (attrs 'in-cents 'cents) '((120000) (250000) (150000))))

(spec-test 'OP.enrich "merchant categories are added where the lookup knows the merchant"
  (check-equal? (take (attrs 'categorized 'merchant 'category) 3)
                '(("m-shoes" "fashion") ("m-shoes" "fashion") ("m-books" #f))))

(spec-test '(OP.split ES.batch) "a basket splits into one event per item"
  (check-equal? (attrs 'items 'sku 'price) '(("watch" 1400) ("strap" 100))))

(spec-test 'OP.route "web and till payments are routed apart"
  (check-equal? (ids 'in-person) '("q1" "q2" "q3"))
  (check-equal? (length (events 'online)) 11))

(spec-test '(TM.timer TM.event-time) "a payment settles five minutes after it is made, by event time"
  (check-equal? (take (map output-at (outs 'settled)) 3) (list (m 10) (m 10) (m 10)))
  (check-equal? (take (ids 'settled) 3) '("p1" "p2" "p3")))

(spec-test '(TM.retraction DR.retract) "the chargeback cancels p7 and everything derived from it"
  (check-equal? (retracted 'large) '("p7"))
  (check-equal? (length (retracted 'in-cents)) 1)
  ;; p7 was still waiting to settle: it never does
  (check-false (member "p7" (ids 'settled))))

;; --- spending per card ---------------------------------------------------

(spec-test '(WN.sliding AG.count AG.sum AG.avg AG.min AG.max AG.variance ES.keyed)
           "each card's spending over a sliding half hour"
  (define c1 (for/list ([e (events 'spend-30m)] #:when (equal? (event-attr e 'key) "c1"))
               (list (event-ts e) (event-te e) (event-attr e 'payments) (event-attr e 'total)
                     (event-attr e 'smallest) (event-attr e 'largest))))
  (check-equal? c1 (list (list -1200 (m 10) 4 1552 2 900) (list -600 (m 20) 4 1552 2 900)
                         (list 0 (m 30) 6 3132 2 1500) (list (m 10) (m 40) 2 1580 80 1500)
                         (list (m 20) (m 50) 2 1580 80 1500)))
  ;; the half hour from midnight: six payments of c1, averaging 522
  (check-equal? (for/first ([e (events 'spend-30m)] #:when (and (equal? (event-attr e 'key) "c1") (= (event-ts e) 0)))
                  (event-attr e 'mean))
                522))

(spec-test '(WN.session AG.events AG.first AG.last AG.collect AG.argmax AG.argmin)
           "a card's bursts of activity, with what they contain"
  (define first-c1 (findf (λ (e) (equal? (event-attr e 'key) "c1")) (events 'sessions)))
  (check-equal? (list (event-ts first-c1) (event-te first-c1)) (list (m 1) (m 14)))
  (check-equal? (map (λ (a) (event-attr first-c1 a)) '(payments first-merchant last-merchant merchants))
                '(4 "m-shoes" "m-shoes" ("m-shoes" "m-shoes" "m-books" "m-shoes")))
  (check-equal? (map (λ (a) (event-id (event-attr first-c1 a))) '(biggest smallest)) '("p4" "p1")))

(spec-test '(WN.count AG.incremental) "after each payment, the last three"
  (check-equal? (take (attrs 'last-3 'total) 3) '((652) (1550) (2150))))

(spec-test '(AG.median AG.percentile AG.count-distinct AG.mode AG.top-k AG.approx)
           "the day's holistic and approximate aggregates, without the charged-back payment"
  (check-equal? (attrs 'daily 'median 'p90 'cards 'usual 'top-2 'approx-cards)
                '((80 1500 3 "m-shoes" (1500 1200) 3))))

;; --- joins -----------------------------------------------------------------

(spec-test 'JN.window "a login from another country in the same quarter hour"
  (check-equal? (map event-payload (events 'far-logins))
                (list (hasheq 'card "c1" 'amount 80 'paid-in "ES" 'logged-in "BR")
                      (hasheq 'card "c1" 'amount 1500 'paid-in "ES" 'logged-in "BR"))))

(spec-test 'JN.outer "payments with no login in their quarter hour"
  (check-equal? (map event-payload (events 'no-login)) (list (hasheq 'card "c3" 'amount 12 'login-country #f))))

(spec-test 'JN.interval "declines within ten minutes after a payment"
  (check-equal? (remove-duplicates (attrs 'then-declined 'amount 'reason))
                '((1200 "cvv") (30 "cvv") (1200 "funds") (30 "funds"))))

(spec-test 'JN.table "every payment with its card holder"
  (check-equal? (remove-duplicates (attrs 'with-holder 'card 'holder))
                '(("c1" "Ana") ("c2" "Ben") ("c3" "Cy"))))

(spec-test 'JN.as-of "each payment against the limit in force when it was made"
  (check-equal? (attrs 'over-limit 'card 'amount 'limit) '(("c2" 1200 1000)))
  ;; c1's limit rose to 2000 at minute 20, before its 1500 payment
  (check-equal? (last (filter (λ (x) (equal? (car x) "c1")) (attrs 'against-limit 'card 'amount 'limit)))
                '("c1" 1500 2000)))

;; --- patterns ----------------------------------------------------------

;; the constituents of each match of a pattern node, as id lists
(define (matches node) (map (λ (e) (map event-id (event-constituents e))) (events node)))

(spec-test '(PT.absence DR.derive) "large payments with no 3-D Secure confirmation within two minutes"
  (check-equal? (sort (attrs 'unconfirmed 'card 'reason) string<? #:key car)
                '(("c1" "unconfirmed") ("c2" "unconfirmed"))))

(spec-test '(PT.conjunction PT.where) "a login and a large payment in either order, from different countries"
  (check-equal? (matches 'login-and-payment) '(("l3" "p10"))))

(spec-test '(PT.disjunction DR.retract) "a decline for funds, or a payment over 2000 (later charged back)"
  (check-equal? (matches 'funds-or-huge) '(("d4") ("p7")))
  (check-equal? (length (retracted 'funds-or-huge)) 1))

(spec-test '(PT.any OUT.first) "two of three signals, the first per initiator"
  (check-equal? (matches 'two-signals) '(("p5" "d1") ("l3" "p10"))))

(spec-test '(PT.kleene-star SEL.next OUT.maximal) "a small payment, any number of declines, then a large one"
  (check-equal? (sort (matches 'small-then-large) string<? #:key car) '(("p1" "p10") ("q1" "p5"))))

(spec-test '(PT.repeat SKIP.past-last OUT.first) "three to five declines, reported once"
  (check-equal? (matches 'decline-burst) '(("d1" "d2" "d3"))))

(spec-test '(PT.threshold PT.repeat SEL.next) "three payments in a row whose total crosses 2000"
  (check-equal? (car (matches 'big-spree)) '("p4" "p9" "p10")))

(spec-test '(CON.unrestricted CON.recent CON.chronicle CON.continuous) "which login explains c1's large payment"
  (define (c1 node) (filter (λ (m) (member "p10" m)) (matches node)))
  (check-equal? (c1 'any-login) '(("l1" "p10") ("l3" "p10")))
  (check-equal? (c1 'latest-login) '(("l3" "p10")))
  (check-equal? (c1 'oldest-login) '(("l1" "p10")))
  (check-equal? (c1 'every-login) '(("l1" "p10") ("l3" "p10"))))

(spec-test '(OUT.first OUT.last EVAL.deferred) "after a small payment, the first large one or all of them"
  ;; c3's match with p7 is reported, then retracted with the chargeback
  (check-equal? (matches 'first-large) '(("p1" "p2") ("p3" "p4") ("q1" "p5") ("p8" "p7") ("p9" "p10")))
  (check-equal? (length (retracted 'first-large)) 1)
  ;; last output waits for the bound, after the chargeback: c3's never appears
  (check-equal? (matches 'last-large) '(("p1" "p2" "p4" "p10") ("p3" "p4" "p10") ("p9" "p10") ("q1" "p5"))))

(spec-test '(SKIP.no-skip SKIP.to-next SKIP.past-last SKIP.to-first SKIP.to-last PT.kleene-plus)
           "runs of at least two declines, under each skip strategy"
  (check-equal? (length (matches 'runs-no-skip)) 11)
  (check-equal? (matches 'runs-to-first) (matches 'runs-no-skip))      ; to-first d discards nothing here
  (check-equal? (matches 'runs-to-next) '(("d1" "d2") ("d2" "d3") ("d3" "d4")))
  (check-equal? (matches 'runs-to-last) '(("d1" "d2") ("d2" "d3") ("d3" "d4")))
  (check-equal? (matches 'runs-past-last) '(("d1" "d2") ("d3" "d4"))))

(spec-test 'EVAL.immediate "a payment then a decline, reported when the decline arrives"
  (check-equal? (for/list ([o (outs 'declined-soon-after)]) (output-at o))
                (list (m 13) (m 13) (m 14) (m 14) (m 15) (m 15) (m 16))))

(spec-test '(CON.retain OUT.timeout-partial PT.flush) "four declines time out as a partial lock-out, retained"
  (check-equal? (for/list ([o (outs 'lockout)])
                  (list (output-at o) (event-attr (output-item o) 'reason) (map event-id (event-constituents (output-item o)))))
                (list (list (m 23) 'timeout '("d1" "d2" "d3" "d4")))))

;; --- derivation --------------------------------------------------------

(spec-test '(DR.closure DR.derive ES.composite ES.space) "a pattern over derived alerts escalates a card"
  (check-equal? (attrs 'escalations 'card 'reasons) '(("c1" ("far-login" "unconfirmed"))))
  ;; the escalation is two levels above the payments
  (check-equal? (map event-level (events 'escalations)) '(2)))

(spec-test 'DR.act "each escalation notifies the fraud desk, with a completion event"
  (check-equal? (unbox fraud:notified) '("c1"))
  (check-equal? (attrs 'notify 'result) '((sent))))

(spec-test 'DR.cycle-guard "a review fed back on itself stops at the cycle guard's bound"
  (check-equal? (map event-level (events 'reviews)) '(3 4))
  (check-equal? (for/sum ([c (in-hash-values (run-result-counters r))]) (hash-ref c 'recursion-bound 0)) 1))
