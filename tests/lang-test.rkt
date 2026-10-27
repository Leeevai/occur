#lang racket/base
(require rackunit
         racket/list
         "util.rkt"
         "../main.rkt"
         (only-in "lang/basic.rkt" [occur-run basic-run] [occur-program basic-program]))

(define (rendered r)
  (for/list ([o (run-result-outputs r)])
    (list (output-at o) (output-node o) (event-id (output-item o)))))

(spec-test '(ES.stream ES.event) "a #lang occur module runs its sources through its streams"
  (check-equal? (rendered (basic-run))
                '((60 all-events "e1") (60 swipes "e1")
                  (120 all-events "e2") (120 swipes "e2")
                  (240 all-events "v1"))))

(spec-test 'ES.type "event-type declares a runtime type; the type name builds checked events"
  (check-true (program? (basic-program))))

(define header "#lang occur\n(event-type Swipe [card : String] [amount : Real])\n")

(define-syntax-rule (check-compile-error src rx line)
  (let ([r (compile-error src)])
    (check-true (list? r) (format "expected a compile-time error for ~s" src))
    (when r
      (check-regexp-match rx (first r))
      (check-equal? (second r) line))))

(spec-test 'ES.type "duplicate attribute names are a compile-time error"
  (check-compile-error "#lang occur\n(event-type X [a : Real]\n  [a : String])\n"
                       #rx"duplicate attribute name" 3))

(spec-test 'ES.type "unknown value domains are a compile-time error"
  (check-compile-error "#lang occur\n(event-type X [a : Float])\n" #rx"unknown value domain" 2)
  (check-false (compile-error "#lang occur\n(event-type X [a : (Listof (Record [u : String]))] [b : Location])\n")))

(spec-test 'ES.space "unknown event types are a compile-time error"
  (check-compile-error (string-append header "(source feed #:types (Swipe\n Verified))\n")
                       #rx"unknown event type Verified" 4))

(spec-test 'ES.event "event constructors check attributes at compile time"
  (check-compile-error (string-append header "(define e (Swipe #:at 1 [card \"c\"]\n [amont 2]))\n")
                       #rx"unknown attribute amont for event type Swipe" 4)
  (check-compile-error (string-append header "(define e (Swipe #:at 1 [card \"c\"]))\n")
                       #rx"missing attribute amount" 3)
  (check-compile-error (string-append header "(define e (Swipe [card \"c\"] [amount 1]))\n")
                       #rx"needs a time" 3))

(spec-test 'ES.stream "sources and streams are checked against the types they carry"
  (check-compile-error
   (string-append header "(event-type V [card : String])\n(source feed #:types (Swipe)\n #:from (events (V #:at 1 [card \"c\"])))\n")
   #rx"type V is not declared by source feed" 5)
  (check-compile-error
   (string-append header "(event-type V [card : String])\n(source feed #:types (Swipe))\n(define-stream s (from feed V))\n")
   #rx"feed does not carry events of type V" 5)
  (check-compile-error
   (string-append header "(source feed #:types (Swipe) #:watermark sometimes)\n")
   #rx"watermark strategy" 3)
  (check-compile-error
   (string-append header "(source feed #:types (Swipe))\n(define-stream s (from feed) (sparkle))\n")
   #rx"unknown stage" 4)
  (check-compile-error
   (string-append header "(define-stream s (from nowhere))\n")
   #rx"not a declared source or stream" 3))

(test-case "a payload outside D_τ is rejected when the event is built"
  (check-exn #rx"payload not in D_Swipe"
             (λ () (eval-occur (string-append header "(define e (Swipe #:at 1 [card 7] [amount 1]))\n")))))

(require (only-in "lang/scripted.rkt" [occur-steps scripted-steps] [occur-run scripted-run]
                  [occur-program scripted-program]))

(spec-test '(TM.processing-time ES.stream) "a script replaces the data; processing time is simulated"
  (define steps (scripted-steps))
  (check-equal? (map (λ (s) (cond [(step:arrive? s) 'arrive] [(step:advance? s) 'advance]
                                  [(step:watermark? s) 'watermark] [(step:suspend? s) 'suspend]))
                     steps)
                '(arrive advance watermark arrive suspend))
  (define r (scripted-run))
  (check-equal? (rendered r) '((5 verified "v1")))
  (check-equal? (run-result-now r) 90))

(spec-test 'ES.stream "a JSON Lines source is read relative to its module"
  (define p (scripted-program))
  (define src (hash-ref (program-sources p) 'feed))
  (check-equal? (length ((source-data src))) 4)
  (check-equal? (source-watermark src) '(bounded-delay 30)))

(require (only-in "lang/ops.rkt" [occur-run ops-run]))

(define (by-node r node) (run-result-events r node))

(spec-test '(OP.filter OP.project OP.map OP.enrich OP.split OP.route)
           "the single-event stages run as declared"
  (define r (ops-run))
  (check-equal? (map event-id (by-node r 'big)) '("a"))
  (check-equal? (map event-payload (by-node r 'slim))
                (list (hasheq 'card "c1" 'amount 1500) (hasheq 'card "c2" 'amount 3)))
  (check-equal? (map (λ (e) (list (event-type e) (event-attr e 'cents))) (by-node r 'charges))
                '((Charge 150000) (Charge 300)))
  (check-equal? (map (λ (e) (list (event-attr e 'amount) (event-attr e 'flag))) (by-node r 'doubled))
                '((3000 #t) (6 #t)))
  (check-equal? (map (λ (e) (event-attr e 'category)) (by-node r 'categorized)) '("grocery"))
  (check-equal? (map (λ (e) (list (event-attr e 'sensor) (event-n e) (event-ts e) (event-te e)))
                     (by-node r 'readings))
                '(("s1" 3 0 2) ("s2" 5 1 9)))
  (check-equal? (map event-id (by-node r 'large)) '("a"))
  (check-equal? (map event-id (by-node r 'small)) '("b"))
  (check-equal? (by-node r 'by-size) '()))

(define ops-header
  (string-append header "(source feed #:types (Swipe))\n"))

(spec-test 'OP.project "projecting an attribute the stream does not carry is a compile-time error"
  (check-compile-error (string-append ops-header "(define-stream s (from feed)\n (project card zip))\n")
                       #rx"unknown attribute zip: Swipe events carry \\(card amount\\)" 5))

(spec-test 'OP.map "a typed map must give exactly the type's attributes"
  (check-compile-error (string-append ops-header "(event-type C [card : String] [cents : Integer])\n"
                                      "(define-stream s (from feed)\n (map C [card card]))\n")
                       #rx"missing attribute cents" 6)
  (check-compile-error (string-append ops-header "(event-type C [card : String])\n"
                                      "(define-stream s (from feed)\n (map C [card card] [zip 1]))\n")
                       #rx"unknown attribute zip for event type C" 6))

(spec-test 'OP.split "a split child needs a time"
  (check-compile-error (string-append ops-header
                                      "(define-stream s (from feed)\n (split Swipe #:for ([i 3]) [card card] [amount i]))\n")
                       #rx"each child needs #:ts" 5))

(spec-test 'OP.route "route must be the last stage"
  (check-compile-error (string-append ops-header
                                      "(define-stream s (from feed)\n (route [a #t])\n (filter #t))\n")
                       #rx"must be the last stage" 5))

(spec-test 'OP.filter "stage expressions see the attributes of the shape"
  (check-compile-error (string-append ops-header "(define-stream s (from feed) (filter (> amont 1)))\n")
                       #rx"amont: unbound identifier" 4))

(require (only-in "lang/structure.rkt" [occur-run structure-run] [occur-program structure-program]))

(spec-test '(ST.monotonic-merge ES.substream) "#:merge high-water-mark merges per (entity, sub-stream)"
  (define r (structure-run))
  (check-equal? (map (λ (e) (list (event-attr e 'owner) (event-ts e) (event-te e) (event-n e)))
                     (by-node r 'contributions))
                '(("d1" 90 100 10) ("d1" 100 105 5) ("d2" 80 90 4)))
  (check-equal? (run-result-counter r 'arrivals/merge 'late) 25))

(spec-test '(ST.dedupe ST.merge ST.key-by ST.throttle ST.reorder) "structure stages run as declared"
  (define r (structure-run))
  (define (pid node) (map (λ (e) (event-attr e 'id)) (by-node r node)))
  (check-equal? (pid 'unique) '("a" "b" "a"))
  (check-equal? (pid 'both) '("a" "a" "b" "z" "a"))
  (check-equal? (pid 'by-id) '("a"))          ; the second "a" of key a; b's first is dropped
  (check-equal? (pid 'sampled) '("a" "a"))
  (check-equal? (pid 'ordered) '("a" "a" "b" "a")))

(define st-header
  (string-append "#lang occur\n(event-type P [id : String])\n(source pings #:types (P))\n"))

(spec-test 'TM.clock "unknown clocks are a compile-time error"
  (check-compile-error (string-append st-header "(define-stream s (from pings)\n (dedupe #:horizon 5 #:clock wall))\n")
                       #rx"unknown clock" 5))

(spec-test 'ES.substream "source keying attributes are checked against the source's types"
  (check-compile-error "#lang occur\n(event-type P [id : String])\n(source s #:types (P)\n #:key owner)\n"
                       #rx"unknown attribute owner" 4)
  (check-compile-error "#lang occur\n(event-type P [id : String])\n(source s #:types (P) #:key id\n #:merge high-water-mark)\n"
                       #rx"needs #:substream" 4))

(require (only-in "lang/progress.rkt" [occur-run progress-run]))

(spec-test '(TM.keyed-watermark TM.skew-guard TM.lateness TM.timer TM.idle TM.schedule TM.watermark)
           "time and progress forms run as declared"
  (define r (progress-run))
  (check-equal? (map (λ (e) (event-attr e 'watermark)) (by-node r 'progress)) '(50 70))
  (check-equal? (run-result-counter r 'arrivals/skew-guard 'future-skew) 1)
  (check-equal? (map (λ (e) (event-attr e 'card)) (by-node r 'on-time)) '("a"))
  (check-equal? (map (λ (e) (event-attr e 'card)) (by-node r 'late-swipes)) '("late"))
  ;; the heartbeat moved W to 100, which released a (10 + 5); the late swipe's
  ;; timer (40 + 5) is already behind the watermark, so it fires on arrival
  (check-equal? (map (λ (o) (list (output-at o) (event-attr (output-item o) 'card)))
                     (filter (λ (o) (eq? (output-node o) 'delayed)) (run-result-outputs r)))
                '((20 "a") (30 "late")))
  (check-equal? (map output-at (filter (λ (o) (eq? (output-node o) 'quiet)) (run-result-outputs r))) '(602))
  (check-equal? (map (λ (e) (event-attr e 'ticks)) (by-node r 'hourly)) '(1)))

(spec-test 'TM.schedule "a malformed cron expression is a compile-time error"
  (check-compile-error "#lang occur\n(define-schedule s\n \"0 25 * * *\")\n" #rx"out of range" 3))

(require (only-in "lang/retract.rkt" [occur-run retract-run]))

(spec-test 'TM.retraction "⊖e in a source cancels the events derived from e"
  (define items (map output-item (run-result-outputs (retract-run))))
  (check-equal? (length items) 3)
  (check-true (retraction? (third items)))
  (check-equal? (event-attr (retraction-event (third items)) 'card) "c1"))

(require (only-in "lang/windows.rkt" [occur-run windows-run]))

(define (spans r node) (map (λ (e) (list (event-ts e) (event-te e) (event-attr e 'count))) (by-node r node)))

(spec-test '(WN.tumbling WN.sliding WN.session WN.count WN.landmark WN.punctuated WN.since-last WN.empty)
           "window stages run as declared"
  (define r (windows-run))
  (check-equal? (spans r 'per-10) '((0 10 2) (10 20 1) (30 40 3)))
  (check-equal? (take (spans r 'every-5) 3) '((-5 5 1) (0 10 2) (5 15 2)))
  (check-equal? (map (λ (e) (list (event-attr e 'key) (event-ts e) (event-te e))) (by-node r 'sessions))
                ;; delivered grouped by key within the final step (DR.ordered)
                '(("c1" 1 15) ("c1" 31 45) ("c2" 12 22)))
  (check-equal? (spans r 'last-3) '((1 12 3) (31 35 3)))
  (check-equal? (length (by-node r 'since-zero)) 6)
  (check-equal? (map (λ (e) (list (event-ts e) (event-te e) (event-attr e 'reason))) (by-node r 'shifts))
                '((31 35 closed)))
  (check-equal? (spans r 'batches) '((1 5 2) (12 31 2) (33 35 2)))
  (check-equal? (spans r 'empties) '((0 10 2) (10 20 1) (20 30 0) (30 40 3))))

(spec-test '(CX.entity CX.activation CX.spatial CX.composite CX.state CX.segmentation CX.temporal)
           "context stages and directories run as declared"
  (define r (windows-run))
  (check-equal? (map (λ (e) (event-attr e 'device)) (by-node r 'seen)) '("d1"))
  (check-equal? (run-result-counter r 'readings/directory 'ignored) 1)
  (check-equal? (map (λ (e) (event-attr e 'key)) (by-node r 'activated)) '("d1"))
  (check-equal? (map (λ (e) (event-attr e 'device)) (by-node r 'near-depot)) '("d1"))
  (check-equal? (event-attr (car (by-node r 'by-hour)) 'context) '((segment "c1") (sliding 0 3600)))
  (check-equal? (map event-ts (by-node r 'during)) '(12)))

(spec-test 'WN.calendar "calendar windows check their cron expression at compile time"
  (check-compile-error (string-append header "(source feed #:types (Swipe))\n(define-stream s (from feed)\n (window (calendar \"0 0 * * 9\")))\n")
                       #rx"out of range" 5))

(spec-test 'CX.entity "#:directory needs a declared directory and a #:key"
  (check-compile-error (string-append header "(source feed #:types (Swipe) #:key card\n #:directory nope)\n")
                       #rx"not a declared directory" 4))

(require (only-in "lang/aggregate.rkt" [occur-run aggregate-run]))

(spec-test '(AG.sum AG.count AG.percentile AG.avg AG.variance AG.count-distinct AG.top-k AG.mode AG.median
             AG.first AG.last AG.argmax AG.span AG.approx)
           "aggregate stages fuse with windows or run per key"
  (define r (aggregate-run))
  (check-equal? (map (λ (e) (list (event-attr e 'key) (event-ts e) (event-te e) (event-attr e 'total) (event-attr e 'n)))
                     (by-node r 'spend))
                '(("c1" -300 300 40 2) ("c1" 0 600 90 3) ("c1" 300 900 50 1) ("c2" -300 300 5 1) ("c2" 0 600 5 1)))
  (check-equal? (map (λ (e) (event-attr e 'total)) (by-node r 'running)) '(10 40 5 90))
  (define st (car (by-node r 'stats)))
  (check-equal? (event-attr st 'mean) 95/4)
  (check-equal? (event-attr st 'cards) 2)
  (check-equal? (event-attr st 'top) '(50 30))
  (check-equal? (event-attr st 'busiest) "c1")
  (check-equal? (event-attr st 'middle) 20)
  (check-equal? (list (event-attr st 'first-card) (event-attr st 'last-card)) '("c1" "c1"))
  (check-equal? (event-attr (event-attr st 'biggest) 'amount) 50)
  (check-equal? (event-attr st 'span) 370)
  (check-equal? (event-attr st 'approx-cards) 2))

(spec-test 'AG.count "unknown aggregates are compile-time errors"
  (check-compile-error (string-append header "(source feed #:types (Swipe))\n(define-stream s (from feed)\n (aggregate [x (sumx amount)]))\n")
                       #rx"unknown aggregate" 5))

(require (only-in "lang/joins.rkt" [occur-run joins-run]))

(spec-test '(JN.window JN.interval JN.outer JN.table JN.as-of) "join stages run as declared"
  (define r (joins-run))
  (check-equal? (map (λ (e) (list (event-attr e 'account) (event-attr e 'amount) (event-attr e 'paid)))
                     (by-node r 'settled))
                '(("a" 5 5)))
  (check-equal? (map (λ (e) (event-payload e)) (by-node r 'nearby))
                (list (hasheq 'account "a" 'gap 0) (hasheq 'account "a" 'gap -8)))
  (check-equal? (map (λ (e) (list (event-type e) (event-attr e 'account) (event-attr e 'paid))) (by-node r 'unpaid))
                ;; the order at 30 shares its window with the payment at 31, so only b is unpaid
                '((Unpaid "a" 5) (Unpaid "a" 1) (Unpaid "b" #f)))
  (check-equal? (map (λ (e) (event-attr e 'tier)) (by-node r 'with-tier)) '("gold" "platinum"))
  (check-equal? (map (λ (e) (list (event-attr e 'account) (event-attr e 'tier))) (by-node r 'tier-then))
                ;; the final step's outputs are grouped by key, b first (DR.ordered)
                '(("b" #f) ("a" "gold") ("a" "platinum"))))

(spec-test 'JN.window "join keys are checked against both sides"
  (check-compile-error
   (string-append header "(event-type P [card : String])\n(source a #:types (Swipe))\n(source b #:types (P))\n"
                  "(define-stream s (from a)\n (join b #:kind table #:key amount))\n")
   #rx"unknown attribute amount: P events carry" 7))
