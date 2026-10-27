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
