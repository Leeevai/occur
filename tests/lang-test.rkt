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
