#lang racket/base
(require rackunit
         racket/runtime-path
         "util.rkt"
         "../private/time.rkt"
         "../private/event.rkt"
         "../private/operator.rkt"
         "../private/io.rkt")

(define-runtime-path swipes "data/swipes.jsonl")
(define Swipe (make-event-type 'Swipe '((card . String) (amount . Real))))
(define Verified (make-event-type 'Verified '((card . String))))
(define Place (make-event-type 'Place `((where . Location) (when . Time) (tags . ,(list-domain 'String)))))

(spec-test '(ES.stream ES.event) "JSON Lines become arrivals with processing times"
  (define as (read-jsonl-arrivals swipes (list Swipe Verified)))
  (check-equal? (length as) 4)
  (check-equal? (map arrival-at as) '(60 120 240 240))
  (define e1 (arrival-event (car as)))
  (check-equal? (list (event-id e1) (event-type e1) (event-ts e1) (event-attr e1 'amount)) '("e1" Swipe 60 2))
  (define v (arrival-event (caddr as)))
  (check-equal? (list (event-type v) (event-ts v)) '(Verified 210))
  (define b (arrival-event (cadddr as)))
  (check-equal? (list (event-ts b) (event-te b) (event-n b)) '(230 240 3)))

(spec-test 'ES.type "decoding follows the schema and rejects payloads outside D_τ"
  (define a (jsexpr->arrival (hasheq 'type "Place" 't 5 'where (hasheq 'x 1 'y 2)
                                     'when "2026-01-05T10:20:00Z" 'tags '("a" "b"))
                             (list Place)))
  (check-equal? (event-attr (arrival-event a) 'where) (location 1 2))
  (check-equal? (event-attr (arrival-event a) 'when) (utc 2026 1 5 10 20))
  (check-exn #rx"amount: \"x\" is not a Real"
             (λ () (jsexpr->arrival (hasheq 'type "Swipe" 't 1 'card "c" 'amount "x") (list Swipe))))
  (check-exn #rx"not declared"
             (λ () (jsexpr->arrival (hasheq 'type "Other" 't 1) (list Swipe))))
  (check-exn #rx"needs \"t\""
             (λ () (jsexpr->arrival (hasheq 'card "c" 'amount 1) (list Swipe)))))

(test-case "events and items print compactly"
  (define e (make-event 'Swipe '((card . "c1") (amount . 2)) #:at 60 #:n 2))
  (check-equal? (event->string e) "Swipe [60, 60] n=2 amount=2 card=\"c1\"")
  (check-equal? (item->string (retraction e)) "⊖ Swipe [60, 60] n=2 amount=2 card=\"c1\""))
