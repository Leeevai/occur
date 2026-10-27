#lang racket/base
(require rackunit
         "util.rkt"
         "../private/time.rkt"
         "../private/event.rkt")

(define Swipe (make-event-type 'Swipe '((card . String) (amount . Real))))
(define Heartbeat (make-event-type 'Heartbeat '()))
(define Fraud (make-event-type 'SuspectedFraud '((card . String) (n . Integer) (total . Real))
                               #:derived? #t))

(spec-test 'ES.type "a type has a name, a schema of distinct attributes, and a kind"
  (check-equal? (event-type-name Swipe) 'Swipe)
  (check-equal? (event-type-attributes Swipe) '(card amount))
  (check-equal? (event-type-domain Swipe 'amount) 'Real)
  (check-false (event-type-domain Swipe 'merchant))
  (check-true (event-type-primitive? Swipe))
  (check-true (event-type-derived? Fraud))
  (check-equal? (event-type-attributes Heartbeat) '()))

(spec-test 'ES.type "duplicate attribute names and unknown domains are rejected"
  (check-exn #rx"duplicate attribute" (λ () (make-event-type 'Bad '((a . Real) (a . String)))))
  (check-exn #rx"unknown value domain" (λ () (make-event-type 'Bad '((a . Float))))))

(spec-test 'ES.type "payload domains cover base, record, and list domains"
  (define Reading (make-event-type 'Reading
                                   (list (cons 'at 'Time)
                                         (cons 'every 'Duration)
                                         (cons 'where 'Location)
                                         (cons 'ok 'Boolean)
                                         (cons 'count 'Integer)
                                         (cons 'meta (record-domain '((unit . String))))
                                         (cons 'values (list-domain 'Real)))))
  (define good (hasheq 'at 10 'every 5 'where (location 1 2) 'ok #t 'count 3
                       'meta (hasheq 'unit "C") 'values '(1.5 2)))
  (check-true (payload-valid? Reading good))
  (check-regexp-match #rx"count" (payload-problem Reading (hash-set good 'count 1.5)))
  (check-regexp-match #rx"missing attribute meta" (payload-problem Reading (hash-remove good 'meta)))
  (check-regexp-match #rx"unknown attribute" (payload-problem Reading (hash-set good 'extra 1)))
  (check-true (payload-valid? Heartbeat (hasheq)))
  (check-equal? (domain->string (list-domain (record-domain '((a . Real))))) "(Listof (Record [a : Real]))"))

(spec-test 'ES.type "a registry is the finite set T of types"
  (define T (make-type-registry (list Swipe Fraud)))
  (check-true (type-registry? T))
  (check-eq? (hash-ref T 'Swipe) Swipe))

(spec-test 'ES.event "an instance carries id, type, payload, interval, multiplicity, source, constituents"
  (define e (make-event 'Swipe '((card . "c1") (amount . 2)) #:id "e1" #:at 60 #:src 'feed))
  (check-equal? (event-id e) "e1")
  (check-equal? (event-type e) 'Swipe)
  (check-equal? (event-attr e 'amount) 2)
  (check-equal? (event-ts e) 60)
  (check-equal? (event-te e) 60)
  (check-equal? (event-t e) 60)
  (check-equal? (event-n e) 1)
  (check-equal? (event-src e) 'feed)
  (check-true (event-primitive? e))
  (check-true (event-point? e))
  (check-equal? (event-interval e) (interval 60 60))
  (check-exn #rx"no such attribute" (λ () (event-attr e 'merchant)))
  (check-equal? (event-attr/default e 'merchant 'none) 'none))

(spec-test 'ES.event "the interval must satisfy ts <= te and n must be natural"
  (check-exn #rx"ts <= te" (λ () (make-event 'X '() #:ts 5 #:te 4)))
  (check-exn exn:fail? (λ () (make-event 'X '() #:at 1 #:n -1)))
  (check-exn #rx"needs a time" (λ () (make-event 'X '())))
  (check-exn #rx"point events" (λ () (event-t (make-event 'X '() #:ts 1 #:te 2)))))

(spec-test 'ES.event "ids are fresh and deterministic under call-with-fresh-ids"
  (define ids (call-with-fresh-ids
               (λ () (map event-id (list (make-event 'X '() #:at 1) (make-event 'X '() #:at 2))))))
  (check-equal? ids '("e1" "e2"))
  (check-equal? (call-with-fresh-ids (λ () (event-id (make-event 'X '() #:at 1)))) "e1"))

(spec-test 'ES.event "functional update and printing"
  (define e (make-event 'Swipe (hasheq 'card "c1") #:id "e1" #:at 1))
  (check-equal? (event-attr (event-set-attr e 'card "c2") 'card) "c2")
  (check-equal? (event-te (event-update e #:te 9)) 9)
  (check-regexp-match #rx"#<event e1 Swipe \\[1, 1\\] card=\"c1\">" (format "~a" e)))
