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
