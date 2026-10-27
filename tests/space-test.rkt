#lang racket/base
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/event.rkt")

(define Swipe (make-event-type 'Swipe '((card . String) (amount . Real))))
(define Fraud (make-event-type 'SuspectedFraud '((card . String) (n . Integer)) #:derived? #t))
(define Alert (make-event-type 'Alert '((level . Integer)) #:derived? #t))
(define T (make-type-registry (list Swipe Fraud Alert)))

(define (swipe id t amount) (make-event 'Swipe `((card . "c1") (amount . ,amount)) #:id id #:at t))

(spec-test 'ES.space "primitive events are level 0; derived events sit one level above their constituents"
  (define e1 (swipe "e1" 1 2))
  (define e2 (swipe "e2" 2 600))
  (define f (make-composite 'SuspectedFraud '((card . "c1") (n . 1)) (list e1 e2)))
  (define a (make-composite 'Alert '((level . 3)) (list f)))
  (check-equal? (map event-level (list e1 f a)) '(0 1 2))
  (check-true (andmap (λ (e) (event-space-member? e T)) (list e1 f a))))

(spec-test 'ES.space "membership follows the inductive definition"
  (define e1 (swipe "e1" 1 2))
  (check-regexp-match #rx"not in T" (event-space-problem (make-event 'Unknown '() #:at 1) T))
  (check-regexp-match #rx"payload not in D_Swipe"
                      (event-space-problem (make-event 'Swipe '((card . 7) (amount . 1)) #:at 1) T))
  (check-regexp-match #rx"primitive event has no constituents"
                      (event-space-problem (make-event 'Swipe '((card . "c") (amount . 1)) #:at 1
                                                       #:constituents (list e1)) T))
  (check-regexp-match #rx"needs constituents or a summary"
                      (event-space-problem (make-event 'Alert '((level . 1)) #:at 1) T))
  ;; a bad constituent makes the composite a non-member
  (define bad (make-event 'Swipe '((card . 1) (amount . 1)) #:id "bad" #:at 1))
  (check-regexp-match #rx"bad: payload"
                      (event-space-problem (make-composite 'Alert '((level . 1)) (list bad)) T)))

(spec-test 'ES.space "a summarized derived event is admitted at level 1"
  (define Out (make-event-type 'Out '((summary . Any)) #:derived? #t))
  (define o (make-event 'Out '((summary . ((samples . 3)))) #:ts 0 #:te 20))
  (check-true (event-space-member? o (make-type-registry (list Out))))
  (check-equal? (event-level o) 1))

(spec-test 'ES.space "the compact form forgets provenance; 𝓔 = 𝓔_P ⊎ 𝓔_D; 𝓔_τ restricts by type"
  (define e1 (swipe "e1" 1 2))
  (define f (make-composite 'SuspectedFraud '((card . "c1") (n . 1)) (list e1)))
  (check-equal? (erase-provenance f) (vector 'SuspectedFraud (event-payload f) 1 1 1))
  (define-values (p d) (split-primitive/derived (list e1 f)))
  (check-equal? (list (length p) (length d)) '(1 1))
  (check-equal? (restrict-to-type (list e1 f) 'Swipe) (list e1)))

;; Generate random event hierarchies and check that level is 1 + max level.
(define gen:tree
  (gen:let ([depth (gen:integer-in 0 3)] [width (gen:integer-in 1 3)])
    (let build ([d depth])
      (if (zero? d)
          (swipe (fresh-id) 1 2)
          (make-composite 'Alert '((level . 1)) (for/list ([i width]) (build (sub1 d))))))))

(spec-test 'ES.space "property: the abstraction level is the depth of the provenance tree"
  (check-property
   (make-config #:tests (trials))
   (property ([e gen:tree])
     (define (depth x) (if (null? (event-constituents x)) 0
                           (add1 (apply max (map depth (event-constituents x))))))
     (check-equal? (event-level e) (depth e))
     (check-true (event-space-member? e T)))))
