#lang racket/base
(require rackunit
         rackcheck
         "util.rkt"
         "../private/time.rkt"
         "../private/event.rkt")

(define (batch ts te n) (make-batch-event 'Reading '() #:ts ts #:te te #:n n))

(spec-test 'ES.batch "share follows the three cases of the definition"
  (check-equal? (batch-share (batch 90 100 10) -inf.0) 10)      ; ts > b
  (check-equal? (batch-share (batch 90 100 10) 100) 0)          ; te <= b
  (check-equal? (batch-share (batch 90 100 0) 50) 0)            ; n = 0
  (check-equal? (batch-share (batch 95 105 10) 100) 5)          ; round(10 · 5/10)
  (check-equal? (batch-share-interval (batch 95 105 10) 100) (interval 100 105)))

(spec-test 'ES.batch "round is half-to-even and the share is at least 1 when straddling"
  ;; n · (te − b)/(te − ts) = 4 · 1/8 = 1/2 rounds to 0, then max(1, ·) = 1
  (check-equal? (batch-share (batch 0 8 4) 7) 1)
  ;; 5 · 5/10 = 5/2 rounds to 2 (half to even), not 3
  (check-equal? (batch-share (batch 0 10 5) 5) 2)
  ;; 7 · 5/10 = 7/2 rounds to 4
  (check-equal? (batch-share (batch 0 10 7) 5) 4)
  ;; at b = ts the boundary is not strictly before ts, so the third case applies
  (check-equal? (batch-share (batch 0 10 6) 0) 6))

(spec-test 'ES.batch "a sensor chunk of 50 readings is one batch event with n = 50"
  (define e (batch 0 49 50))
  (check-equal? (event-n e) 50)
  (check-equal? (batch-share e -inf.0) 50))

(define gen:batch
  (gen:let ([ts (gen:integer-in 0 100)]
            [len (gen:integer-in 0 50)]
            [n (gen:integer-in 0 60)])
    (batch ts (+ ts len) n)))

(spec-test 'ES.batch "property: 0 <= share <= n, and the share never grows as b advances"
  (check-property
   (make-config #:tests (trials))
   (property ([e gen:batch] [b1 (gen:integer-in -10 160)] [d (gen:integer-in 0 60)])
     (define s1 (batch-share e b1))
     (define s2 (batch-share e (+ b1 d)))
     (check-true (<= 0 s1 (event-n e)))
     (check-true (<= s2 s1))
     (check-equal? (batch-share e (event-te e)) 0)
     (check-equal? (batch-share e -inf.0) (event-n e)))))
