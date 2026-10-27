#lang racket/base
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/time.rkt"
         "../private/event.rkt"
         "../private/allen.rkt")

(define (iv a b) (interval a b))

(spec-test '(AL.before AL.after) "a before b iff a⁺ < b⁻; after is its inverse"
  (check-true (before? (iv 1 2) (iv 3 4)))
  (check-true (after? (iv 3 4) (iv 1 2)))
  (check-false (before? (iv 1 3) (iv 3 4))))

(spec-test '(AL.meets AL.met-by) "a meets b iff a⁺ = b⁻"
  (check-true (meets? (iv 1 3) (iv 3 4)))
  (check-true (met-by? (iv 3 4) (iv 1 3))))

(spec-test '(AL.overlaps AL.overlapped-by) "a overlaps b iff a⁻ < b⁻ < a⁺ < b⁺"
  (check-true (overlaps? (iv 1 3) (iv 2 4)))
  (check-true (overlapped-by? (iv 2 4) (iv 1 3))))

(spec-test '(AL.starts AL.started-by) "a starts b iff a⁻ = b⁻ and a⁺ < b⁺"
  (check-true (starts? (iv 1 2) (iv 1 4)))
  (check-true (started-by? (iv 1 4) (iv 1 2))))

(spec-test '(AL.during AL.contains) "a during b iff b⁻ < a⁻ and a⁺ < b⁺"
  (check-true (during? (iv 2 3) (iv 1 4)))
  (check-true (contains? (iv 1 4) (iv 2 3))))

(spec-test '(AL.finishes AL.finished-by) "a finishes b iff a⁺ = b⁺ and b⁻ < a⁻"
  (check-true (finishes? (iv 2 4) (iv 1 4)))
  (check-true (finished-by? (iv 1 4) (iv 2 4))))

(spec-test 'AL.equals "equals is self-inverse"
  (check-true (equals? (iv 1 4) (iv 1 4)))
  (check-equal? (allen-inverse 'equals) 'equals))

(spec-test '(AL.before AL.starts AL.during AL.finishes AL.after AL.equals)
           "points: three point/point relations and five point/interval relations"
  (check-equal? (map (λ (p) (allen-relation (make-point p) (make-point 5))) '(4 5 6)) '(before equals after))
  (check-equal? (map (λ (p) (allen-relation (make-point p) (iv 2 6))) '(1 2 4 6 7))
                '(before starts during finishes after))
  ;; and the inverses, interval to point
  (check-equal? (map (λ (p) (allen-relation (iv 2 6) (make-point p))) '(1 2 4 6 7))
                '(after started-by contains finished-by before)))

(spec-test 'AL.during "relations apply to events through their occurrence intervals"
  (define shift (make-event 'Shift '() #:ts 8 #:te 17))
  (define alarm (make-event 'Alarm '() #:at 12))
  (check-true (during? alarm shift))
  (check-true (contains? shift alarm))
  (check-equal? (allen-relation '(1 . 2) '(2 . 9)) 'meets))

(define gen:proper
  (gen:let ([a (gen:integer-in 0 10)] [len (gen:integer-in 1 6)]) (iv a (+ a len))))

(spec-test '(AL.before AL.after AL.meets AL.met-by AL.overlaps AL.overlapped-by AL.starts AL.started-by
             AL.during AL.contains AL.finishes AL.finished-by AL.equals)
           "property: exactly one of the 13 relations holds, and swapping gives its inverse"
  (define preds (list before? after? meets? met-by? overlaps? overlapped-by? starts? started-by?
                      during? contains? finishes? finished-by? equals?))
  (check-property
   (make-config #:tests (trials))
   (property ([a gen:proper] [b gen:proper])
     (check-equal? (length (filter (λ (p) (p a b)) preds)) 1)
     (check-equal? (allen-relation b a) (allen-inverse (allen-relation a b)))
     (check-not-false (memq (allen-relation a b) relations)))))
