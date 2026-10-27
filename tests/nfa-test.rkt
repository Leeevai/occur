#lang racket/base
;; The incremental engine against the reference oracle.
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "pattern-gen.rkt"
         "../private/event.rkt"
         "../private/pattern/ast.rkt"
         "../private/pattern/reference.rkt"
         "../private/pattern/nfa.rkt")

;; Run the engine over events in the given arrival order. With ascending?,
;; the watermark follows the newest end time after each arrival; at the end
;; it goes to +∞.
(define (engine-matches p arrivals #:activation [t0 0] #:ascending? [ascending? #f])
  (define-values (e0 d0) (engine-start (make-engine p #:activation t0)))
  (define-values (e1 acc)
    (for/fold ([e e0] [acc d0]) ([ev arrivals] [i (in-naturals)])
      (define-values (e2 d) (engine-event e ev i))
      (if ascending?
          (let-values ([(e3 d2) (engine-advance e2 (event-te ev) 0)]) (values e3 (append acc d d2)))
          (values e2 (append acc d)))))
  (define-values (e3 dfinal) (engine-advance e1 +inf.0 +inf.0))
  (append acc dfinal))

(define (keys ms) (sort (map pmatch-key ms) (λ (a b) (string<? (format "~s" a) (format "~s" b)))))

(define (ev id type t [payload '()]) (make-event type payload #:id id #:at t))

(test-case "the engine reproduces the oracle on the card-testing pattern of §15"
  (define a (prim 'Swipe #:bind 'a #:when (λ (e) (< (event-attr e 'amount) 5))))
  (define b (kleene+ (prim 'Swipe #:bind 'b #:when (λ (e) (> (event-attr e 'amount) 500)))))
  (define p (within (neg-between a (prim 'Verified) b) 600))
  (define H (for/list ([amt '(2 600 50 900)] [i (in-naturals 1)]) (ev (format "e~a" i) 'Swipe (* 60 i) `((amount . ,amt)))))
  (check-equal? (keys (engine-matches p H)) (keys (denote p H))))

(test-case "ACT and hybrid absence: silence from activation"
  (define p (absence (disj act (prim 'X #:bind 'x)) (prim 'X) 600 #:clock 'hybrid))
  (define-values (e0 d0) (engine-start (make-engine p #:activation 0)))
  (define-values (e1 d1) (engine-advance e0 -inf.0 600))
  (check-equal? (map pmatch-te d1) '(600))
  (define-values (evt proc) (engine-deadlines e0))
  (check-equal? proc 600))

(spec-test '(PT.primitive PT.activation PT.sequence PT.conjunction PT.disjunction PT.negation PT.absence
             PT.span-absence PT.kleene-plus PT.kleene-star PT.repeat PT.any PT.within PT.where
             PT.aperiodic PT.aperiodic-cumulative PT.periodic PT.periodic-cumulative SEL.any CON.unrestricted OUT.every)
           "property: in any arrival order, the engine finds exactly the oracle's matches"
  (check-property
   (make-config #:tests (trials))
   (property ([seed (gen:integer-in 0 1000000)] [h gen:history] [shuffle-seed (gen:integer-in 0 1000)])
     (define p (random-pattern seed))
     (define arrivals (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
                        (random-seed shuffle-seed) (shuffle h)))
     (check-equal? (keys (engine-matches p arrivals)) (keys (denote p h #:activation 0))))))

(spec-test '(EVAL.deferred PT.negation PT.absence PT.span-absence PT.aperiodic PT.periodic)
           "property: with the watermark advancing, deferred decisions still give the oracle's matches"
  (check-property
   (make-config #:tests (trials))
   (property ([seed (gen:integer-in 0 1000000)] [h gen:point-history])
     (define p (random-pattern seed))
     (check-equal? (keys (engine-matches p h #:ascending? #t)) (keys (denote p h #:activation 0))))))
