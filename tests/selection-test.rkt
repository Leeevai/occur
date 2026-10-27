#lang racket/base
;; Selection strategies (spec §12.1).
(require rackunit
         rackcheck
         racket/list
         racket/set
         "util.rkt"
         "pattern-gen.rkt"
         "../private/event.rkt"
         "../private/pattern/ast.rkt"
         "../private/pattern/reference.rkt"
         "../private/pattern/nfa.rkt"
         "../private/pattern/policies.rkt")

(define (engine-matches p arrivals strategy #:activation [t0 0])
  (define-values (e0 d0) (engine-start (make-engine p #:activation t0 #:selection strategy)))
  (define-values (e1 acc)
    (for/fold ([e e0] [acc d0]) ([ev arrivals] [i (in-naturals)])
      (define-values (e2 d) (engine-event e ev i))
      (values e2 (append acc d))))
  (define-values (e3 dfinal) (engine-advance e1 +inf.0 +inf.0))
  (append acc dfinal))

(define (keys ms) (sort (map pmatch-key ms) (λ (a b) (string<? (format "~s" a) (format "~s" b)))))
(define (id-set ms) (list->set (map (λ (m) (sort (pmatch-ids m) string<?)) ms)))

(define (swipe id t amount) (make-event 'Swipe `((amount . ,amount)) #:id id #:at (* 60 t)))
(define trace-a (list (swipe "e1" 1 2) (swipe "e2" 2 600) (swipe "e3" 3 50) (swipe "e4" 4 900)))
(define card-testing
  (within (neg-between (prim 'Swipe #:bind 'a #:when (λ (e) (< (event-attr e 'amount) 5)))
                       (prim 'Verified)
                       (kleene+ (prim 'Swipe #:bind 'b #:when (λ (e) (> (event-attr e 'amount) 500)))))
          600))

(spec-test '(SEL.strict SEL.partition SEL.next SEL.any) "trace A (§15), OUT.every column: each strategy's matches"
  (define (go s) (id-set (engine-matches card-testing trace-a s)))
  (check-equal? (go 'strict) (set '("e1" "e2")))
  (check-equal? (go 'partition) (set '("e1" "e2")))
  (check-equal? (go 'next) (set '("e1" "e2") '("e1" "e2" "e4")))
  (check-equal? (go 'any) (set '("e1" "e2") '("e1" "e2" "e4") '("e1" "e4")))
  ;; the oracle's filter agrees
  (for ([s selection-strategies])
    (check-equal? (id-set (select-matches (denote card-testing trace-a) s trace-a)) (go s))))

(spec-test 'SEL.strict "strict contiguity is adjacency in the whole stream, partition contiguity within a key"
  (define p (seq (prim 'A #:bind 'a) (prim 'B #:bind 'b)))
  ;; two keys interleaved: the stream is a1 x a2 b1, where x belongs to another key
  (define a1 (make-event 'A '() #:id "a1" #:at 1))
  (define b1 (make-event 'B '() #:id "b1" #:at 4))
  (define-values (e0 _d) (engine-start (make-engine p #:activation 0 #:selection 'strict)))
  (define-values (e1 d1) (engine-event e0 a1 0))
  (define-values (e2 d2) (engine-event e1 b1 2))      ; global index 2: one event of another key between
  (check-equal? d2 '())
  (define-values (f0 _f) (engine-start (make-engine p #:activation 0 #:selection 'partition)))
  (define-values (f1 g1) (engine-event f0 a1 0))
  (define-values (f2 g2) (engine-event f1 b1 2))
  (check-equal? (map pmatch-ids g2) '(("a1" "b1"))))

(spec-test '(SEL.strict SEL.partition SEL.next SEL.any)
           "property: on one key the match sets nest strict ⊆ partition ⊆ next ⊆ any"
  (check-property
   (make-config #:tests (trials))
   (property ([seed (gen:integer-in 0 1000000)] [h gen:point-history])
     (define p (random-pattern seed #:fragment 'selection))
     (define sets (for/list ([s '(strict partition next any)]) (keys (engine-matches p h s))))
     (for ([smaller sets] [larger (cdr sets)])
       (for ([k smaller]) (check-not-false (member k larger)))))))

(spec-test '(SEL.strict SEL.partition SEL.next) "property: the engine under each strategy equals the oracle filtered by it"
  (check-property
   (make-config #:tests (trials))
   (property ([seed (gen:integer-in 0 1000000)] [h gen:point-history] [s (gen:one-of '(strict partition next))])
     (define p (random-pattern seed #:fragment 'selection))
     (check-equal? (keys (engine-matches p h s)) (keys (select-matches (denote p h #:activation 0) s h))))))

(spec-test '(SEL.strict SEL.partition) "the oracle reads strict contiguity over all keys when given the global arrival index"
  (define a (make-event 'A '((k . "x")) #:id "a" #:at 1))
  (define b (make-event 'B '((k . "x")) #:id "b" #:at 3))
  (define p (seq (prim 'A #:bind 'a) (prim 'B #:bind 'b)))
  (define hx (list a b))                       ; key x's history
  (define global (hash "a" 0 "o" 1 "b" 2))     ; key y's event "o" arrived between them
  (define ms (denote p hx))
  (check-equal? (length (select-matches ms 'strict hx)) 1)
  (check-equal? (length (select-matches ms 'strict hx #:global-index global)) 0)
  (check-equal? (length (select-matches ms 'partition hx #:global-index global)) 1))

(spec-test 'OUT.maximal "the oracle's maximal output keeps the matches not strictly contained in another"
  (check-equal? (id-set (maximal-matches (denote card-testing trace-a))) (set '("e1" "e2" "e4")))
  (check-equal? (id-set (maximal-matches (select-matches (denote card-testing trace-a) 'strict trace-a)))
                (set '("e1" "e2"))))
