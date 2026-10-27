#lang racket/base
;; Consumption, output, skip, and evaluation policies (spec §12.2, §12.3).
(require rackunit
         racket/list
         racket/set
         "util.rkt"
         "../private/event.rkt"
         "../private/dataflow.rkt"
         "../private/pattern/ast.rkt"
         "../private/pattern/reference.rkt"
         "../private/pattern/operators.rkt"
         "../private/pattern/policies.rkt"
         "../private/pattern/families.rkt")

(define (ev id type t [payload '()]) (make-event type payload #:id id #:at t))
(define (A id t) (ev id 'A t)) (define (B id t) (ev id 'B t)) (define (C id t) (ev id 'C t))
(define a (prim 'A #:bind 'a)) (define b (prim 'B #:bind 'b)) (define c (prim 'C #:bind 'c))

;; run a pattern operator over events (arriving at their times, watermark
;; ascending), returning (emission-time . sorted ids) for every output
(define (outputs p events #:selection [sel 'any] #:steps [steps #f]
                 #:consumption [con 'unrestricted] #:output [out 'every] #:skip [skip 'no-skip]
                 #:eval [ev #f] #:timeout-partial [tp #f] #:retain [retain '()])
  (define op (incremental-pattern-operator
              p #:selection sel
              #:policy (make-match-policy #:consumption con #:output out #:skip skip #:eval ev
                                          #:timeout-partial tp #:retain retain)))
  (define src (make-source 'in #:watermark 'ascending))
  (define n (make-node 'p op (list src) #:sink? #t))
  (define r (run (make-program (list n)) (or steps (for/list ([e events]) (arrive (event-te e) 'in e)))))
  (for/list ([o (run-result-outputs r)])
    (cons (output-at o) (sort (map event-id (event-constituents (output-item o))) string<?))))
(define (ids os) (map cdr os))

;; --- §15 trace A, the OUT.maximal column -----------------------------------
(define (swipe id t amount) (make-event 'Swipe `((amount . ,amount)) #:id id #:at (* 60 t)))
(define trace-a (list (swipe "e1" 1 2) (swipe "e2" 2 600) (swipe "e3" 3 50) (swipe "e4" 4 900)))
(define card-testing
  (within (neg-between (prim 'Swipe #:bind 'a #:when (λ (e) (< (event-attr e 'amount) 5)))
                       (prim 'Verified)
                       (kleene+ (prim 'Swipe #:bind 'b #:when (λ (e) (> (event-attr e 'amount) 500)))))
          600))

(spec-test '(OUT.maximal SEL.strict SEL.partition SEL.next SEL.any EVAL.deferred)
           "trace A (§15), OUT.maximal column, emitted once the watermark passes 11 minutes"
  (define (go s) (ids (outputs card-testing trace-a #:selection s #:output 'maximal)))
  (check-equal? (go 'strict) '(("e1" "e2")))
  (check-equal? (go 'partition) '(("e1" "e2")))
  (check-equal? (go 'next) '(("e1" "e2" "e4")))
  (check-equal? (go 'any) '(("e1" "e2" "e4")))
  ;; deferred until W > ts + 10 min = 660
  (define steps (append (for/list ([e trace-a]) (arrive (event-te e) 'in e))
                        (list (watermark-to 'in 660) (advance-to 700) (watermark-to 'in 661))))
  (check-equal? (map car (outputs card-testing trace-a #:selection 'next #:output 'maximal #:steps steps)) '(700)))

;; --- §12.2 consumption ------------------------------------------------------
(define ab (seq a b))
(define two-as (list (A "a1" 1) (A "a2" 2) (B "b1" 3) (B "b2" 4)))

(spec-test 'CON.unrestricted "unrestricted: every event may take part in any number of matches"
  (check-equal? (list->set (ids (outputs ab two-as #:consumption 'unrestricted)))
                (set '("a1" "b1") '("a2" "b1") '("a1" "b2") '("a2" "b2"))))

(spec-test 'CON.recent "recent: only the most recent initiator, which detection does not consume"
  (check-equal? (ids (outputs ab two-as #:consumption 'recent)) '(("a2" "b1") ("a2" "b2"))))

(spec-test 'CON.chronicle "chronicle: initiators and terminators paired oldest first, each once"
  (check-equal? (ids (outputs ab two-as #:consumption 'chronicle)) '(("a1" "b1") ("a2" "b2"))))

(spec-test 'CON.continuous "continuous: a terminator completes every open initiator, which are then consumed"
  (check-equal? (ids (outputs ab two-as #:consumption 'continuous)) '(("a1" "b1") ("a2" "b1"))))

(spec-test 'CON.cumulative "cumulative: the terminator emits one match with every accumulated occurrence"
  (check-equal? (ids (outputs ab two-as #:consumption 'cumulative)) '(("a1" "a2" "b1"))))

;; --- §12.3 output -----------------------------------------------------------
(define bs (list (A "a1" 1) (B "b1" 2) (B "b2" 3)))
(define ab+ (within (seq a (kleene+ b)) 100))

(spec-test 'OUT.every "every: all matches"
  (check-equal? (length (outputs ab+ bs #:output 'every)) 3))

(spec-test 'OUT.first "first: only the first match per initiator"
  (check-equal? (ids (outputs ab+ bs #:output 'first)) '(("a1" "b1"))))

(spec-test 'OUT.last "last: only the last match per initiator, once its scope has passed"
  (define os (outputs ab+ bs #:output 'last))
  (check-equal? (ids os) '(("a1" "b1" "b2"))))

(spec-test 'OUT.maximal "maximal: only matches not strictly contained in another"
  (check-equal? (ids (outputs ab+ bs #:output 'maximal)) '(("a1" "b1" "b2"))))

;; --- §12.3 skipping (the example of Flink CEP's after-match skip) ------------
(define bc (seq (kleene+ b) c))
(define bbbc (list (B "b1" 1) (B "b2" 2) (B "b3" 3) (C "c" 4)))

(spec-test 'SKIP.no-skip "no-skip keeps every partial match"
  (check-equal? (length (outputs bc bbbc #:skip 'no-skip)) 7))

(spec-test 'SKIP.to-next "to-next discards partial matches with the same initiator"
  (check-equal? (ids (outputs bc bbbc #:skip 'to-next)) '(("b1" "b2" "b3" "c") ("b2" "b3" "c") ("b3" "c"))))

(spec-test 'SKIP.past-last "past-last discards partial matches started before the match ended"
  (check-equal? (ids (outputs bc bbbc #:skip 'past-last)) '(("b1" "b2" "b3" "c"))))

(spec-test 'SKIP.to-first "to-first(x) discards partial matches started from the match's start up to its first x"
  ;; the first b of the emitted match is its initiator, so the range is empty
  (check-equal? (length (outputs bc bbbc #:skip '(to-first b))) 7)
  ;; here the range [a, b1) holds a: the other matches started at a are discarded
  (check-equal? (ids (outputs (seq a (kleene+ b) c) (list (A "a" 0) (B "b1" 1) (B "b2" 2) (C "c" 3))
                              #:skip '(to-first b)))
                '(("a" "b1" "b2" "c"))))

(spec-test 'SKIP.to-last "to-last(x) discards partial matches started before the last event bound to x"
  (check-equal? (ids (outputs bc bbbc #:skip '(to-last b))) '(("b1" "b2" "b3" "c") ("b3" "c"))))

;; --- §12.3 evaluation timing --------------------------------------------------
(spec-test 'EVAL.immediate "immediate: a monotone match is emitted as soon as its terminator arrives"
  (define steps (list (arrive 1 'in (A "a1" 1)) (arrive 5 'in (B "b1" 2))))
  (check-equal? (map car (outputs ab '() #:eval 'immediate #:steps steps)) '(5)))

(spec-test 'EVAL.deferred "deferred: emitted when the watermark passes the end of the match's scope"
  (define steps (list (arrive 1 'in (A "a1" 1)) (arrive 5 'in (B "b1" 2)) (advance-to 9) (watermark-to 'in 3)))
  (check-equal? (map car (outputs ab '() #:eval 'deferred #:steps steps)) '(9)))

;; --- timed-out partial matches and retain ------------------------------------
(define acc3 (accumulate-pattern 'X 3 0))
(define (X id t) (ev id 'X t))

(spec-test '(OUT.timeout-partial PT.flush) "a partial match past its time bound is reported as timed out"
  (define steps (list (arrive 0 'in (X "x0" 0)) (arrive 1 'in (X "x1" 1)) (advance-to 20)))
  (define os (outputs acc3 '() #:selection 'next #:output 'first #:consumption 'cumulative
                      #:timeout-partial '(10 processing-time) #:steps steps))
  (check-equal? os '((10 "x0" "x1"))))

(spec-test '(CON.retain PT.flush) "retain: a timed-out output consumes nothing, so its events keep counting"
  (define steps (list (arrive 0 'in (X "x0" 0)) (arrive 1 'in (X "x1" 1)) (advance-to 20) (arrive 25 'in (X "x2" 25))))
  (define retained (outputs acc3 '() #:selection 'next #:output 'first #:consumption 'cumulative
                            #:timeout-partial '(10 processing-time) #:retain '(timeout) #:steps steps))
  (check-equal? retained '((10 "x0" "x1") (25 "x0" "x1" "x2")))
  (define consumed (outputs acc3 '() #:selection 'next #:output 'first #:consumption 'cumulative
                            #:timeout-partial '(10 processing-time) #:steps steps))
  (check-equal? consumed '((10 "x0" "x1"))))
