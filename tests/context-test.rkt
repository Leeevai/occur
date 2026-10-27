#lang racket/base
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/time.rkt"
         "../private/event.rkt"
         "../private/operator.rkt"
         "../private/dataflow.rkt"
         "../private/context.rkt")

(define (ev id t [payload '()]) (make-event 'X payload #:id id #:at t))
(define (ctxs r) (for/list ([e (run-result-events r)]) (list (event-id (car (event-constituents e))) (event-attr e 'context))))
(define (run-ctx a events) (run-operator (context-operator a) (list events)))

(spec-test 'CX.temporal "a fixed interval context scopes events to [s, e)"
  (define r (run-ctx (fixed-interval-context 10 20) (list (ev "a" 5) (ev "b" 10) (ev "c" 19) (ev "d" 20))))
  (check-equal? (ctxs r) '(("b" (fixed 10 20)) ("c" (fixed 10 20))))
  (check-equal? (run-result-counter r 'op 'outside) 2))

(define (kind k) (λ (e) (eq? (event-attr/default e 'kind #f) k)))

(spec-test 'CX.temporal "an event interval opens on A and closes on B or a timeout"
  (define a (event-interval-context (kind 'open) (kind 'close) #:timeout 50))
  (define r (run-ctx a (list (ev "x" 0) (ev "o1" 5 '((kind . open))) (ev "m" 8) (ev "c1" 9 '((kind . close)))
                             (ev "y" 10) (ev "o2" 20 '((kind . open))) (ev "z" 100))))
  (check-equal? (ctxs r) '(("o1" (interval "o1")) ("m" (interval "o1")) ("c1" (interval "o1"))
                           ("o2" (interval "o2")))))

(spec-test 'CX.temporal "sliding fixed and sliding event intervals"
  (check-equal? (ctxs (run-ctx (sliding-interval-context 10 5) (list (ev "a" 7))))
                '(("a" (sliding 0 10)) ("a" (sliding 5 15))))
  (define r (run-ctx (sliding-event-interval-context (kind 'open) 10)
                     (list (ev "o1" 0 '((kind . open))) (ev "o2" 5 '((kind . open))) (ev "a" 7) (ev "b" 12))))
  (check-equal? (ctxs r) '(("o1" (sliding-event "o1")) ("o2" (sliding-event "o1")) ("o2" (sliding-event "o2"))
                           ("a" (sliding-event "o1")) ("a" (sliding-event "o2")) ("b" (sliding-event "o2")))))

(spec-test 'CX.segmentation "segmentation partitions by attribute values"
  (define r (run-ctx (segmentation-context '(region tier))
                     (list (ev "a" 1 '((region . "eu") (tier . 1))) (ev "b" 2 '((region . "us") (tier . 2))))))
  (check-equal? (ctxs r) '(("a" (segment "eu" 1)) ("b" (segment "us" 2)))))

(define (at-loc id t x y [kind #f]) (ev id t `((location . ,(location x y)) (kind . ,kind))))

(spec-test 'CX.spatial "within distance r of a fixed location, of an entity, or of an event"
  (check-equal? (ctxs (run-ctx (fixed-location-context (location 0 0) 5)
                               (list (at-loc "in" 1 3 4) (at-loc "out" 2 6 0))))
                (list (list "in" (list 'near (location 0 0) 5))))
  (define depots (hash "d1" (location 0 0) "d2" (location 100 0)))
  (check-equal? (ctxs (run-ctx (entity-distance-context (λ (id) (hash-ref depots id #f)) (λ () '("d1" "d2")) 10)
                               (list (at-loc "t1" 1 95 0) (at-loc "t2" 2 50 0))))
                '(("t1" (near-entity "d2"))))
  (check-equal? (ctxs (run-ctx (event-distance-context (kind 'crash) 10 60)
                               (list (at-loc "crash" 0 0 0 'crash) (at-loc "near" 10 3 4)
                                     (at-loc "far" 20 30 0) (at-loc "later" 70 1 1))))
                '(("crash" (near-event "crash")) ("near" (near-event "crash")))))

(spec-test 'CX.composite "a composite context intersects its parts"
  (define a (composite-context (segmentation-context '(customer)) (sliding-interval-context 3600 3600)))
  (check-equal? (ctxs (run-ctx a (list (ev "a" 100 '((customer . "c1"))) (ev "b" 4000 '((customer . "c1"))))))
                '(("a" ((segment "c1") (sliding 0 3600))) ("b" ((segment "c1") (sliding 3600 7200))))))

(spec-test 'CX.state "the operator is active only while the external state holds"
  (define data (make-source 'data))
  (define state (make-source 'state))
  (define n (make-node 'during (state-context-operator (λ (e) (equal? (event-attr e 'status) "on")))
                       (list data state) #:sink? #t))
  (define r (run (make-program (list n))
                 (list (arrive 1 'data (ev "a" 1)) (arrive 2 'state (ev "m1" 2 '((status . "on"))))
                       (arrive 3 'data (ev "b" 3)) (arrive 4 'data (ev "c" 4))
                       (arrive 5 'state (ev "m2" 5 '((status . "off")))) (arrive 6 'data (ev "d" 6)))))
  (check-equal? (ctxs r) '(("b" (state "m1")) ("c" (state "m1"))))
  (check-equal? (run-result-counter r 'during 'inactive) 2))

(define (dev id t d) (ev id t `((device . ,d))))

(define (entity-run steps)
  (define src (make-source 'in))
  (define gate (make-node 'gate (entity-context-operator 'devices #:key (λ (e) (event-attr e 'device))) (list src)
                          #:sink? #t))
  (define acts (make-node 'acts activations-operator (list gate) #:sink? #t))
  (run (make-program (list gate acts)) steps))

(spec-test '(CX.entity CX.activation) "directory entities are activated before they send anything"
  (define r (entity-run (list (directory-push 'devices '(("d1" . active) ("d2" . active)) #:at 0)
                              (arrive 5 'in (dev "a" 5 "d1")))))
  (check-equal? (for/list ([o (run-result-outputs r)] #:when (eq? (output-node o) 'acts))
                  (list (output-at o) (event-attr (output-item o) 'key)))
                '((0 "d1") (0 "d2")))
  (check-equal? (map event-id (run-result-events r 'gate)) '("a")))

(spec-test 'CX.entity "deprecated entities are ignored and counted; removed ones are activated afresh"
  (define r (entity-run (list (directory-push 'devices '(("d1" . active) ("d2" . active)) #:at 0)
                              (directory-push 'devices '(("d1" . inactive)) #:mode 'patch #:at 10)
                              (arrive 11 'in (dev "a" 11 "d1"))
                              (directory-push 'devices '(("d1" . inactive))) ; replace: d2 removed
                              (arrive 20 'in (dev "b" 20 "d2")))))
  (check-equal? (map event-id (run-result-events r 'gate)) '("b"))
  (check-equal? (run-result-counter r 'gate 'ignored) 1)
  (check-equal? (for/list ([o (run-result-outputs r)] #:when (eq? (output-node o) 'acts))
                  (list (output-at o) (event-attr (output-item o) 'key)))
                '((0 "d1") (0 "d2") (20 "d2"))))

(spec-test '(CX.entity CX.activation) "lifecycle signals create and drop downstream state"
  (define src (make-source 'in))
  (define gate (make-node 'gate (entity-context-operator 'devices #:key (λ (e) (event-attr e 'device))) (list src)))
  (define seen (make-node 'seen (make-operator 'seen #:lifecycle? #t #:key (λ (e) (event-attr e 'device))
                                               #:init-key (λ (g k c) (ctx-now c)))
                          (list gate) #:sink? #t))
  (define r (run (make-program (list seen))
                 (list (directory-push 'devices '(("d1" . active)) #:at 3)
                       (directory-push 'devices '(("d1" . inactive)) #:at 9))))
  ;; activated at 3, then dropped when deprecated
  (check-equal? (hash-ref (cdr (hash-ref (run-result-states r) 'seen)) "d1" #f) #f)
  (define r2 (run (make-program (list seen)) (list (directory-push 'devices '(("d1" . active)) #:at 3))))
  (check-equal? (hash-ref (cdr (hash-ref (run-result-states r2) 'seen)) "d1") 3))
