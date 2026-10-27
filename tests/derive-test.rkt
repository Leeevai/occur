#lang racket/base
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/time.rkt"
         "../private/event.rkt"
         "../private/operator.rkt"
         "../private/dataflow.rkt"
         "../private/derive.rkt"
         "../private/io.rkt"
         "../private/ops/stateless.rkt"
         "../private/pattern/ast.rkt"
         "../private/pattern/reference.rkt"
         "../private/pattern/operators.rkt")

(define (ev id type t [payload '()] #:n [n 1]) (make-event type payload #:id id #:at t #:n n))

(spec-test 'DR.derive "a match derives ⟨fresh id, τ′, f(bindings), ts(m), te(m), 1, op, m⟩"
  (define m (car (denote (seq (prim 'A #:bind 'a) (prim 'B #:bind 'b)) (list (ev "a" 'A 1) (ev "b" 'B 5)))))
  (define d (derive-match m #:type 'Pair #:payload (λ (bs) (hasheq 'first (event-id (hash-ref bs 'a)))) #:src 'pairs))
  (check-equal? (list (event-type d) (event-attr d 'first) (event-ts d) (event-te d) (event-n d) (event-src d))
                '(Pair "a" 1 5 1 pairs))
  (check-equal? (map event-id (event-constituents d)) '("a" "b")))

(spec-test 'DR.summary "an output may carry a summary instead of its constituents"
  (define o (summarized-output 'Trigger '((reason . samples)) #:ts 0 #:te 20 #:samples 3 #:sources '(("s1" . load))))
  (check-equal? (event-constituents o) '())
  (check-equal? (event-summary o) (hasheq 'interval '(0 20) 'samples 3 'merged 1 'sources '(("s1" . load))))
  (define w (make-composite 'Window '((count . 3)) (list (ev "x" 'R 1 #:n 2) (ev "y" 'R 4))))
  (define s (summarize w #:sources-of (λ (e) (event-type e))))
  (check-equal? (event-constituents s) '())
  (check-equal? (hash-ref (event-summary s) 'samples) 3)
  (check-equal? (hash-ref (event-summary s) 'interval) '(1 4)))

(spec-test 'DR.output-identity "identity comes from content, not emission time, except for outputs covering no new data"
  (define (id #:at at #:none? [none? #f])
    (output-identity #:instance '(rule "d1" "") #:key "d1" #:reason 'samples #:interval (interval 0 20)
                     #:samples 3 #:emitted-at at #:no-new-data? none?))
  (check-equal? (id #:at 20) (id #:at 99))
  (check-false (equal? (id #:at 20 #:none? #t) (id #:at 99 #:none? #t)))
  (define a (ev "o1" 'Out 5 '((v . 1))))
  (define b (ev "o2" 'Out 5 '((v . 1))))       ; the same content under a different id
  (check-equal? (event-identity a) (event-identity b))
  (define outs (list (output 5 'n "k" a) (output 9 'n "k" b) (output 9 'n "k" (ev "o3" 'Out 6 '((v . 1))))))
  (check-equal? (map (λ (o) (event-id (output-item o))) (dedupe-outputs outs)) '("o1" "o3")))

(spec-test 'DR.output-identity "property: downstream deduplication is idempotent and keeps first occurrences"
  (check-property
   (make-config #:tests (trials))
   (property ([vs (gen:list (gen:integer-in 0 4) #:max-length 15)])
     (define outs (for/list ([v vs] [i (in-naturals)]) (output i 'n #f (ev (format "e~a" i) 'Out 0 `((v . ,v))))))
     (define once (dedupe-outputs outs))
     (check-equal? (dedupe-outputs once) once)
     (check-equal? (length once) (length (remove-duplicates vs))))))

(spec-test 'DR.ordered "outputs are delivered grouped by key, in order within each key"
  (define split (stateless-operator 'split (λ (e c) (for ([k '("b" "a" "b" "a")] [i (in-naturals)])
                                                     (emit! c (make-event 'O `((k . ,k) (i . ,i)) #:at 0))))))
  (define keyed (make-operator 'by #:key (λ (e) (event-attr e 'k))))
  (define src (make-source 'in))
  (define s (make-node 'split split (list src)))
  (define n (make-node 'by keyed (list s) #:sink? #t))
  (define r (run (make-program (list n)) (list (arrive 1 'in (ev "x" 'X 1)))))
  (check-equal? (map (λ (o) (list (output-key o) (event-attr (output-item o) 'i))) (run-result-outputs r))
                '(("b" 0) ("b" 2) ("a" 1) ("a" 3)))
  (check-equal? (map car (delivery-groups (run-result-outputs r))) '("b" "a")))

(spec-test '(DR.closure ES.space) "derived events re-enter the event space and feed other operators"
  ;; pairs of A ; B, then pairs of those pairs: a two-level event hierarchy
  (define src (make-source 'in))
  (define pairs (make-node 'pairs (incremental-pattern-operator (seq (prim 'A #:bind 'a) (prim 'B #:bind 'b))
                                                              #:derive (λ (m) (derive-match m #:type 'Pair)))
                           (list src) #:sink? #t))
  (define twice (make-node 'twice (incremental-pattern-operator (seq (prim 'Pair #:bind 'p) (prim 'Pair #:bind 'q))
                                                              #:derive (λ (m) (derive-match m #:type 'PairOfPairs)))
                           (list pairs) #:sink? #t))
  (define r (run (make-program (list twice))
                 (for/list ([e (list (ev "a1" 'A 1) (ev "b1" 'B 2) (ev "a2" 'A 3) (ev "b2" 'B 4))]) (arrive (event-te e) 'in e))))
  (define top (run-result-events r 'twice))
  (check-equal? (length top) 1)
  (check-equal? (event-level (car top)) 2))

(spec-test 'DR.act "an action runs a side effect and is not an event unless it emits one"
  (define calls (box '()))
  (define op (action-operator (λ (e) (set-box! calls (cons (event-id e) (unbox calls))) 'sent)
                              #:completion 'ActionCompleted))
  (define r (run-operator op (list (list (ev "alert1" 'Alert 1) (ev "alert2" 'Alert 2)))))
  (check-equal? (reverse (unbox calls)) '("alert1" "alert2"))
  (check-equal? (map (λ (e) (list (event-type e) (event-attr e 'result))) (run-result-events r))
                '((ActionCompleted sent) (ActionCompleted sent)))
  (check-equal? (run-result-events (run-operator (action-operator void) (list (list (ev "a" 'Alert 1))))) '()))

(spec-test 'DR.view "a materialized view is queried on demand and exported on a schedule"
  (define op (view-operator #:key (λ (e) (event-attr e 'dev)) #:init (λ () 0) #:update (λ (s e now) (+ s (event-n e)))
                            #:export-every 60))
  (define src (make-source 'in))
  (define n (make-node 'counts op (list src) #:sink? #t))
  (define r (run (make-program (list n))
                 (list (arrive 10 'in (ev "a" 'R 10 '((dev . "d1")) #:n 2)) (arrive 70 'in (ev "b" 'R 70 '((dev . "d1"))))
                       (advance-to 125))))
  (check-equal? (map (λ (o) (list (output-at o) (event-attr (output-item o) 'view))) (run-result-outputs r))
                '((60 2) (120 3)))
  (check-equal? (view-ref r 'counts "d1") 3))

(spec-test 'DR.view "health by arrival age and data age: live, late, quiet, silent"
  (define (h now last newest) (classify-health #:now now #:last-arrival last #:newest newest
                                               #:live 60 #:silent 600 #:stale 300))
  (check-equal? (h 100 90 95) 'live)
  (check-equal? (h 1000 990 100) 'late)
  (check-equal? (h 1000 800 790) 'quiet)
  (check-equal? (h 1000 100 90) 'silent)
  (check-equal? (h 1000 #f -inf.0) 'silent))

(spec-test '(DR.retract TM.retraction) "a retracted constituent retracts every event derived from it, transitively"
  (define src (make-source 'in))
  (define m1 (make-node 'm1 (map-operator (λ (e) (event-payload e)) #:type 'Level1) (list src)))
  (define m2 (make-node 'm2 (map-operator (λ (e) (event-payload e)) #:type 'Level2) (list m1) #:sink? #t))
  (define r (run (make-program (list m2)) (list (arrive 1 'in (ev "x" 'X 1)) (arrive 2 'in (retraction-of "x")))))
  (define items (map output-item (run-result-outputs r)))
  (check-true (retraction? (second items)))
  (check-equal? (event-type (retraction-event (second items))) 'Level2)
  (check-equal? (event-id (retraction-event (second items))) (event-id (first items))))
