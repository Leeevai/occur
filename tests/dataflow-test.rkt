#lang racket/base
(require rackunit
         racket/list
         "util.rkt"
         "../private/time.rkt"
         "../private/event.rkt"
         "../private/operator.rkt"
         "../private/dataflow.rkt")

(define (ev id t [payload '()]) (make-event 'X payload #:id id #:at t))
(define (ats r) (map output-at (run-result-outputs r)))
(define (ids r) (map (λ (o) (event-id (output-item o))) (run-result-outputs r)))

(define pass (make-operator 'pass))
(define src (make-source 'in))

(spec-test 'ES.operator "an operator maps input histories to output events"
  (define keep-big (stateless-operator 'big (λ (e c) (when (> (event-attr e 'v) 2) (emit! c e)))))
  (define r (run-operator keep-big (list (list (ev "a" 1 '((v . 1))) (ev "b" 2 '((v . 5)))))))
  (check-equal? (ids r) '("b"))
  (check-true (operator-monotone? keep-big)))

(test-case "a pass-through sink delivers arrivals in order, stamped with processing time"
  (define n (make-node 'out pass (list src) #:sink? #t))
  (define r (run (make-program (list n))
                 (list (arrive 5 'in (ev "a" 1)) (arrive 7 'in (ev "b" 2)))))
  (check-equal? (ids r) '("a" "b"))
  (check-equal? (ats r) '(5 7)))

;; A keyed counter: per-key state is isolated (ES.keyed).
(define counter
  (make-operator 'count
                 #:key (λ (e) (event-attr e 'k))
                 #:init-key (λ (g k c) 0)
                 #:on-event (λ (g s e c)
                              (emit! c (make-event 'Count `((k . ,(ctx-key c)) (n . ,(add1 s)))
                                                   #:at (event-te e)))
                              (add1 s))))

(spec-test 'ES.keyed "the runner partitions operator state by key"
  (define n (make-node 'count counter (list src) #:sink? #t))
  (define r (run (make-program (list n))
                 (for/list ([k '(a b a a b)] [i (in-naturals)])
                   (arrive i 'in (ev (format "e~a" i) i `((k . ,k)))))))
  (check-equal? (for/list ([e (run-result-events r)]) (cons (event-attr e 'k) (event-attr e 'n)))
                '((a . 1) (b . 1) (a . 2) (a . 3) (b . 2))))

;; Emits a Tick 10 seconds of processing time after the newest arrival.
(define ticker
  (make-operator 'tick
                 #:init-key (λ (g k c) #f)
                 #:on-event (λ (g s e c) (+ (ctx-now c) 10))
                 #:deadline (λ (s) s)
                 #:on-timer (λ (g s c)
                              (cond [(and s (>= (ctx-now c) s))
                                     (emit! c (make-event 'Tick '() #:at (ctx-now c)))
                                     #f]
                                    [else s]))))

(test-case "processing-time deadlines fire at their own time; early wake-ups re-check"
  (define n (make-node 'tick ticker (list src) #:sink? #t))
  (define r (run (make-program (list n))
                 (list (arrive 0 'in (ev "a" 0)) (arrive 5 'in (ev "b" 5)) (advance-to 100))))
  ;; the wake-up at 10 is early (the deadline moved to 15) and emits nothing
  (check-equal? (ats r) '(15)))

(test-case "a deadline due before an arrival fires before the arrival is processed"
  (define n (make-node 'tick ticker (list src) #:sink? #t))
  (define r (run (make-program (list n)) (list (arrive 0 'in (ev "a" 0)) (arrive 30 'in (ev "b" 30)))))
  (check-equal? (ats r) '(10)))

(test-case "after a suspension every overdue instance fires once, at the resume time"
  (define n (make-node 'tick ticker (list src) #:sink? #t))
  (define r (run (make-program (list n)) (list (arrive 0 'in (ev "a" 0)) (suspend 5 30))))
  (check-equal? (ats r) '(30)))

;; Emits each event once the watermark passes te + 5 (an event-time timer).
(define delayer
  (make-operator 'delay
                 #:monotone? #f
                 #:init-key (λ (g k c) '())
                 #:on-event (λ (g s e c) (append s (list e)))
                 #:event-deadline (λ (s) (and (pair? s) (+ (event-te (car s)) 5)))
                 #:on-timer (λ (g s c)
                              (define-values (due later)
                                (partition (λ (e) (<= (+ (event-te e) 5) (ctx-watermark c))) s))
                              (for ([e due]) (emit! c e))
                              later)))

(spec-test 'TM.timer "event-time timers fire when the watermark reaches them"
  (define n (make-node 'delay delayer (list src) #:sink? #t))
  (define r (run (make-program (list n))
                 (list (arrive 1 'in (ev "a" 1)) (arrive 2 'in (ev "b" 2))
                       (watermark-to 'in 6) (watermark-to 'in 100))
                 #:final-watermark? #f))
  (check-equal? (ids r) '("a" "b"))
  (check-equal? (ats r) '(2 2)))

(test-case "the final watermark flushes event-time state at the end of the script"
  (define n (make-node 'delay delayer (list src) #:sink? #t))
  (define r (run (make-program (list n)) (list (arrive 1 'in (ev "a" 1)))))
  (check-equal? (ids r) '("a")))

(spec-test 'TM.watermark "a bounded-delay source advances W to max te − d"
  (define s (make-source 'in #:watermark '(bounded-delay 3)))
  (define n (make-node 'delay delayer (list s) #:sink? #t))
  (define r (run (make-program (list n))
                 (list (arrive 1 'in (ev "a" 1)) (arrive 9 'in (ev "b" 9)) (arrive 20 'in (ev "c" 20)))
                 #:final-watermark? #f))
  ;; W = 6 after b releases a (1 + 5 <= 6); W = 17 after c releases b
  (check-equal? (ids r) '("a" "b"))
  (check-equal? (hash-ref (run-result-watermarks r) 'in) 17))

(test-case "control messages are broadcast to operators that accept them"
  (define cfg (make-operator 'cfg
                             #:init-global (λ () 0)
                             #:on-control (λ (g keys msg c)
                                            (emit! c (make-event 'Cfg `((v . ,msg)) #:at (ctx-now c)))
                                            (values msg keys))))
  (define n (make-node 'cfg cfg (list src) #:sink? #t))
  (define r (run (make-program (list n)) (list (control 42 #:at 3))))
  (check-equal? (map (λ (e) (event-attr e 'v)) (run-result-events r)) '(42))
  (check-equal? (ats r) '(3)))

(test-case "lifecycle signals create and drop per-key state and flow downstream"
  (define seen (make-operator 'seen #:lifecycle? #t #:key (λ (e) (event-attr e 'k))
                              #:init-key (λ (g k c) 'fresh)
                              #:on-event (λ (g s e c) (emit! c (make-event 'S `((s . ,s)) #:at 0)) 'used)))
  (define gate (make-operator 'gate
                              #:on-event (λ (g s e c)
                                           (if (event-has-attr? e 'cmd)
                                               (emit! c (signal (event-attr e 'cmd) (event-attr e 'k) 0 #f))
                                               (emit! c e))
                                           s)))
  (define g (make-node 'gate gate (list src)))
  (define n (make-node 'seen seen (list g) #:sink? #t))
  (define r (run (make-program (list n))
                 (list (arrive 0 'in (ev "a" 0 '((k . 1) (cmd . activate))))
                       (arrive 1 'in (ev "b" 1 '((k . 1))))
                       (arrive 2 'in (ev "c" 2 '((k . 1))))
                       (arrive 3 'in (ev "d" 3 '((k . 1) (cmd . drop))))
                       (arrive 4 'in (ev "e" 4 '((k . 1)))))))
  (check-equal? (map (λ (e) (event-attr e 's)) (run-result-events r)) '(fresh used fresh)))

(test-case "multi-input nodes, output ports, and topological propagation"
  (define split2 (make-operator 'split #:outputs 2
                                #:on-event (λ (g s e c) (emit! c e (if (odd? (event-ts e)) 1 0)) s)))
  (define union (make-operator 'union #:arity 2))
  (define sp (make-node 'split split2 (list src)))
  (define u (make-node 'union union (list (cons sp 1) (cons sp 0)) #:sink? #t))
  (define r (run (make-program (list u)) (for/list ([i 4]) (arrive i 'in (ev (format "e~a" i) i)))))
  (check-equal? (ids r) '("e0" "e1" "e2" "e3")))

(spec-test 'DR.cycle-guard "programs are validated: names, inputs, arity, and cycles"
  (check-exn #rx"arity" (λ () (make-node 'u (make-operator 'u #:arity 2) (list src))))
  (check-exn #rx"unknown input" (λ () (make-program (list (make-node 'n pass (list 'nowhere))))))
  (check-exn #rx"share a name"
             (λ () (make-program (list (make-node 'n pass (list src)) (make-node 'n pass (list src))))))
  (define loop-a (make-node 'a (make-operator 'a #:arity 2) (list src 'b)))
  (define loop-b (make-node 'b pass (list loop-a)))
  (check-exn #rx"cycle guard" (λ () (make-program (list loop-b))))
  (define rec-a (make-node 'a (make-operator 'a #:arity 2) (list src 'b) #:recursive 3))
  (define rec-b (make-node 'b (stateless-operator 'b (λ (e c) (emit! c (make-composite 'Up '() (list e)))))
                           (list rec-a) #:sink? #t))
  (define r (run (make-program (list rec-b)) (list (arrive 0 'in (ev "a" 0)))))
  ;; levels 1, 2, 3 are emitted by b; the level-3 event stops at a's bound
  (check-equal? (map event-level (run-result-events r)) '(1 2 3))
  (check-equal? (run-result-counter r 'a 'recursion-bound) 1))

(test-case "ids of derived events are deterministic across runs"
  (define mk (stateless-operator 'mk (λ (e c) (emit! c (make-composite 'D '() (list e))))))
  (define (go) (ids (run (make-program (list (make-node 'mk mk (list src) #:sink? #t)))
                         (list (arrive 0 'in (ev "a" 0)) (arrive 1 'in (ev "b" 1))))))
  (check-equal? (go) '("e1" "e2"))
  (check-equal? (go) (go)))

(spec-test 'TM.retraction "⊖e propagates to every output derived from e"
  (define mk (stateless-operator 'mk (λ (e c)
                                       (if (event-has-attr? e 'undo)
                                           (emit! c (retraction (make-event 'X '() #:id (event-attr e 'undo) #:at 0)))
                                           (emit! c (make-composite 'D '() (list e)))))))
  (define up (make-node 'up (stateless-operator 'up (λ (e c)
                                                      (if (event-has-attr? e 'undo)
                                                          (emit! c (retraction (make-event 'X '() #:id (event-attr e 'undo) #:at 0)))
                                                          (emit! c e))))
                        (list src)))
  (define down (make-node 'down (stateless-operator 'down (λ (e c) (emit! c (make-composite 'D '() (list e)))))
                          (list up) #:sink? #t))
  (define r (run (make-program (list down))
                 (list (arrive 0 'in (ev "a" 0)) (arrive 1 'in (ev "b" 1 '((undo . "a")))))))
  (define items (map output-item (run-result-outputs r)))
  (check-true (event? (first items)))
  (check-true (retraction? (second items)))
  (check-equal? (event-id (retraction-event (second items))) (event-id (first items))))

(test-case "generators of arity 0 start with the run and arm their deadlines"
  (define gen (make-operator 'every-10 #:arity 0
                             #:init-key (λ (g k c) (+ (ctx-now c) 10))
                             #:deadline (λ (s) s)
                             #:on-timer (λ (g s c)
                                          (emit! c (make-event 'Tick '() #:at (ctx-now c)))
                                          (+ s 10))))
  (define n (make-node 'gen gen '() #:sink? #t))
  (define r (run (make-program (list n)) (list (advance-to 35)) #:clock (simulated-clock 5)))
  (check-equal? (ats r) '(15 25 35)))

(test-case "a timer that changes nothing does not re-arm a deadline that is already due"
  ;; this operator's event deadline never moves; at the final watermark it
  ;; would otherwise be woken forever
  (define stuck (make-operator 'stuck #:init-key (λ (g k c) 'pending)
                               #:on-event (λ (g s e c) s)
                               #:event-deadline (λ (s) 5)
                               #:on-timer (λ (g s c) s)))
  (define n (make-node 'stuck stuck (list src) #:sink? #t))
  (check-equal? (run-result-outputs (run (make-program (list n)) (list (arrive 1 'in (ev "a" 1))))) '()))

(test-case "an eager unkeyed operator starts with the run although it has inputs"
  (define eager (make-operator 'eager #:eager? #t
                               #:init-key (λ (g k c) (+ (ctx-now c) 10))
                               #:deadline (λ (s) s)
                               #:on-timer (λ (g s c) (emit! c (make-event 'Tick '() #:at (ctx-now c))) #f)))
  (define n (make-node 'eager eager (list src) #:sink? #t))
  (check-equal? (ats (run (make-program (list n)) (list (advance-to 50)))) '(10)))
