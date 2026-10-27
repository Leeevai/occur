#lang racket/base
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/time.rkt"
         "../private/event.rkt"
         "../private/operator.rkt"
         "../private/dataflow.rkt"
         "../private/progress.rkt")

(define (contrib kind ts te n) (make-batch-event 'C `((dev . "d1") (kind . ,kind)) #:ts ts #:te te #:n n))

(spec-test 'TM.keyed-watermark "W^any is the fastest kind and W^all the slowest; unseen kinds are −∞"
  ;; the contributions of trace R3 (§16.7)
  (define steps (list (contrib 'load 0 50 50) (contrib 'position 0 70 70) (contrib 'load 51 130 80)
                      (contrib 'position 71 250 180) (contrib 'load 131 300 170)))
  (define M '(load position))
  (define ws (for/fold ([acc '()] [w empty-keyed-wm] #:result (reverse acc)) ([e steps])
               (define w2 (keyed-wm-observe w (event-attr e 'kind) (event-te e)))
               (values (cons (list (keyed-wm-all w2 M) (keyed-wm-any w2 M)) acc) w2)))
  (check-equal? (map car ws) '(-inf.0 50 70 130 250))
  (check-equal? (map cadr ws) '(50 70 130 250 300))
  ;; with every kind watched, M is the kinds seen so far
  (check-equal? (keyed-wm-all (keyed-wm-observe empty-keyed-wm 'load 5) #f) 5)
  (check-equal? (keyed-wm-any empty-keyed-wm #f) -inf.0))

(spec-test 'TM.keyed-watermark "the operator emits progress per key when W_k advances"
  (define op (keyed-watermark-operator #:key (λ (e) (event-attr e 'dev)) #:kind (λ (e) (event-attr e 'kind))
                                       #:kinds '(load position) #:mode 'all))
  (define src (make-source 'in))
  (define n (make-node 'kw op (list src)))
  (define p (make-node 'progress (make-operator 'p) (list (cons n 1)) #:sink? #t))
  (define r (run (make-program (list p))
                 (for/list ([e (list (contrib 'load 0 50 50) (contrib 'position 0 70 70) (contrib 'load 51 130 80))]
                            [i (in-naturals)])
                   (arrive i 'in e))))
  (check-equal? (map (λ (e) (event-attr e 'watermark)) (run-result-events r)) '(50 70)))

(spec-test 'TM.keyed-watermark "property: W^all <= W^any, and both are monotone"
  (check-property
   (make-config #:tests (trials))
   (property ([obs (gen:list (gen:tuple (gen:one-of '(a b c)) (gen:integer-in 0 100)) #:max-length 20)])
     (for/fold ([w empty-keyed-wm] [pa -inf.0] [pl -inf.0] #:result (void)) ([o obs])
       (define w2 (keyed-wm-observe w (first o) (second o)))
       (define any (keyed-wm-any w2 '(a b c)))
       (define all (keyed-wm-all w2 '(a b c)))
       (check-true (<= all any))
       (check-true (>= any pa))
       (check-true (>= all pl))
       (values w2 any all)))))

(define (pt id t) (make-event 'X '() #:id id #:at t))

(spec-test 'TM.skew-guard "events stamped more than B ahead of the clock are dropped and counted"
  (define r (run-operator (skew-guard-operator (hours 1))
                          (list (list (pt "ok" 4000) (pt "future" 9000) (pt "edge" 4600)))
                          #:arrivals '((1000 1000 1000))))
  (check-equal? (map event-id (run-result-events r)) '("ok" "edge"))
  (check-equal? (run-result-counter r 'op 'future-skew) 1))

(define (late-run policy)
  (define src (make-source 'in))
  (define n (make-node 'late (lateness-operator policy) (list src)))
  (define on (make-node 'on-time (make-operator 'on) (list (cons n 0)) #:sink? #t))
  (define side (make-node 'side (make-operator 'side) (list (cons n 1)) #:sink? #t))
  (run (make-program (list on side))
       (list (arrive 1 'in (pt "a" 10)) (watermark-to 'in 20)
             (arrive 2 'in (pt "b" 18)) (arrive 3 'in (pt "c" 5)) (arrive 4 'in (pt "d" 25)))
       #:final-watermark? #f))

(spec-test 'TM.lateness "late events are dropped, sent to a side output, or accepted within L"
  (define dropped (late-run 'drop))
  (check-equal? (map event-id (run-result-events dropped 'on-time)) '("a" "d"))
  (check-equal? (run-result-counter dropped 'late 'late) 2)
  (define side (late-run 'side))
  (check-equal? (map event-id (run-result-events side 'side)) '("b" "c"))
  (define allowed (late-run '(allow 5)))
  (check-equal? (map event-id (run-result-events allowed 'on-time)) '("a" "b" "d"))
  (check-equal? (run-result-counter allowed 'late 'late-accepted) 1)
  (check-equal? (run-result-counter allowed 'late 'late) 1))

(spec-test '(TM.timer TM.event-time) "an event-time timer fires when the watermark reaches it"
  (define src (make-source 'in))
  (define n (make-node 'd (delay-operator 10) (list src) #:sink? #t))
  (define r (run (make-program (list n))
                 (list (arrive 1 'in (pt "a" 5)) (arrive 2 'in (pt "b" 7))
                       (watermark-to 'in 16) (arrive 3 'in (pt "c" 20)) (watermark-to 'in 40))
                 #:final-watermark? #f))
  (check-equal? (map event-id (run-result-events r)) '("a" "b" "c"))
  (check-equal? (map output-at (run-result-outputs r)) '(2 3 3)))

(spec-test '(TM.timer TM.processing-time) "a processing-time timer fires when the clock reaches it"
  (define src (make-source 'in))
  (define n (make-node 'd (delay-operator 10 #:clock 'processing-time) (list src) #:sink? #t))
  (define r (run (make-program (list n))
                 (list (arrive 1 'in (pt "a" 500)) (arrive 4 'in (pt "b" 0)) (advance-to 30))))
  (check-equal? (map event-id (run-result-events r)) '("a" "b"))
  (check-equal? (map output-at (run-result-outputs r)) '(11 14)))

(spec-test 'TM.idle "noticing silence needs a processing-time or hybrid timer"
  (define (idle-run clock)
    (define src (make-source 'in))
    (define n (make-node 'idle (idle-operator 60 #:clock clock #:key (λ (e) 'k)) (list src) #:sink? #t))
    (run (make-program (list n))
         (list (arrive 0 'in (pt "a" 0)) (arrive 30 'in (pt "b" 10)) (advance-to 200)
               (arrive 300 'in (pt "c" 300)) (advance-to 500))))
  (define p (idle-run 'processing-time))
  (check-equal? (map output-at (run-result-outputs p)) '(90 360))
  (define h (idle-run 'hybrid))
  ;; hybrid: the newest event time (10) plus 60, compared with the clock
  (check-equal? (map output-at (run-result-outputs h)) '(70 360))
  (check-equal? (map (λ (e) (event-attr e 'since)) (run-result-events h)) '(10 300)))
