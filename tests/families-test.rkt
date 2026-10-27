#lang racket/base
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/time.rkt"
         "../private/event.rkt"
         "../private/dataflow.rkt"
         "../private/window.rkt"
         "../private/pattern/ast.rkt"
         "../private/pattern/reference.rkt"
         "../private/pattern/families.rkt")

(define (ev id type t [payload '()] #:te [te t] #:n [n 1]) (make-event type payload #:id id #:ts t #:te te #:n n))
(define (ids ms) (sort (map pmatch-ids ms) (λ (a b) (string<? (format "~a" a) (format "~a" b)))))

(spec-test 'PT.threshold "a threshold fires when an aggregate of the matched events crosses k"
  (define H (for/list ([a '(400 300 500)] [i (in-naturals)]) (ev (format "s~a" i) 'Swipe i `((amount . ,a)))))
  (define p (threshold-pattern (kleene+ (prim 'Swipe #:bind 'b))
                               (λ (bs) (apply + (map (λ (b) (event-attr b 'amount)) (hash-ref bs 'b)))) '> 1000))
  (check-equal? (ids (denote p H)) '(("s0" "s1" "s2")))
  (check-true (crosses? 101 '> 100))
  (check-false (crosses? #f '> 100)))

(spec-test '(PT.trend.increasing PT.trend.decreasing PT.trend.non-increasing PT.trend.non-decreasing
             PT.trend.stable PT.trend.mixed)
           "trends classify the successive values of a Kleene binding"
  (check-true (increasing? '(1 2 5)))
  (check-false (increasing? '(1 2 2)))
  (check-true (decreasing? '(9 4 1)))
  (check-true (non-increasing? '(9 9 1)))
  (check-true (non-decreasing? '(1 1 2)))
  (check-true (stable? '(20.1 20.3 20.2) 0.5))
  (check-true (mixed? '(1 3 2)))
  (check-false (mixed? '(1 2 3)))
  (check-equal? (map trend '((1 2 3) (3 2 1) (5 5 5) (1 1 2) (2 2 1) (1 3 2)))
                '(increasing decreasing stable non-decreasing non-increasing mixed)))

(spec-test 'PT.relative "a binding is the maximum or minimum among the matched events"
  (define readings (for/list ([v '(3 9 4)] [i (in-naturals)]) (ev (format "r~a" i) 'R i `((v . ,v)))))
  (define v (λ (e) (event-attr e 'v)))
  (check-true (relative-max? (second readings) readings v))
  (check-false (relative-max? (first readings) readings v))
  (check-true (relative-min? (first readings) readings v)))

(spec-test 'PT.spatial.distance "distance to an anchor within or beyond r, with min, max and avg"
  (define depot (location 0 0))
  (check-true (within-distance? (location 3 4) depot 5))
  (check-true (beyond-distance? (location 6 8) depot 5))
  (define locs (list (location 3 4) (location 6 8)))
  (check-equal? (list (min-distance locs depot) (max-distance locs depot) (avg-distance locs depot)) '(5 10 15/2)))

(spec-test 'PT.spatial.moving "moving toward, away, or in a constant direction"
  (define depot (location 0 0))
  (define approaching (list (location 10 0) (location 6 0) (location 2 0)))
  (check-true (moving-toward? approaching depot))
  (check-true (moving-away? (reverse approaching) depot))
  (check-true (constant-direction? approaching))
  (check-false (constant-direction? (list (location 0 0) (location 1 0) (location 1 1)))))

(spec-test 'PT.accumulate "accumulate is (b:X)+ where count(b) >= N and span(b) >= S"
  ;; the contributions of trace R1 (§16.7): the first run reaching N = 3 is {0, 10, 20}
  (define cs (list (ev "c0" 'C 0) (ev "c1" 'C 10) (ev "c2" 'C 20) (ev "c3" 'C 25 #:te 30 #:n 2)))
  (define ms (denote (accumulate-pattern 'C 3 0) cs))
  (check-not-false (member '("c0" "c1" "c2") (ids ms)))
  ;; multiplicity counts: one batch of 2 plus one more reaches 3
  (check-not-false (member '("c2" "c3") (ids ms)))
  ;; span: S = 25 needs the run to cover 25 seconds
  (check-false (member '("c0" "c1" "c2") (ids (denote (accumulate-pattern 'C 3 25) cs)))))

(spec-test '(PT.silence PT.activation) "silence fires Δ after the newest data, or after the activation"
  (define p (silence-pattern 'C 600))
  ;; trace R4 (§16.7): activated at 0, data [880, 890], more data [1595, 1600]
  (define cs (list (ev "c1" 'C 880 #:te 890 #:n 2) (ev "c2" 'C 1595 #:te 1600)))
  (define ms (denote p cs #:activation 0))
  (check-equal? (map (λ (m) (list (pmatch-ids m) (pmatch-ts m) (pmatch-te m))) ms)
                '((() 0 600) (("c1") 880 1490) (("c2") 1595 2200))))

(spec-test 'PT.recovery "recovery is the first contribution after a silence"
  (define H (list (ev "s1" 'Silence 600) (ev "c1" 'C 890) (ev "c2" 'C 900)))
  (check-equal? (ids (denote (recovery-pattern 'Silence 'C) H)) '(("s1" "c1") ("s1" "c2")))
  ;; with skip-till-next-match (§12) only ("s1" "c1") remains; see tests/policies-test.rkt
  )

(spec-test '(PT.window-close OUT.coalesce TM.keyed-watermark) "window-close coalesces the windows that closed together (trace R3)"
  (define (c kind ts te n) (ev (format "~a~a" kind ts) 'C ts `((dev . "d1") (kind . ,kind)) #:te te #:n n))
  (define op (window-close-operator 60 #:kind (λ (e) (event-attr e 'kind)) #:key (λ (e) (event-attr e 'dev))
                                    #:kinds '(load position) #:mode 'all))
  (define arrivals (list (c 'load 0 50 50) (c 'position 0 70 70) (c 'load 51 130 80)
                         (c 'position 71 250 180) (c 'load 131 300 170)))
  (define r (run-operator op (list arrivals) #:arrivals (list '(1 2 3 4 5)) #:watermark 'none))
  (check-equal? (map (λ (e) (list (event-ts e) (event-te e) (event-attr e 'windows) (event-attr e 'count)))
                     (run-result-events r))
                '((0 60 1 200) (60 120 1 180) (120 240 2 170)))
  ;; under mode any, [0, 60) closes at the second step
  (define any-op (window-close-operator 60 #:kind (λ (e) (event-attr e 'kind)) #:key (λ (e) (event-attr e 'dev))
                                        #:kinds '(load position) #:mode 'any))
  (define r2 (run-operator any-op (list (take arrivals 2)) #:arrivals (list '(1 2)) #:watermark 'none))
  (check-equal? (map event-te (run-result-events r2)) '(60)))

(spec-test 'PT.schedule "a schedule is a processing-time calendar window, fired even when empty"
  (define op (calendar-window-operator "0 * * * *" contents-accumulator))
  (define src (make-source 'in))
  (define n (make-node 'sched op (list src) #:sink? #t))
  (define r (run (make-program (list n)) (list (advance-to (utc 2026 1 5 12 30)))
                 #:clock (simulated-clock (utc 2026 1 5 10 20)) #:final-watermark? #f))
  (check-equal? (map (λ (e) (list (event-ts e) (event-te e) (event-attr e 'count))) (run-result-events r))
                (list (list (utc 2026 1 5 10 0) (utc 2026 1 5 11 0) 0)
                      (list (utc 2026 1 5 11 0) (utc 2026 1 5 12 0) 0))))

(spec-test 'PT.trend.increasing "property: increasing implies non-decreasing, and stable(0) means constant"
  (check-property
   (make-config #:tests (trials))
   (property ([xs (gen:list (gen:integer-in 0 5) #:max-length 6)])
     (when (increasing? xs) (check-true (non-decreasing? xs)))
     (when (decreasing? xs) (check-true (non-increasing? xs)))
     (check-equal? (stable? xs 0) (<= (length (remove-duplicates xs)) 1))
     (check-not-false (memq (trend xs) '(increasing decreasing stable non-decreasing non-increasing mixed))))))
