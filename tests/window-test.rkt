#lang racket/base
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/time.rkt"
         "../private/event.rkt"
         "../private/operator.rkt"
         "../private/dataflow.rkt"
         "../private/progress.rkt"
         "../private/window.rkt")

(define (pt id t [payload '()] #:te [te t] #:n [n 1]) (make-event 'X payload #:id id #:ts t #:te te #:n n))
(define (wins r) (for/list ([e (run-result-events r)])
                   (list (event-ts e) (event-te e) (event-attr e 'count))))

(define (run-window op steps #:final? [final? #t])
  (define src (make-source 'in))
  (define n (make-node 'w op (list src) #:sink? #t))
  (run (make-program (list n)) steps #:final-watermark? final?))

(spec-test 'WN.tumbling "tumbling windows fire when the watermark reaches their end"
  (define r (run-window (grid-window-operator (tumbling 10) contents-accumulator)
                        (list (arrive 1 'in (pt "a" 1)) (arrive 2 'in (pt "b" 5)) (arrive 3 'in (pt "c" 12))
                              (watermark-to 'in 10) (arrive 4 'in (pt "d" 25)) (watermark-to 'in 30))
                        #:final? #f))
  (check-equal? (wins r) '((0 10 2) (10 20 1) (20 30 1)))
  (check-equal? (map output-at (run-result-outputs r)) '(3 4 4))
  (define first (car (run-result-events r)))
  (check-equal? (map event-id (event-constituents first)) '("a" "b")))

(spec-test 'WN.tumbling "the first window is the one containing the first event; offsets shift the grid"
  (define r (run-window (grid-window-operator (tumbling 10 #:offset 3) contents-accumulator)
                        (list (arrive 1 'in (pt "a" 25)) (arrive 2 'in (pt "early" 4)) (arrive 3 'in (pt "b" 34)))))
  (check-equal? (wins r) '((23 33 1) (33 43 1)))
  (check-equal? (run-result-counter r 'w 'before-first-window) 1))

(spec-test 'WN.sliding "a sliding window event belongs to at most ⌈Δ/σ⌉ windows"
  (check-equal? (grid-starts-at (sliding 10 5) 7) '(0 5))
  (check-equal? (grid-starts-at (sliding 10 3) 7) '(0 3 6))
  (define r (run-window (grid-window-operator (sliding 10 5) contents-accumulator)
                        (list (arrive 1 'in (pt "a" 7)) (arrive 2 'in (pt "b" 12)))))
  (check-equal? (wins r) '((0 10 1) (5 15 2) (10 20 1))))

(spec-test 'WN.empty "with fire-empty, every grid window from the first fires, even when empty"
  (define op (grid-window-operator (tumbling 10) contents-accumulator #:fire-empty? #t))
  (define r (run-window op (list (arrive 1 'in (pt "a" 1)) (arrive 2 'in (pt "b" 35)) (watermark-to 'in 40))
                        #:final? #f))
  (check-equal? (wins r) '((0 10 1) (10 20 0) (20 30 0) (30 40 1)))
  (check-equal? (event-constituents (second (run-result-events r))) '())
  ;; without it, empty windows emit nothing
  (define r2 (run-window (grid-window-operator (tumbling 10) contents-accumulator)
                         (list (arrive 1 'in (pt "a" 1)) (arrive 2 'in (pt "b" 35)) (watermark-to 'in 40))
                         #:final? #f))
  (check-equal? (wins r2) '((0 10 1) (30 40 1))))

(spec-test '(TM.lateness TM.retraction) "allowed lateness updates a fired window with a correction"
  (define op (grid-window-operator (tumbling 10) contents-accumulator #:lateness 10))
  (define r (run-window op (list (arrive 1 'in (pt "a" 1)) (watermark-to 'in 12)
                                 (arrive 2 'in (pt "b" 8))      ; late, within L: correction
                                 (watermark-to 'in 20)
                                 (arrive 3 'in (pt "c" 9)))     ; past e + L: dropped and counted
                        #:final? #f))
  (define items (map output-item (run-result-outputs r)))
  (check-equal? (length items) 3)
  (check-true (retraction? (second items)))
  (check-equal? (event-attr (retraction-event (second items)) 'count) 1)
  (check-equal? (event-attr (third items) 'count) 2)
  (check-equal? (run-result-counter r 'w 'late) 1))

(spec-test 'TM.lateness "without allowed lateness, an event for a complete window is late"
  (define r (run-window (grid-window-operator (tumbling 10) contents-accumulator)
                        (list (arrive 1 'in (pt "a" 1)) (watermark-to 'in 15) (arrive 2 'in (pt "b" 3)))))
  (check-equal? (wins r) '((0 10 1)))
  (check-equal? (run-result-counter r 'w 'late) 1))

(spec-test '(WN.tumbling TM.processing-time) "a processing-time window assigns by arrival and fires on the clock"
  (define op (grid-window-operator (tumbling 60) contents-accumulator #:closer 'clock))
  (define r (run-window op (list (arrive 10 'in (pt "a" 999)) (arrive 50 'in (pt "b" 0)) (arrive 70 'in (pt "c" 5))
                                 (advance-to 200))))
  (check-equal? (wins r) '((0 60 2) (60 120 1)))
  (check-equal? (map output-at (run-result-outputs r)) '(60 120)))

(spec-test '(TM.keyed-watermark WN.tumbling) "windows close per key under a keyed watermark (trace R3 geometry)"
  (define (c kind ts te n) (make-batch-event 'C `((dev . "d1") (kind . ,kind)) #:ts ts #:te te #:n n))
  (define op (grid-window-operator (tumbling 60) contents-accumulator
                                   #:key (λ (e) (event-attr e 'dev))
                                   #:closer (list 'keyed 'all '(load position) (λ (e) (event-attr e 'kind)))
                                   #:fire-empty? #t))
  (define r (run-window op (for/list ([e (list (c 'load 0 50 50) (c 'position 0 70 70) (c 'load 51 130 80)
                                               (c 'position 71 250 180) (c 'load 131 300 170))]
                                      [i (in-naturals)])
                             (arrive i 'in e))
                        #:final? #f))
  (check-equal? (for/list ([o (run-result-outputs r)]) (list (output-at o) (event-ts (output-item o))))
                '((2 0) (3 60) (4 120) (4 180))))

(spec-test 'WN.tumbling "interval events are assigned by start, by end, or by overlap"
  (define (go mode) (wins (run-window (grid-window-operator (tumbling 10) contents-accumulator #:assign mode)
                                      (list (arrive 1 'in (pt "a" 5 #:te 25))))))
  (check-equal? (go 'start) '((0 10 1)))
  (check-equal? (go 'end) '((20 30 1)))
  (check-equal? (go 'overlap) '((0 10 1) (10 20 1) (20 30 1))))

(spec-test 'WN.tumbling "property: tumbling windows partition the events"
  (check-property
   (make-config #:tests (trials))
   (property ([ts (gen:list (gen:integer-in 0 100) #:max-length 20)] [size (gen:integer-in 1 30)])
     (define es (for/list ([t (sort ts <)] [i (in-naturals)]) (pt (format "e~a" i) t)))
     (define r (run-window (grid-window-operator (tumbling size) contents-accumulator)
                           (for/list ([e es] [i (in-naturals)]) (arrive i 'in e))))
     (check-equal? (apply + (map third (wins r))) (length es))
     (for ([w (wins r)]) (check-equal? (- (second w) (first w)) size)))))

(spec-test 'WN.session "sessions are maximal runs with gaps below g; an arrival may merge two"
  (define op (session-window-operator 10 contents-accumulator #:key (λ (e) 'user)))
  (define r (run-window op (list (arrive 1 'in (pt "a" 0)) (arrive 2 'in (pt "b" 5))
                                 (arrive 3 'in (pt "c" 30)) (arrive 4 'in (pt "d" 50)))))
  (check-equal? (wins r) '((0 15 2) (30 40 1) (50 60 1)))
  ;; an out-of-order event bridging two open sessions merges them
  (define r2 (run-window op (list (arrive 1 'in (pt "a" 0)) (arrive 2 'in (pt "c" 16)) (arrive 3 'in (pt "b" 8)))))
  (check-equal? (wins r2) '((0 26 3))))

(spec-test 'WN.session "sessions fire when the watermark passes last + g"
  (define op (session-window-operator 10 contents-accumulator))
  (define r (run-window op (list (arrive 1 'in (pt "a" 0)) (arrive 2 'in (pt "b" 5)) (watermark-to 'in 15)
                                 (arrive 3 'in (pt "c" 40)))
                        #:final? #f))
  (check-equal? (wins r) '((0 15 2)))
  (check-equal? (map output-at (run-result-outputs r)) '(2)))

(spec-test '(WN.count AG.incremental) "count windows hold the last n events and advance every m"
  (define r (run-window (count-window-operator 3 2 contents-accumulator)
                        (for/list ([i 7]) (arrive i 'in (pt (format "e~a" i) (* 10 i) #:n (add1 i))))))
  ;; windows end at the 4th and 6th events (the 2nd is not yet full)
  (check-equal? (map (λ (e) (list (event-ts e) (event-te e) (event-attr e 'count) (event-attr e 'events)))
                     (run-result-events r))
                '((10 30 9 3) (30 50 15 3))))

(spec-test 'WN.landmark "a landmark window grows from t0 forever"
  (define r (run-window (landmark-window-operator 100 contents-accumulator)
                        (list (arrive 1 'in (pt "early" 50)) (arrive 2 'in (pt "a" 120)) (arrive 3 'in (pt "b" 160)))))
  (check-equal? (wins r) '((100 120 1) (100 160 2)))
  (check-equal? (run-result-counter r 'w 'before-landmark) 1))

(define (shift kind t) (pt (format "~a~a" kind t) t `((kind . ,kind))))
(define (kind? k) (λ (e) (eq? (event-attr/default e 'kind #f) k)))

(spec-test 'WN.punctuated "a punctuated window spans from an opening event to a closing one or a timeout"
  (define op (punctuated-window-operator (kind? 'start) (kind? 'end) contents-accumulator #:timeout 100))
  (define r (run-window op (list (arrive 1 'in (shift 'start 10)) (arrive 2 'in (shift 'work 20))
                                 (arrive 3 'in (shift 'work 30)) (arrive 4 'in (shift 'end 40))
                                 (arrive 5 'in (shift 'work 50))
                                 (arrive 6 'in (shift 'start 60)) (arrive 7 'in (shift 'work 70)))))
  (check-equal? (map (λ (e) (list (event-ts e) (event-te e) (event-attr e 'count) (event-attr e 'reason)))
                     (run-result-events r))
                '((10 40 2 closed) (60 160 1 timeout)))
  (check-equal? (map event-id (event-constituents (car (run-result-events r))))
                '("start10" "work20" "work30" "end40"))
  (check-equal? (run-result-counter r 'w 'outside) 1))

(spec-test 'WN.since-last "a since-last window closes when the instance fires and reopens after"
  (define op (since-last-window-operator (λ (res) (>= (hash-ref res 'count) 3)) contents-accumulator))
  (define r (run-window op (for/list ([t '(0 10 20 25 40 50)] [n '(1 1 1 2 1 1)] [i (in-naturals)])
                             (arrive t 'in (pt (format "e~a" i) t #:n n)))))
  (check-equal? (wins r) '((0 20 3) (25 40 3))))
