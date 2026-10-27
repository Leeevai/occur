#lang racket/base
;; Acceptance, spec §16.7: the Reflex traces R1–R6, through lib/reflex.rkt
;; and the programs in examples/reflex/. Every output is checked exactly:
;; reason, window, samples, windows, params, and emission time.
(require rackunit
         racket/list
         "util.rkt"
         "../main.rkt")

;; The triggers of a run: (emission time, reason, window, samples, windows,
;; params), each emitted when it says it was.
(define (triggers r)
  (for/list ([o (run-result-outputs r)] #:when (eq? (output-node o) 'triggers))
    (define e (output-item o))
    (check-equal? (output-at o) (event-attr e 'emitted-at))
    (list (event-attr e 'emitted-at) (event-attr e 'reason) (event-attr e 'interval)
          (event-attr e 'samples) (event-attr e 'windows) (event-attr e 'params))))

(require (prefix-in r1: "../examples/reflex/r1-accumulate.rkt"))

(spec-test '(PT.accumulate OUT.rate-limit WN.since-last AG.multiplicity AG.span)
           "trace R1: accumulate with a deferring rate limit; the capped data rides in the output at 80"
  (check-equal? (triggers (r1:occur-run))
                '((20 samples (0 20) 3 1 #hasheq())
                  (80 samples (25 50) 4 1 #hasheq()))))

(require (prefix-in r2: "../examples/reflex/r2-merge.rkt"))

(spec-test '(ST.monotonic-merge ES.batch)
           "trace R2: each arrival's share past the high-water mark, the marks, and the late counts"
  (define (contributions r)
    (for/list ([e (run-result-events r 'contributions)]) (list (event-ts e) (event-te e) (event-n e))))
  (check-equal? (contributions (r2:occur-run)) '((90 100 10) (100 105 5)))
  ;; row by row: run the setup and the first k arrivals, then read the mark
  ;; and the late count of d1's sensor
  (define steps (r2:occur-steps))
  (define setup (filter (λ (s) (not (step:arrive? s))) steps))
  (define arrivals (filter step:arrive? steps))
  (define rows
    (for/list ([k (in-range 1 5)])
      (define r (run (r2:occur-program) (append setup (take arrivals k)) #:final-watermark? #f))
      (define marks (hash-ref (cdr (hash-ref (run-result-states r) 'contributions)) "d1"))
      (define st (hash-ref marks (cons "d1" "d1-load")))
      (list (hwm-state-hw st) (hwm-state-late st) (run-result-counter r 'contributions 'late))))
  (check-equal? rows '((100 0 0) (105 5 5) (105 15 15) (105 25 25))))

(require (prefix-in r3: "../examples/reflex/r3-window.rkt"))

(define (triggers-for r target)
  (for/list ([o (run-result-outputs r)]
             #:when (and (eq? (output-node o) 'triggers) (equal? (event-attr (output-item o) 'target) target)))
    (define e (output-item o))
    (list (event-attr e 'emitted-at) (event-attr e 'reason) (event-attr e 'interval)
          (event-attr e 'samples) (event-attr e 'windows) (event-attr e 'params))))

(spec-test '(PT.window-close WN.tumbling TM.keyed-watermark OUT.coalesce)
           "trace R3: a require_all window closes under W_all, coalescing windows that close together"
  (define r (r3:occur-run))
  (check-equal? (triggers-for r "all-kinds-job")
                '((3 window (0 60) 200 1 #hasheq())
                  (4 window (60 120) 180 1 #hasheq())
                  (5 window (120 240) 170 2 #hasheq())))
  ;; under mode any, [0, 60] closes at step 2, because W_any = 70 there
  (check-equal? (car (triggers-for r "any-kind-job")) '(2 window (0 60) 120 1 #hasheq())))

(require (prefix-in r4: "../examples/reflex/r4-silence.rkt"))

(spec-test '(PT.silence PT.recovery PT.activation OUT.edge CX.activation TM.idle TM.clock)
           "trace R4: silence counted from activation, with recovery, edge-triggered on the hybrid clock"
  (check-equal? (map (λ (t) (take t 3)) (triggers (r4:occur-run)))
                '((600 silence (none 600))
                  (900 recovered (none 890))
                  (1490 silence (890 1490))
                  (1600 recovered (890 1600)))))

(require (prefix-in r5: "../examples/reflex/r5-cron.rkt"))

(define (hm h m) (utc 2026 1 5 h m))

(spec-test '(PT.schedule WN.calendar WN.empty TM.schedule OUT.coalesce)
           "trace R5: missed hourly ticks coalesce into one output, and an empty hour still fires"
  (check-equal? (triggers (r5:occur-run))
                (list (list (hm 13 10) 'schedule (list (hm 10 0) (hm 13 0)) 5 3 #hasheq())
                      (list (hm 14 0) 'schedule (list (hm 13 0) (hm 14 0)) 0 1 #hasheq()))))
