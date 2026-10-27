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
