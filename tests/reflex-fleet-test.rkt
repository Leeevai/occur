#lang racket/base
;; The fleet of examples/reflex/fleet.rkt: the behaviors of spec §16.1–§16.3
;; on arrivals and rules that come as data.
(require rackunit
         racket/list
         "util.rkt"
         "../main.rkt"
         (prefix-in fleet: "../examples/reflex/fleet.rkt"))

(define r (fleet:occur-run))
(define (hm h m) (utc 2026 1 5 h m))
(define (rows #:device [device #f] #:target [target #f])
  (for/list ([o (run-result-outputs r)]
             #:when (eq? (output-node o) 'triggers)
             #:when (or (not device) (equal? (event-attr (output-item o) 'device) device))
             #:when (or (not target) (equal? (event-attr (output-item o) 'target) target)))
    (define e (output-item o))
    (list (output-at o) (event-attr e 'binding-key) (event-attr e 'reason) (event-attr e 'interval)
          (event-attr e 'samples) (event-attr e 'params))))
(define (total name) (for/sum ([c (in-hash-values (run-result-counters r))]) (hash-ref c name 0)))

(spec-test '(RL.binding RL.fan-out RL.identity PT.flush OUT.rate-limit)
           "a template bound to both pumps and again to pump-2: one instance per (rule, device, binding key)"
  (check-equal? (rows #:target "pressure-evaluator")
                (list (list (hm 0 5) "strict" 'samples (list (hm 0 0) (hm 0 5)) 6 #hasheq((threshold . 40)))
                      (list (hm 0 10) "standard" 'samples (list (hm 0 0) (hm 0 10)) 12 #hasheq((threshold . 50)))
                      ;; pump-2's standard instance is never ready: flushed 15 minutes after its data
                      (list (hm 0 20) "standard" 'max_delay (list (hm 0 0) (hm 0 5)) 6 #hasheq((threshold . 50)))
                      ;; pump-1's next data at 01:00 is flushed 15 minutes after it arrived
                      (list (hm 1 15) "standard" 'max_delay (list (hm 0 55) (hm 1 0)) 3 #hasheq((threshold . 50))))))

(spec-test '(RL.source RL.ownership RL.dangling)
           "a JSON rule source: its rule is bound, a claimed id is refused, and a missing rule's binding is reported"
  (check-equal? (rows #:target "vibration-evaluator")
                (list (list (hm 0 16) "" 'samples (list (hm 0 12) (hm 0 16)) 3 #hasheq())))
  (check-equal? (for/list ([rep (map cdr (run-result-reports r))])
                  (list (rules-report-kind rep) (rules-report-source rep)))
                '((refused-owner vendor) (dangling vendor))))

(spec-test '(PT.window-close PT.silence PT.recovery PT.schedule CX.activation)
           "the tank's window, every device's connectivity, and the nightly summary"
  (check-equal? (rows #:target "tank-report")
                (list (list (hm 0 20) "" 'window (list (hm 0 0) (hm 0 15)) 4 #hasheq())))
  (check-equal? (map (λ (x) (list (first x) (third x) (fourth x))) (rows #:device "pump-1" #:target "connectivity"))
                (list (list (hm 0 46) 'silence (list (hm 0 16) (hm 0 46)))
                      (list (hm 1 0) 'recovered (list (hm 0 16) (hm 1 0)))
                      (list (hm 1 30) 'silence (list (hm 1 0) (hm 1 30)))))
  (check-equal? (for/list ([x (rows #:target "nightly-summary")]) (list (first x) (fourth x) (fifth x)))
                (for/list ([n '(6 18 4)])
                  (list (hm 2 0) (list (utc 2026 1 4 2 0) (hm 2 0)) n))))

(spec-test '(CX.entity TM.skew-guard ST.monotonic-merge ES.keyed)
           "the deprecated device is ignored, the far-future write dropped, and the repeated write merged away"
  (check-false (for/or ([o (run-result-outputs r)]) (equal? (event-attr/default (output-item o) 'device #f) "old-1")))
  (check-equal? (list (total 'ignored) (total 'future-skew) (total 'late)) '(1 1 6)))
