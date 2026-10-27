#lang racket/base
;; The rules engine (spec §14, §11.4): carry, re-judge, flush, convergence.
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/time.rkt"
         "../private/event.rkt"
         "../private/dataflow.rkt"
         "../private/context.rkt"
         "../private/rules.rkt"
         "../private/rules-engine.rkt")

(define (contrib ts te n #:kind [kind 'load] #:dev [dev "d1"])
  (make-batch-event 'Contribution `((dev . ,dev) (kind . ,kind)) #:ts ts #:te te #:n n))
(define (program)
  (define src (make-source 'contribs))
  (define gate (make-node 'gate (entity-context-operator 'devices #:key (λ (e) (event-attr e 'dev))) (list src)))
  (make-program (list (make-node 'rules (rules-operator #:key (λ (e) (event-attr e 'dev))) (list gate) #:sink? #t))))
(define (push items #:at [t #f] #:name [name 'alarms] #:mode [mode 'replace])
  (control (rule-source-msg name items mode) #:at t #:config? #t))
(define (go steps) (run (program) steps #:final-watermark? #f))
(define (triggers r)
  (for/list ([e (run-result-events r)])
    (list (event-attr e 'emitted-at) (event-attr e 'reason) (event-attr e 'window) (event-attr e 'samples))))

(define (acc n #:flush [flush #f] #:retain? [retain? #f] #:cap [cap #f] #:params [ps (hasheq)] #:id [id "acc"])
  (make-rule id #:condition (condition 'accumulate (hasheq 'count n 'span 0))
             #:modifiers (for/hasheq ([kv (list (and flush (cons 'flush flush)) (and retain? (cons 'flush-retain? #t))
                                               (and cap (cons 'rate-limit cap)))] #:when kv)
                           (values (car kv) (cdr kv)))
             #:params ps))

(spec-test '(PT.flush OUT.timeout-partial TM.clock) "max_delay flushes a partial accumulation on the processing clock"
  (define r (go (list (push (list (acc 5 #:flush 100)) #:at 0)
                      (arrive 10 'contribs (contrib 10 10 1)) (arrive 20 'contribs (contrib 20 20 1)) (advance-to 300))))
  (check-equal? (triggers r) '((110 max_delay (10 20) 2))))

(spec-test '(PT.flush PT.accumulate) "after a samples output, the flush clock starts again at the next contribution"
  ;; samples at 20 consume both contributions; the next one arrives at 500,
  ;; long after 10 + 100, and is flushed at 600, not at once
  (define r (go (list (push (list (acc 2 #:flush 100)) #:at 0)
                      (arrive 10 'contribs (contrib 10 10 1)) (arrive 20 'contribs (contrib 20 20 1))
                      (arrive 500 'contribs (contrib 500 500 1)) (advance-to 700))))
  (check-equal? (triggers r) '((20 samples (10 20) 2) (600 max_delay (500 500) 1))))

(spec-test '(CON.retain PT.flush) "with reset_on_max_delay false the flush retains its contributions"
  (define steps (list (push (list (acc 3 #:flush 100 #:retain? #t)) #:at 0)
                      (arrive 10 'contribs (contrib 10 10 1)) (advance-to 150)
                      (arrive 160 'contribs (contrib 160 160 1)) (arrive 170 'contribs (contrib 170 170 1))))
  (check-equal? (triggers (go steps)) '((110 max_delay (10 10) 1) (170 samples (10 170) 3))))

(spec-test 'OUT.rate-limit "a drop-mode cap discards outputs inside it"
  (define r (go (list (push (list (acc 1 #:cap (cons 60 'drop))) #:at 0)
                      (arrive 0 'contribs (contrib 0 0 1)) (arrive 10 'contribs (contrib 10 10 1))
                      (arrive 70 'contribs (contrib 70 70 1)))))
  (check-equal? (map car (triggers r)) '(0 70)))

(spec-test 'RL.rejudge "a new binding on a key that already has data re-judges at once"
  (define r (go (list (push (list (acc 3)) #:at 0 #:name 'base)
                      (arrive 5 'contribs (contrib 5 5 1))
                      (push (list (make-binding "acc" #:on "d1" #:key "b2" #:params '((t . 1)))) #:at 50 #:name 'extra))))
  (check-equal? (for/list ([e (run-result-events r)]) (list (event-attr e 'binding-key) (event-attr e 'reason)))
                '(("b2" config))))

(spec-test 'RL.rejudge "a re-judge under the cap is superseded when a data output goes first"
  (define r (go (list (push (list (acc 1 #:cap (cons 60 'defer) #:params (hasheq 'v 1))) #:at 0)
                      (arrive 0 'contribs (contrib 0 0 1))
                      (push (list (acc 1 #:cap (cons 60 'defer) #:params (hasheq 'v 2))) #:at 10)   ; config, held
                      (arrive 20 'contribs (contrib 20 20 1))                                       ; data, held: supersedes
                      (advance-to 100))))
  (check-equal? (map (λ (t) (take t 2)) (triggers r)) '((0 samples) (60 samples)))
  (check-equal? (event-attr (last (run-result-events r)) 'params) (hasheq 'v 2)))

(define (win size #:on-update [ou 'continue])
  (make-rule "w" #:condition (condition 'window-close (hasheq 'size size 'offset 0 'mode 'any)) #:on-update ou))

(spec-test 'RL.carry "window progress carries under continue only with the same grid"
  (define (with second-size)
    (go (list (push (list (win 60)) #:at 0)
              (arrive 1 'contribs (contrib 0 30 30))
              (push (list (win second-size)) #:at 2)
              (arrive 3 'contribs (contrib 31 70 39)))))
  ;; an unchanged rule is no edit: the grid anchored at 0 closes [0, 60) at W = 70
  (check-equal? (triggers (with 60)) '((3 window (0 60) 69)))
  ;; a new grid restarts: it anchors at the next contribution and closes [30, 60)
  (check-equal? (triggers (with 30)) '((3 window (30 60) 39))))

(spec-test 'RL.carry "under continue the rate-limit state carries even when progress restarts"
  (define (r policy)
    (go (list (push (list (make-rule "x" #:condition (condition 'accumulate (hasheq 'count 1 'span 0))
                                     #:modifiers (hasheq 'rate-limit (cons 100 'defer)) #:on-update policy)) #:at 0)
              (arrive 0 'contribs (contrib 0 0 1))
              (push (list (make-rule "x" #:condition (condition 'silence (hasheq 'after 1000))
                                     #:modifiers (hasheq 'rate-limit (cons 100 'defer)) #:on-update policy)) #:at 10)
              (push (list (make-rule "x" #:condition (condition 'accumulate (hasheq 'count 1 'span 0))
                                     #:modifiers (hasheq 'rate-limit (cons 100 'defer)) #:on-update policy)) #:at 20)
              (arrive 30 'contribs (contrib 30 30 1)) (advance-to 200))))
  (check-equal? (map car (triggers (r 'continue))) '(0 100))   ; the cap from t = 0 still holds
  (check-equal? (map car (triggers (r 'restart))) '(0 30)))

(spec-test 'RL.dangling "reports of a push name refused, dangling, and invalid items"
  (define r (go (list (push (list (acc 3) (make-binding "nope" #:on "d1")) #:at 0))))
  (check-equal? (map (λ (rep) (rules-report-kind (cdr rep))) (run-result-reports r)) '(dangling)))

(define gen:rules
  (gen:let ([n (gen:integer-in 1 4)] [after (gen:integer-in 50 400)] [cap (gen:one-of '(#f 30 90))])
    (list (acc n #:cap (and cap (cons cap 'defer)))
          (make-rule "q" #:condition (condition 'silence (hasheq 'after after 'recovery #t))))))

(spec-test 'RL.converge "property: replacing a source with identical content changes nothing and emits nothing"
  (check-property
   (make-config #:tests (trials))
   (property ([rules gen:rules] [ts (gen:list (gen:integer-in 0 500) #:max-length 8)] [k (gen:integer-in 0 8)])
     (define arrivals (for/list ([t (sort ts <)]) (arrive t 'contribs (contrib t t 1))))
     (define-values (before after) (split-at arrivals (min k (length arrivals))))
     (define plain (go (append (list (push rules #:at 0)) arrivals (list (advance-to 2000)))))
     (define resynced (go (append (list (push rules #:at 0)) before
                                  (list (push rules)) after (list (advance-to 2000)))))
     (check-equal? (triggers resynced) (triggers plain)))))
