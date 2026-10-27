#lang racket/base
;; The acceptance traces of §16.7 against the runtime rules engine.
(require rackunit
         racket/list
         "util.rkt"
         "../private/time.rkt"
         "../private/event.rkt"
         "../private/dataflow.rkt"
         "../private/context.rkt"
         "../private/rules.rkt"
         "../private/rules-engine.rkt")

(define (contrib ts te n #:kind [kind 'load] #:dev [dev "d1"])
  (make-batch-event 'Contribution `((dev . ,dev) (kind . ,kind) (substream . ,(format "~a-~a" dev kind)))
                    #:ts ts #:te te #:n n))

(define (program)
  (define src (make-source 'contribs))
  (define gate (make-node 'gate (entity-context-operator 'devices #:key (λ (e) (event-attr e 'dev))) (list src)))
  (define rules (make-node 'rules (rules-operator #:key (λ (e) (event-attr e 'dev))) (list gate) #:sink? #t))
  (make-program (list rules)))

(define (push-rules items #:at [t #f] #:name [name 'alarms] #:mode [mode 'replace])
  (control (rule-source-msg name items mode) #:at t #:config? #t))

(define (triggers r)
  (for/list ([o (run-result-outputs r)] #:when (event? (output-item o)))
    (define e (output-item o))
    (list (output-at o) (event-attr e 'reason) (event-attr e 'window) (event-attr e 'samples) (event-attr e 'windows))))

(define (go steps #:start [start 0])
  (run (program) steps #:clock (simulated-clock start) #:final-watermark? #f))

(define (acc-rule n s #:min-interval [mi #f] #:params [params (hasheq)] #:on-update [ou 'restart])
  (make-rule "acc" #:condition (condition 'accumulate (hasheq 'count n 'span s))
             #:modifiers (if mi (hasheq 'rate-limit (cons mi 'defer)) (hasheq))
             #:params params #:target "evaluator" #:on-update ou))

(spec-test '(PT.accumulate OUT.rate-limit WN.since-last AG.multiplicity AG.span)
           "trace R1: accumulate with a deferring rate limit; the capped data rides in the next output"
  (define r (go (list (push-rules (list (acc-rule 3 0 #:min-interval 60)) #:at 0)
                      (arrive 0 'contribs (contrib 0 0 1)) (arrive 10 'contribs (contrib 10 10 1))
                      (arrive 20 'contribs (contrib 20 20 1)) (arrive 30 'contribs (contrib 25 30 2))
                      (arrive 40 'contribs (contrib 40 40 1)) (arrive 50 'contribs (contrib 50 50 1))
                      (advance-to 100))))
  (check-equal? (triggers r) '((20 samples (0 20) 3 1) (80 samples (25 50) 4 1))))

(spec-test '(PT.window-close TM.keyed-watermark WN.tumbling OUT.coalesce)
           "trace R3: a window rule with require_all closes under W_all and coalesces"
  (define rule (make-rule "win" #:condition (condition 'window-close (hasheq 'size 60 'offset 0 'mode 'all))
                          #:watch '(load position) #:target "card-builder"))
  (define r (go (list (push-rules (list rule) #:at 0)
                      (arrive 1 'contribs (contrib 0 50 50 #:kind 'load))
                      (arrive 2 'contribs (contrib 0 70 70 #:kind 'position))
                      (arrive 3 'contribs (contrib 51 130 80 #:kind 'load))
                      (arrive 4 'contribs (contrib 71 250 180 #:kind 'position))
                      (arrive 5 'contribs (contrib 131 300 170 #:kind 'load)))))
  (check-equal? (triggers r) '((3 window (0 60) 200 1) (4 window (60 120) 180 1) (5 window (120 240) 170 2)))
  ;; under mode any, [0, 60) closes at step 2
  (define any (make-rule "win" #:condition (condition 'window-close (hasheq 'size 60 'offset 0 'mode 'any))
                         #:watch '(load position)))
  (define r2 (go (list (push-rules (list any) #:at 0)
                       (arrive 1 'contribs (contrib 0 50 50 #:kind 'load)) (arrive 2 'contribs (contrib 0 70 70 #:kind 'position)))))
  (check-equal? (map car (triggers r2)) '(2)))

(spec-test '(PT.silence PT.recovery PT.activation OUT.edge CX.activation TM.idle)
           "trace R4: silence counted from activation, with recovery; edge-triggered"
  (define rule (make-rule "quiet" #:condition (condition 'silence (hasheq 'after 600 'recovery #t)) #:target "connectivity"))
  (define r (go (list (push-rules (list rule) #:at 0)
                      (directory-push 'devices '(("d1" . active)) #:at 0)
                      (arrive 900 'contribs (contrib 880 890 2))
                      (arrive 1600 'contribs (contrib 1595 1600 1))
                      (advance-to 1700))))
  (check-equal? (map (λ (t) (take t 3)) (triggers r))
                '((600 silence (none 600)) (900 recovered (none 890)) (1490 silence (890 1490)) (1600 recovered (890 1600)))))

(define (hm h m) (utc 2026 1 5 h m))

(spec-test '(PT.schedule WN.calendar WN.empty TM.schedule OUT.coalesce)
           "trace R5: a cron rule with missed ticks coalesced and an empty window still firing"
  (define rule (make-rule "hourly" #:condition (condition 'schedule (hasheq 'cron "0 * * * *")) #:target "report"))
  (define r (go (list (directory-push 'devices '(("d1" . active)) #:at (hm 10 0))
                      (push-rules (list rule) #:at (hm 10 20))
                      (arrive (hm 10 30) 'contribs (contrib 0 1 2))
                      (arrive (hm 10 45) 'contribs (contrib 2 3 3))
                      (suspend (hm 10 50) (hm 13 10))
                      (advance-to (hm 14 30)))
                #:start (hm 10 0)))
  (check-equal? (triggers r)
                (list (list (hm 13 10) 'schedule (list (hm 10 0) (hm 13 0)) 5 3)
                      (list (hm 14 0) 'schedule (list (hm 13 0) (hm 14 0)) 0 1))))

(define (params-of r) (for/list ([e (run-result-events r)]) (event-attr e 'params)))

(spec-test '(RL.rejudge RL.semantics CON.retain) "trace R6: a job-only edit re-judges with reason config and keeps progress"
  (define before (acc-rule 3 0 #:params (hasheq 'threshold 50)))
  (define after (acc-rule 3 0 #:params (hasheq 'threshold 40)))
  (define r (go (list (push-rules (list before) #:at 0)
                      (arrive 500 'contribs (contrib 500 500 1))
                      (push-rules (list after) #:at 1000)
                      (arrive 1010 'contribs (contrib 510 510 1))
                      (arrive 1020 'contribs (contrib 520 520 1)))))
  (check-equal? (triggers r) '((1000 config (500 500) 0 1) (1020 samples (500 520) 3 1)))
  (check-equal? (params-of r) (list (hasheq 'threshold 40) (hasheq 'threshold 40))))

(spec-test '(RL.carry RL.semantics) "trace R6, variant: a semantic edit carries progress under continue and restarts otherwise"
  (define (variant policy)
    (go (list (push-rules (list (acc-rule 3 0 #:on-update policy)) #:at 0)
              (arrive 500 'contribs (contrib 500 500 1)) (arrive 510 'contribs (contrib 510 510 1))
              (push-rules (list (acc-rule 2 0 #:on-update policy)) #:at 1000))))
  (check-equal? (triggers (variant 'continue)) '((1000 samples (500 510) 2 1)))
  (check-equal? (triggers (variant 'restart)) '()))
