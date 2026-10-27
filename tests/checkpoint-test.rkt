#lang racket/base
;; TM.checkpoint and TM.replay.
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/time.rkt"
         "../private/event.rkt"
         "../private/dataflow.rkt"
         "../private/context.rkt"
         "../private/window.rkt"
         "../private/derive.rkt"
         "../private/ops/structure.rkt"
         "../private/rules.rkt"
         "../private/rules-engine.rkt")

(define (contrib dev ts te n) (make-batch-event 'C `((dev . ,dev) (sensor . ,(string-append dev "-s")) (kind . load)) #:ts ts #:te te #:n n))

;; arrivals → high-water-mark merge → rules (accumulate under a cap, silence)
;; and a tumbling window over the merged contributions
(define (program)
  (define src (make-source 'in #:watermark 'ascending))
  (define merge (make-node 'merge (monotonic-merge-operator (λ (e) (event-attr e 'sensor))) (list src)))
  (define rules (make-node 'rules (rules-operator #:key (λ (e) (event-attr e 'dev))) (list merge) #:sink? #t))
  (define win (make-node 'win (grid-window-operator (tumbling 50) contents-accumulator #:key (λ (e) (event-attr e 'dev)))
                         (list merge) #:sink? #t))
  (make-program (list rules win)))

(define the-rules
  (list (make-rule "acc" #:condition (condition 'accumulate (hasheq 'count 3 'span 0))
                   #:modifiers (hasheq 'rate-limit (cons 30 'defer)))
        (make-rule "quiet" #:condition (condition 'silence (hasheq 'after 120 'recovery #t)))))

(define (push-rules [rules the-rules]) (control (rule-source-msg 'alarms rules 'replace) #:config? #t))

(define (observed r)
  (for/list ([o (dedupe-outputs (run-result-outputs r))])
    (define e (output-item o))
    (list (output-at o) (output-node o) (event-type e) (event-ts e) (event-te e) (event-attr/default e 'reason #f)
          (event-attr/default e 'samples (event-attr/default e 'count #f)))))

(define gen:arrivals
  (gen:let ([xs (gen:list (gen:tuple (gen:one-of '("d1" "d2")) (gen:integer-in 1 40) (gen:integer-in 1 3)) #:max-length 14)])
    (let loop ([xs xs] [t 0] [acc '()])
      (if (null? xs) (reverse acc)
          (let* ([x (car xs)] [t2 (+ t (cadr x))])
            (loop (cdr xs) t2 (cons (arrive t2 'in (contrib (car x) t2 t2 (caddr x))) acc)))))))

(spec-test 'TM.checkpoint "checkpoint, crash, restore: outputs repeat only within the last snapshot interval"
  (define arrivals (for/list ([t '(10 20 30 40 50 60 70 80 90 300)] [i (in-naturals)])
                     (arrive t 'in (contrib (if (even? i) "d1" "d2") t t 1))))
  (define plain (run (program) (cons (push-rules) arrivals)))
  (define-values (before after) (split-at arrivals 4))
  (define-values (lost rest) (split-at after 3))
  (define crashed (run (program) (append (list (push-rules)) before (list (checkpoint)) lost (list (restore)) rest)))
  ;; the steps after the checkpoint ran twice, so their outputs repeat ...
  (check-true (> (length (run-result-outputs crashed)) (length (run-result-outputs plain))))
  ;; ... and downstream deduplication by output identity removes the repeats
  (check-equal? (observed crashed) (observed plain)))

(spec-test 'TM.checkpoint "property: checkpoint, restore, then continue equals the uninterrupted run, apart from repeats"
  (check-property
   (make-config #:tests (trials))
   (property ([arrivals gen:arrivals] [i (gen:integer-in 0 14)] [j (gen:integer-in 0 14)])
     (define n (length arrivals))
     (define a (min i n)) (define b (min (max i j) n))
     (define steps (append (list (push-rules)) (take arrivals a) (list (checkpoint)) (take (drop arrivals a) (- b a))
                           (list (restore)) (drop arrivals b) (list (advance-to 2000))))
     (check-equal? (observed (run (program) steps))
                   (observed (run (program) (append (list (push-rules)) arrivals (list (advance-to 2000)))))))))

(spec-test '(TM.replay OUT.coalesce) "a cold start replays retained input, holds outputs, and releases one per instance after the rules push"
  (define steps (list (push-rules)
                      (arrive 10 'in (contrib "d1" 10 10 1)) (arrive 20 'in (contrib "d1" 20 20 1))
                      (arrive 25 'in (contrib "d1" 25 25 1))          ; acc fires at 25
                      (arrive 40 'in (contrib "d1" 40 40 3))          ; ready, capped until 55
                      (cold-start)                                    ; at 40: rebuild from the log
                      (push-rules)))                                  ; the configuration lands
  (define r (run (program) steps #:final-watermark? #f))
  (define rules-out (filter (λ (o) (eq? (output-node o) 'rules)) (run-result-outputs r)))
  ;; before the crash: the samples output at 25; after the cold start the
  ;; replay recomputes it, holds it, and releases it at the push (at 40)
  (check-equal? (map output-at rules-out) '(25 40))
  (define orig (output-item (car rules-out)))
  (define again (output-item (last rules-out)))
  (check-equal? (event-attr again 'window) '(10 25))
  ;; the same content, so downstream deduplication drops the repeat
  (check-equal? (event-attr again 'identity) (event-attr orig 'identity)))

(spec-test '(TM.replay OUT.coalesce) "outputs one instance produced during the replay are coalesced into one"
  (define steps (list (push-rules (list (make-rule "w" #:condition (condition 'window-close (hasheq 'size 10 'offset 0 'mode 'any)))))
                      (arrive 5 'in (contrib "d1" 5 5 1)) (arrive 15 'in (contrib "d1" 15 15 1))
                      (arrive 25 'in (contrib "d1" 25 25 1)) (arrive 35 'in (contrib "d1" 35 35 1))
                      (cold-start)
                      (push-rules (list (make-rule "w" #:condition (condition 'window-close (hasheq 'size 10 'offset 0 'mode 'any)))))))
  (define r (run (program) steps #:final-watermark? #f))
  (define after (last (filter (λ (o) (eq? (output-node o) 'rules)) (run-result-outputs r))))
  (define e (output-item after))
  (check-equal? (list (output-at after) (event-attr e 'window) (event-attr e 'windows) (event-attr e 'merged) (event-attr e 'samples))
                ;; three windows merged; their samples are all four replayed contributions
                '(35 (0 30) 3 3 4)))

(spec-test 'TM.replay "after a replay each held output is judged against the instances then in force"
  (define steps (list (push-rules)
                      (arrive 10 'in (contrib "d1" 10 10 3))
                      (cold-start)
                      (push-rules (list (cadr the-rules)))))         ; the accumulate rule is gone
  (define r (run (program) steps #:final-watermark? #f))
  (check-equal? (for/list ([o (run-result-outputs r)] #:when (eq? (output-node o) 'rules))
                  (list (output-at o) (event-attr (output-item o) 'rule)))
                '((10 "acc")))
  (check-equal? (run-result-counter r 'rules 'dropped-after-replay) 1))

(spec-test '(TM.replay PT.silence TM.idle) "processing-time timers stay suspended during the replay"
  ;; silence after 120: replaying the arrival at 10 must not be mistaken for a silence
  (define steps (list (push-rules (list (cadr the-rules)))
                      (arrive 10 'in (contrib "d1" 10 10 1))
                      (advance-to 100)
                      (cold-start)
                      (push-rules (list (cadr the-rules)))
                      (advance-to 200)))
  (define r (run (program) steps #:final-watermark? #f))
  (check-equal? (for/list ([o (run-result-outputs r)] #:when (eq? (output-node o) 'rules))
                  (list (output-at o) (event-attr (output-item o) 'reason)))
                '((130 silence))))
