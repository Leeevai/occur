#lang racket/base
;; Rules as data (spec §14): the model.
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/rules.rkt"
         "../private/rules-engine.rkt")

(define acc (condition 'accumulate (hasheq 'count 3 'span 0)))
(define quiet (condition 'silence (hasheq 'after 600 'recovery #t)))

(spec-test 'RL.template "a template names a condition, watched kinds, modifiers, a target, params, and its keys"
  (define r (make-rule "pressure" #:condition acc #:watch '(pressure) #:modifiers (hasheq 'rate-limit (cons 60 'defer))
                       #:target "threshold-evaluator" #:params '((threshold . 50)) #:applies-to '("d1" "d2")))
  (check-equal? (rule-watch r) '(pressure))
  (check-equal? (rule-params r) (hasheq 'threshold 50))
  (define-values (rules bindings reports) (rules-in-force (hash 'alarms (list r))))
  (define-values (on-d1 _1) (instances-for rules bindings "d1"))
  (define-values (on-d9 _2) (instances-for rules bindings "d9"))
  (check-equal? (hash-keys on-d1) '(("pressure" "d1" "")))
  (check-equal? on-d9 (hash)))

(spec-test '(RL.binding RL.identity) "a binding instantiates a template on a key, overriding and adding params"
  (define r (make-rule "pressure" #:condition acc #:params '((threshold . 50) (unit . "bar")) #:applies-to 'none))
  (define b (make-binding "pressure" #:on "d7" #:key "cfg-41" #:overrides '((count . 5)) #:params '((threshold . 40))))
  (define-values (rules bindings reports) (rules-in-force (hash 'alarms (list r b))))
  (define-values (insts _) (instances-for rules bindings "d7"))
  (define sp (hash-ref insts '("pressure" "d7" "cfg-41")))
  (check-equal? (condition-params (ispec-condition sp)) (hasheq 'count 5 'span 0))
  (check-equal? (ispec-params sp) (hasheq 'threshold 40 'unit "bar"))
  ;; a binding with the empty binding key replaces the template's own instance
  (define all (make-rule "pressure" #:condition acc))
  (define own (make-binding "pressure" #:on "d7" #:overrides '((count . 9))))
  (define-values (r2 b2 rep2) (rules-in-force (hash 'alarms (list all own))))
  (define-values (i2 _2) (instances-for r2 b2 "d7"))
  (check-equal? (hash-count i2) 1)
  (check-equal? (hash-ref (condition-params (ispec-condition (hash-ref i2 '("pressure" "d7" "")))) 'count) 9))

(spec-test 'RL.fan-out "a binding may name several keys; each gets its own instance"
  (define r (make-rule "quiet" #:condition quiet #:applies-to 'none))
  (define b (make-binding "quiet" #:on '(12 13 14) #:key "cfg-9" #:overrides '((after . 7200))))
  (define-values (rules bindings _) (rules-in-force (hash 'alarms (list r b))))
  (for ([k '(12 13 14)])
    (define-values (insts _d) (instances-for rules bindings k))
    (check-equal? (hash-keys insts) (list (list "quiet" k "cfg-9"))))
  (check-equal? (sort (named-keys rules bindings) <) '(12 13 14)))

(spec-test 'RL.identity "no two bindings may produce the same instance"
  (define r (make-rule "quiet" #:condition quiet #:applies-to 'none))
  (define b1 (make-binding "quiet" #:on 1 #:key "k"))
  (define b2 (make-binding "quiet" #:on 1 #:key "k" #:overrides '((after . 5))))
  (define-values (rules bindings _) (rules-in-force (hash 'a (list r b1) 'b (list b2))))
  (define-values (insts dups) (instances-for rules bindings 1))
  (check-equal? (hash-count insts) 1)
  (check-equal? (map rules-report-kind dups) '(duplicate-instance)))

(spec-test 'RL.source "sources are replaced whole or patched item by item; the rule set is their union"
  (define r1 (make-rule "a" #:condition acc))
  (define r2 (make-rule "b" #:condition quiet))
  (define s0 (apply-source-msg (hash) (rule-source-msg 'one (list r1) 'replace)))
  (define s1 (apply-source-msg s0 (rule-source-msg 'two (list r2) 'replace)))
  (define-values (rules _b _r) (rules-in-force s1))
  (check-equal? (sort (hash-keys rules) string<?) '("a" "b"))
  (define s2 (apply-source-msg s1 (rule-source-msg 'one (list (removal 'rule "a")) 'patch)))
  (define-values (rules2 _b2 _r2) (rules-in-force s2))
  (check-equal? (hash-keys rules2) '("b"))
  (define s3 (apply-source-msg s2 (rule-source-msg 'two '() 'replace)))
  (define-values (rules3 _b3 _r3) (rules-in-force s3))
  (check-equal? rules3 (hash)))

(spec-test 'RL.ownership "a rule belongs to one source; defining it in a second is refused; bindings may cross"
  (define r (make-rule "a" #:condition acc #:applies-to 'none))
  (define-values (rules bindings reports)
    (rules-in-force (hash 'one (list r) 'two (list (make-rule "a" #:condition quiet) (make-binding "a" #:on "d1")))))
  (check-equal? (condition-kind (rule-condition (hash-ref rules "a"))) 'accumulate)
  (check-equal? (map rules-report-kind reports) '(refused-owner))
  (check-equal? (length bindings) 1))

(spec-test 'RL.dangling "dangling and invalid items are left out and reported; the rest applies"
  (define-values (rules bindings reports)
    (rules-in-force (hash 'alarms (list (make-rule "ok" #:condition acc)
                                        (make-binding "missing" #:on "d1")
                                        (make-rule "bad" #:condition (condition 'silence (hasheq 'after -5)))
                                        (make-binding "ok" #:on "d2" #:overrides '((after . 3)))
                                        (make-rule "odd" #:condition (condition 'teleport (hasheq)))))))
  (check-equal? (hash-keys rules) '("ok"))
  (check-equal? bindings '())
  (check-equal? (map rules-report-kind reports) '(invalid invalid dangling invalid))
  (check-regexp-match #rx"after is not an overridable parameter of accumulate"
                      (rules-report-reason (last reports))))

(spec-test 'RL.semantics "an edit is no change, job-only (target or params), or semantic (fingerprint)"
  (define (spec r) (let-values ([(rules bs _) (rules-in-force (hash 's (list r)))])
                     (let-values ([(insts _d) (instances-for rules bs "d")]) (car (hash-values insts)))))
  (define base (make-rule "r" #:condition acc #:target "t" #:params '((x . 1))))
  (check-equal? (classify-edit (spec base) (spec base)) 'none)
  (check-equal? (classify-edit (spec base) (spec (make-rule "r" #:condition acc #:target "u" #:params '((x . 1))))) 'job-only)
  (check-equal? (classify-edit (spec base) (spec (make-rule "r" #:condition acc #:target "t" #:params '((x . 2))))) 'job-only)
  (check-equal? (classify-edit (spec base) (spec (make-rule "r" #:condition (condition 'accumulate (hasheq 'count 4 'span 0))
                                                            #:target "t" #:params '((x . 1)))))
                'semantic)
  (check-equal? (classify-edit (spec base) (spec (make-rule "r" #:condition acc #:watch '(load) #:target "t" #:params '((x . 1)))))
                'semantic)
  (check-equal? (classify-edit (spec base) (spec (make-rule "r" #:condition acc #:modifiers (hasheq 'rate-limit (cons 5 'defer))
                                                            #:target "t" #:params '((x . 1)))))
                'semantic))
