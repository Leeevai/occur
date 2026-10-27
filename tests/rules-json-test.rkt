#lang racket/base
(require rackunit
         json
         "util.rkt"
         "../private/rules.rkt"
         "../private/rules-engine.rkt")

(spec-test '(RL.source RL.dangling) "rule sources can arrive as JSON Lines items"
  (define items
    (map (λ (l) (jsexpr->rule-item (string->jsexpr l)))
         (list "{\"kind\": \"rule\", \"id\": \"acc\", \"condition\": {\"type\": \"accumulate\", \"count\": 3, \"span\": 0}, \"rate_limit\": [60, \"defer\"], \"watch\": [\"pressure\"], \"target\": \"t\", \"params\": {\"threshold\": 50}}"
               "{\"kind\": \"binding\", \"rule\": \"acc\", \"on\": [\"d7\", \"d8\"], \"key\": \"cfg-41\", \"overrides\": {\"count\": 5}, \"params\": {\"threshold\": 40}}"
               "{\"kind\": \"binding\", \"rule\": \"missing\", \"on\": [\"d1\"]}")))
  (define r (car items))
  (check-equal? (rule-watch r) '(pressure))
  (check-equal? (hash-ref (rule-modifiers r) 'rate-limit) (cons 60 'defer))
  (define-values (rules bindings reports) (rules-in-force (hash 'json items)))
  (check-equal? (length bindings) 1)
  (check-equal? (map rules-report-kind reports) '(dangling))
  (define-values (insts _) (instances-for rules bindings "d8"))
  (check-equal? (hash-ref (condition-params (ispec-condition (hash-ref insts '("acc" "d8" "cfg-41")))) 'count) 5)
  (check-equal? (removal-id (jsexpr->rule-item (hasheq 'kind "remove-rule" 'id "acc"))) "acc"))
