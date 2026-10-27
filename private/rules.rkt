#lang racket/base
;; Rules as data (spec §14).
;;
;; The rules — which operators run, for which keys, with which parameters —
;; arrive and change while the engine runs. A rule template names a
;; condition (an operator of §7–§12: accumulate, window close, silence,
;; schedule, or any registered condition kind), the kinds of sub-stream it
;; watches, modifiers (rate limit, flush, consumption per reason), a target,
;; default params, and the keys it applies to. Bindings instantiate templates
;; on keys with overrides. Sources group rules and bindings and are replaced
;; whole or patched; the rule set in force is their union.
(require racket/contract
         racket/list
         racket/match
         racket/string
         "time.rkt"
         "event.rkt")

;; ===========================================================================
;; Data

;; kind: a registered condition kind; params: hasheq
(struct condition (kind params) #:transparent)

;; RL.template. watch: #f (all kinds) or a list of kinds; modifiers: hasheq
;; with 'rate-limit (Δ . mode), 'flush Δ, 'flush-retain? boolean;
;; applies-to: 'all, 'none, or a list of keys; on-update: 'restart or 'continue
(struct rule (id watch condition modifiers target params applies-to on-update) #:transparent)

;; RL.binding and RL.fan-out: one binding names one or more keys.
;; watch: #f (keep the template's) or a list (narrow or replace);
;; overrides: hasheq of the condition's overridable parameters; params:
;; hasheq overriding the template's params.
(struct binding (rule-id keys binding-key watch overrides params) #:transparent)

;; A removal of one item from a patched source.
(struct removal (kind id) #:transparent)   ; kind: 'rule or 'binding; id: rule id or (rule-id keys binding-key)

;; A push of a rule source: replace it whole, or patch it item by item.
(struct rule-source-msg (name items mode) #:transparent)

(define (make-rule id #:condition cnd #:watch [watch #f] #:modifiers [mods (hasheq)] #:target [target #f]
                   #:params [params (hasheq)] #:applies-to [applies-to 'all] #:on-update [on-update 'restart])
  (rule id watch cnd mods target (->hasheq params) applies-to on-update))

(define (make-binding rule-id #:on keys #:key [bkey ""] #:watch [watch #f]
                      #:overrides [overrides (hasheq)] #:params [params (hasheq)])
  (binding rule-id (if (list? keys) keys (list keys)) bkey watch (->hasheq overrides) (->hasheq params)))

(define (->hasheq v) (if (hash? v) v (for/hasheq ([p (in-list v)]) (values (car p) (cdr p)))))

;; ===========================================================================
;; Condition kinds. A kind declares its overridable numeric parameters, a
;; validator, and its behaviour (rules-engine.rkt registers the four
;; families of §11.4).

(struct ckind (name overridable validate carry-key behaviour) #:transparent)

(define condition-kinds (make-hasheq))
(define (register-condition-kind! k) (hash-set! condition-kinds (ckind-name k) k))
(define (lookup-condition-kind name) (hash-ref condition-kinds name #f))

;; ===========================================================================
;; RL.source, RL.ownership, RL.dangling: the rule set in force

;; A report: (kind detail), kinds: 'refused-owner 'dangling 'invalid 'duplicate-instance
(struct rules-report (kind source item reason) #:transparent)

;; sources: hash name -> list of items, in push order
(define (apply-source-msg sources msg)
  (define name (rule-source-msg-name msg))
  (case (rule-source-msg-mode msg)
    [(replace) (hash-set sources name (rule-source-msg-items msg))]
    [(patch)
     (define items (hash-ref sources name '()))
     (hash-set sources name
               (for/fold ([items items]) ([it (in-list (rule-source-msg-items msg))])
                 (match it
                   [(removal 'rule id) (filter (λ (x) (not (and (rule? x) (equal? (rule-id x) id)))) items)]
                   [(removal 'binding id) (filter (λ (x) (not (and (binding? x) (equal? (binding-identity x) id)))) items)]
                   [(? rule? r) (append (filter (λ (x) (not (and (rule? x) (equal? (rule-id x) (rule-id r))))) items) (list r))]
                   [(? binding? b) (append (filter (λ (x) (not (and (binding? x) (equal? (binding-identity x) (binding-identity b))))) items)
                                           (list b))])))]))

(define (binding-identity b) (list (binding-rule-id b) (binding-keys b) (binding-binding-key b)))

;; Validation of one item; #f when valid, else the reason.
(define (rule-problem r)
  (define k (lookup-condition-kind (condition-kind* r)))
  (cond [(not k) (format "unknown condition kind ~a" (condition-kind* r))]
        [else ((ckind-validate k) (condition-params (rule-condition r)))]))
(define (condition-kind* r) (condition-kind (rule-condition r)))

(define (binding-problem b r)
  (define k (lookup-condition-kind (condition-kind* r)))
  (or (for/or ([p (in-hash-keys (binding-overrides b))])
        (and (not (memq p (ckind-overridable k)))
             (format "~a is not an overridable parameter of ~a (overridable: ~a)" p (ckind-name k)
                     (ckind-overridable k))))
      (and k ((ckind-validate k)
              (for/fold ([ps (condition-params (rule-condition r))]) ([(p v) (in-hash (binding-overrides b))])
                (hash-set ps p v))))))

;; The rule set in force: (values rules-by-id bindings reports), where
;; bindings is a list of (source . binding) in source order.
(define (rules-in-force sources)
  (define names (sort (hash-keys sources) (λ (a b) (string<? (format "~a" a) (format "~a" b)))))
  (define-values (rules owners reports)
    (for*/fold ([rules (hash)] [owners (hash)] [reports '()])
               ([n (in-list names)] [it (in-list (hash-ref sources n))] #:when (rule? it))
      (define id (rule-id it))
      (cond
        [(and (hash-ref owners id #f) (not (equal? (hash-ref owners id) n)))
         (values rules owners (cons (rules-report 'refused-owner n id
                                                  (format "rule ~a belongs to source ~a" id (hash-ref owners id)))
                                    reports))]
        [(rule-problem it) => (λ (p) (values rules owners (cons (rules-report 'invalid n id p) reports)))]
        [else (values (hash-set rules id it) (hash-set owners id n) reports)])))
  (define-values (bindings reports2)
    (for*/fold ([bs '()] [reports reports])
               ([n (in-list names)] [it (in-list (hash-ref sources n))] #:when (binding? it))
      (define r (hash-ref rules (binding-rule-id it) #f))
      (cond
        [(not r) (values bs (cons (rules-report 'dangling n (binding-identity it)
                                                (format "binding of missing rule ~a" (binding-rule-id it)))
                                  reports))]
        [(binding-problem it r) => (λ (p) (values bs (cons (rules-report 'invalid n (binding-identity it) p) reports)))]
        [else (values (append bs (list (cons n it))) reports)])))
  (values rules bindings (reverse reports2)))

;; ===========================================================================
;; RL.identity and instance specs: what runs on one key.

;; An instance spec: identity (rule-id key binding-key), the effective watch,
;; condition (with overrides), modifiers, target, params, and on-update.
(struct ispec (identity rule-id key binding-key watch condition modifiers target params on-update) #:transparent)

(define (effective-spec r b key)
  (define cnd (rule-condition r))
  (ispec (list (rule-id r) key (if b (binding-binding-key b) ""))
         (rule-id r) key (if b (binding-binding-key b) "")
         (or (and b (binding-watch b)) (rule-watch r))
         (if b
             (condition (condition-kind cnd)
                        (for/fold ([ps (condition-params cnd)]) ([(p v) (in-hash (binding-overrides b))]) (hash-set ps p v)))
             cnd)
         (rule-modifiers r) (rule-target r)
         (if b (for/fold ([ps (rule-params r)]) ([(p v) (in-hash (binding-params b))]) (hash-set ps p v)) (rule-params r))
         (rule-on-update r)))

;; The instances in force on `key`: hash identity -> ispec, and reports of
;; duplicate instances. A binding with the empty binding key replaces the
;; template's own instance on its key.
(define (instances-for rules bindings key)
  (define own-replaced
    (for/fold ([s (hash)]) ([sb (in-list bindings)])
      (define b (cdr sb))
      (if (and (equal? (binding-binding-key b) "") (member key (binding-keys b))) (hash-set s (binding-rule-id b) #t) s)))
  (define base
    (for/fold ([h (hash)]) ([(id r) (in-hash rules)]
                            #:when (applies? r key)
                            #:unless (hash-ref own-replaced id #f))
      (define sp (effective-spec r #f key))
      (hash-set h (ispec-identity sp) sp)))
  (for/fold ([h base] [dups '()] #:result (values h (reverse dups)))
            ([sb (in-list bindings)] #:when (member key (binding-keys (cdr sb))))
    (define b (cdr sb))
    (define sp (effective-spec (hash-ref rules (binding-rule-id b)) b key))
    (define id (ispec-identity sp))
    (if (and (hash-ref h id #f) (not (equal? (binding-binding-key b) "")))
        (values h (cons (rules-report 'duplicate-instance (car sb) id "another binding already produces this instance") dups))
        (values (hash-set h id sp) dups))))

(define (applies? r key)
  (match (rule-applies-to r)
    ['all #t]
    ['none #f]
    [(? list? ks) (and (member key ks) #t)]))

;; Every key that some rule or binding names explicitly.
(define (named-keys rules bindings)
  (remove-duplicates
   (append (for*/list ([r (in-hash-values rules)] #:when (list? (rule-applies-to r)) [k (rule-applies-to r)]) k)
           (for*/list ([sb (in-list bindings)] [k (binding-keys (cdr sb))]) k))))

;; ===========================================================================
;; RL.semantics: the fingerprint of an instance and the class of an edit.

(define (fingerprint sp)
  (list (condition-kind (ispec-condition sp))
        (sort (hash->list (condition-params (ispec-condition sp))) symbol<? #:key car)
        (and (ispec-watch sp) (sort (map (λ (x) (format "~a" x)) (ispec-watch sp)) string<?))
        (sort (hash->list (ispec-modifiers sp)) symbol<? #:key car)))

;; 'none, 'job-only (target or params changed), or 'semantic
(define (classify-edit old new)
  (cond [(not (equal? (fingerprint old) (fingerprint new))) 'semantic]
        [(or (not (equal? (ispec-target old) (ispec-target new)))
             (not (equal? (ispec-params old) (ispec-params new))))
         'job-only]
        [else 'none]))

(provide
 (contract-out
  [struct condition ([kind symbol?] [params hash?])]
  [struct rule ([id any/c] [watch (or/c #f list?)] [condition condition?] [modifiers hash?] [target any/c]
                [params hash?] [applies-to (or/c 'all 'none list?)] [on-update (or/c 'restart 'continue)])]
  [struct binding ([rule-id any/c] [keys list?] [binding-key any/c] [watch (or/c #f list?)]
                   [overrides hash?] [params hash?])]
  [struct removal ([kind (or/c 'rule 'binding)] [id any/c])]
  [struct rule-source-msg ([name any/c] [items list?] [mode (or/c 'replace 'patch)])]
  [make-rule (->* (any/c #:condition condition?)
                  (#:watch (or/c #f list?) #:modifiers hash? #:target any/c #:params (or/c hash? list?)
                   #:applies-to (or/c 'all 'none list?) #:on-update (or/c 'restart 'continue))
                  rule?)]
  [make-binding (->* (any/c #:on any/c) (#:key any/c #:watch (or/c #f list?) #:overrides (or/c hash? list?)
                                         #:params (or/c hash? list?))
                     binding?)]
  [struct ckind ([name symbol?] [overridable (listof symbol?)] [validate procedure?]
                  [carry-key procedure?] [behaviour any/c])]
  [register-condition-kind! (-> ckind? void?)]
  [lookup-condition-kind (-> symbol? (or/c #f ckind?))]
  [struct rules-report ([kind symbol?] [source any/c] [item any/c] [reason string?])]
  [apply-source-msg (-> hash? rule-source-msg? hash?)]
  [binding-identity (-> binding? list?)]
  [rules-in-force (-> hash? (values hash? list? list?))]
  [struct ispec ([identity list?] [rule-id any/c] [key any/c] [binding-key any/c] [watch (or/c #f list?)]
                 [condition condition?] [modifiers hash?] [target any/c] [params hash?] [on-update symbol?])]
  [instances-for (-> hash? list? any/c (values hash? list?))]
  [named-keys (-> hash? list? list?)]
  [fingerprint (-> ispec? list?)]
  [classify-edit (-> ispec? ispec? (or/c 'none 'job-only 'semantic))]))

;; ===========================================================================
;; Rules as JSON Lines: one item per line.
;;   {"kind": "rule", "id": .., "condition": {"type": .., param: ..}, "watch": [..],
;;    "rate_limit": [seconds, "defer"|"drop"], "flush": seconds, "retain_flush": bool,
;;    "target": .., "params": {..}, "applies_to": "all"|"none"|[..], "on_update": "continue"|"restart"}
;;   {"kind": "binding", "rule": .., "on": [..], "key": .., "watch": [..], "overrides": {..}, "params": {..}}
;;   {"kind": "remove-rule", "id": ..} or {"kind": "remove-binding", "rule": .., "on": [..], "key": ..}

(require json racket/file)

(define (->sym v) (if (string? v) (string->symbol v) v))
(define (jsexpr->rule-item h)
  (case (hash-ref h 'kind)
    [("rule")
     (define c (hash-ref h 'condition))
     (make-rule (hash-ref h 'id)
                #:condition (condition (->sym (hash-ref c 'type))
                                       (for/hasheq ([(k v) (in-hash c)] #:unless (eq? k 'type))
                                         (values k (if (and (eq? k 'mode) (string? v)) (string->symbol v) v))))
                #:watch (let ([w (hash-ref h 'watch #f)]) (and w (map ->sym w)))
                #:modifiers (for/hasheq ([kv (list (let ([r (hash-ref h 'rate_limit #f)])
                                                     (and r (cons 'rate-limit (cons (car r) (->sym (cadr r))))))
                                                   (let ([f (hash-ref h 'flush #f)]) (and f (cons 'flush f)))
                                                   (and (hash-ref h 'retain_flush #f) (cons 'flush-retain? #t)))]
                                         #:when kv)
                              (values (car kv) (cdr kv)))
                #:target (hash-ref h 'target #f)
                #:params (hash-ref h 'params (hasheq))
                #:applies-to (let ([a (hash-ref h 'applies_to "all")]) (if (string? a) (string->symbol a) a))
                #:on-update (->sym (hash-ref h 'on_update "restart")))]
    [("binding")
     (make-binding (hash-ref h 'rule) #:on (hash-ref h 'on) #:key (hash-ref h 'key "")
                   #:watch (let ([w (hash-ref h 'watch #f)]) (and w (map ->sym w)))
                   #:overrides (hash-ref h 'overrides (hasheq)) #:params (hash-ref h 'params (hasheq)))]
    [("remove-rule") (removal 'rule (hash-ref h 'id))]
    [("remove-binding") (removal 'binding (list (hash-ref h 'rule) (hash-ref h 'on) (hash-ref h 'key "")))]
    [else (raise-arguments-error 'jsexpr->rule-item "unknown item kind" "kind" (hash-ref h 'kind))]))

(define (read-jsonl-rule-items path)
  (for/list ([line (file->lines path)] #:unless (regexp-match? #px"^\\s*$" line))
    (jsexpr->rule-item (string->jsexpr line))))


(provide
 (contract-out
  [jsexpr->rule-item (-> hash? (or/c rule? binding? removal?))]
  [read-jsonl-rule-items (-> path-string? list?)]))
