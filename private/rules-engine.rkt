#lang racket/base
;; The rules engine: instances of rule templates running on keys (§14), with
;; the conditions of §11.4 that data-arrival trigger engines use
;; (PT.accumulate with PT.flush, PT.window-close, PT.silence with
;; PT.recovery, PT.schedule) and their modifiers (OUT.rate-limit, CON.retain).
;;
;; A rules node is keyed by entity. Its global state is the rule set in force;
;; each key holds the entity's state (activation instant, newest contribution
;; per kind) and its instances, identified by (rule, key, binding key).
;; Every instance keeps the contributions since its previous trigger: a
;; trigger's samples are all of them (AG.multiplicity) and its sources their
;; sub-streams and kinds (AG.sources).
(require racket/contract
         racket/list
         racket/match
         "time.rkt"
         "event.rkt"
         "operator.rkt"
         "progress.rkt"
         "scheduler.rkt"
         "derive.rkt"
         "rules.rkt")

;; How a contribution's kind and sub-stream are read: the rules node binds
;; these from its source's keying.
(define current-kind-of (make-parameter (λ (e) (event-attr/default e 'kind #f))))
(define current-substream-of (make-parameter (λ (e) (event-attr/default e 'substream #f))))
(define (kind-of* e) ((current-kind-of) e))

;; ===========================================================================
;; Condition behaviours

;; init : spec now -> cstate
;; on-contribution : cstate c spec now -> (values cstate intents)
;; on-timer : cstate spec now pending -> (values cstate intents)
;; deadline : cstate spec pending -> (or/c #f time)
;; carry? : old-spec new-spec -> boolean (progress means the same in both)
;; rejudge? : boolean (job-only edits make it emit a config output)
;; consumes? : intent -> boolean (handled generically by reason)
(struct behaviour (init on-contribution on-timer deadline carry? rejudge? evaluate) #:transparent)

;; An intent to output: the reason, the window [ws, we] (ws may be 'none),
;; the windows merged, and whether the window is the pending contributions'
;; hull at release (accumulate).
(struct intent (reason ws we windows from-pending?) #:transparent)

(define (param sp k [default #f]) (hash-ref (condition-params (ispec-condition sp)) k default))
(define (modifier sp k [default #f]) (hash-ref (ispec-modifiers sp) k default))

(define (pending-samples pending) (apply + 0 (map event-n pending)))
(define (pending-span pending)
  (if (null? pending) 0 (- (apply max (map event-te pending)) (apply min (map event-ts pending)))))

;; ---------------------------------------------------------------------------
;; PT.accumulate (with PT.flush): over the contributions since the last
;; consuming output (WN.since-last), ready when count >= N and span >= S.
;; cstate: the processing time the flush clock counts from, or #f.

(define accumulate-behaviour
  (behaviour
   (λ (sp now) #f)
   (λ (cs c sp now) (values (or cs now) '()))
   (λ (cs sp now pending)
     (define Δd (modifier sp 'flush))
     (cond
       [(and Δd cs (pair? pending) (>= now (+ cs Δd)) (not (ready? sp pending)))
        ;; a retained flush keeps counting: its clock restarts now
        (values (if (modifier sp 'flush-retain? #f) now #f)
                (list (intent 'max_delay #f #f 1 #t)))]
       [else (values cs '())]))
   (λ (cs sp pending) (and (modifier sp 'flush) cs (pair? pending) (+ cs (modifier sp 'flush))))
   (λ (old new) #t)
   #t
   (λ (cs sp pending) (if (ready? sp pending) (list (intent 'samples #f #f 1 #t)) '()))))

(define (ready? sp pending)
  (and (pair? pending)
       (>= (pending-samples pending) (param sp 'count 1))
       (>= (pending-span pending) (param sp 'span 0))))

;; ---------------------------------------------------------------------------
;; PT.window-close: WN.tumbling(w, o) anchored at the first contribution,
;; closed by the key's data-driven watermark over the watched kinds (all or
;; any); windows that close together are coalesced (OUT.coalesce).
;; cstate: (vector kwm next-start)

(define window-behaviour
  (behaviour
   (λ (sp now) (vector empty-keyed-wm #f))
   (λ (cs c sp now)
     (define size (param sp 'size))
     (define offset (param sp 'offset 0))
     (define kwm (keyed-wm-observe (vector-ref cs 0) (kind-of* c) (event-te c)))
     (define next (or (vector-ref cs 1) (+ offset (* size (floor (/ (- (event-ts c) offset) size))))))
     (define W (keyed-wm-value kwm (param sp 'mode 'all) (ispec-watch sp)))
     (define k (if (and (< W +inf.0) (> W -inf.0)) (max 0 (floor (/ (- W next) size))) 0))
     (if (positive? k)
         (values (vector kwm (+ next (* k size))) (list (intent 'window next (+ next (* k size)) k #f)))
         (values (vector kwm next) '())))
   (λ (cs sp now pending) (values cs '()))
   (λ (cs sp pending) #f)
   (λ (old new) (and (equal? (param old 'size) (param new 'size)) (equal? (param old 'offset 0) (param new 'offset 0))))
   #f
   (λ (cs sp pending) '())))

;; ---------------------------------------------------------------------------
;; PT.silence (with PT.recovery): on the hybrid clock, silence fires once the
;; newest sample time (or the activation instant, ACT) plus Δ is reached by
;; the processing clock; with recovery, the first contribution afterwards
;; reports `recovered`. Together they are edge-triggered (OUT.edge).
;; cstate: (vector anchor silent? silence-start); anchor: (cons 'data t) or (cons 'act t)

(define silence-behaviour
  (behaviour
   (λ (sp now) (vector (cons 'act now) #f #f))
   (λ (cs c sp now)
     (define anchor (vector-ref cs 0))
     (define newest (if (eq? (car anchor) 'data) (time-max (cdr anchor) (event-te c)) (event-te c)))
     (define recovered (and (vector-ref cs 1) (param sp 'recovery #f)
                            (list (intent 'recovered (vector-ref cs 2) (event-te c) 1 #f))))
     (values (vector (cons 'data newest) #f #f) (or recovered '())))
   (λ (cs sp now pending)
     (define anchor (vector-ref cs 0))
     (define due (+ (cdr anchor) (param sp 'after)))
     (cond
       [(and (not (vector-ref cs 1)) (>= now due))
        (define start (if (eq? (car anchor) 'data) (cdr anchor) 'none))
        (values (vector anchor #t start) (list (intent 'silence start due 1 #f)))]
       [else (values cs '())]))
   (λ (cs sp pending) (and (not (vector-ref cs 1)) (+ (cdr (vector-ref cs 0)) (param sp 'after))))
   (λ (old new) #t)
   #f
   (λ (cs sp pending) '())))

;; ---------------------------------------------------------------------------
;; PT.schedule: WN.calendar(expr) in processing time, read in UTC, firing
;; even when empty (WN.empty), missed ticks coalesced (OUT.coalesce).
;; cstate: (vector cron period-start handled)

(define schedule-behaviour
  (behaviour
   (λ (sp now)
     (define c (parse-cron (param sp 'cron)))
     (vector c (or (cron-prev c now) now) now))
   (λ (cs c sp now) (values cs '()))
   (λ (cs sp now pending)
     (define ticks (cron-ticks-between (vector-ref cs 0) (vector-ref cs 2) now))
     (if (null? ticks)
         (values cs '())
         (values (vector (vector-ref cs 0) (last ticks) (last ticks))
                 (list (intent 'schedule (vector-ref cs 1) (last ticks) (length ticks) #f)))))
   (λ (cs sp pending) (cron-next (vector-ref cs 0) (vector-ref cs 2)))
   (λ (old new) (equal? (param old 'cron) (param new 'cron)))
   #f
   (λ (cs sp pending) '())))

;; ---------------------------------------------------------------------------
;; Registering the four conditions with their overridable parameters.

(define (positive-number? v) (and (real? v) (positive? v)))
(define (natural? v) (exact-nonnegative-integer? v))

(register-condition-kind!
 (ckind 'accumulate '(count span)
        (λ (ps) (cond [(not (natural? (hash-ref ps 'count 1))) "count must be a natural number"]
                      [(not (and (real? (hash-ref ps 'span 0)) (>= (hash-ref ps 'span 0) 0))) "span must be a duration"]
                      [else #f]))
        (λ (ps) 'accumulate) accumulate-behaviour))
(register-condition-kind!
 (ckind 'window-close '()
        (λ (ps) (cond [(not (positive-number? (hash-ref ps 'size #f))) "a window needs a positive size"]
                      [(not (memq (hash-ref ps 'mode 'all) '(all any))) "mode must be all or any"]
                      [else #f]))
        (λ (ps) (list (hash-ref ps 'size) (hash-ref ps 'offset 0))) window-behaviour))
(register-condition-kind!
 (ckind 'silence '(after)
        (λ (ps) (if (positive-number? (hash-ref ps 'after #f)) #f "silence needs a positive after"))
        (λ (ps) 'silence) silence-behaviour))
(register-condition-kind!
 (ckind 'schedule '()
        (λ (ps) (with-handlers ([exn:fail? exn-message]) (parse-cron (hash-ref ps 'cron "")) #f))
        (λ (ps) (hash-ref ps 'cron)) schedule-behaviour))

(define (behaviour-of sp) (ckind-behaviour (lookup-condition-kind (condition-kind (ispec-condition sp)))))

;; ===========================================================================
;; Instances

;; pending: contributions since the previous consuming trigger; last: the
;; processing time of the previous output (OUT.rate-limit); held: intents
;; waiting for the cap to end; rejudge?: a config output is due.
(struct inst (spec cstate pending last held rejudge?) #:transparent)

(define (new-instance sp now) (inst sp ((behaviour-init (behaviour-of sp)) sp now) '() #f '() #f))

(define (watches? sp c)
  (define w (ispec-watch sp))
  (or (not w) (and (member (kind-of* c) w) #t)))

;; A rule output (DR.derive with DR.summary): target, key, binding key,
;; reason, window, samples, windows merged, sources, params, emission time,
;; and its identity (DR.output-identity).
(define (rule-output sp reason ws we samples windows sources now)
  (define iv-start (if (eq? ws 'none) we ws))
  (define no-new-data? (eq? reason 'config))
  (define id (output-identity #:instance (ispec-identity sp) #:key (ispec-key sp) #:reason reason
                              #:interval (interval (min iv-start we) we) #:samples samples
                              #:emitted-at now #:no-new-data? no-new-data?))
  (make-event 'RuleOutput
              `((rule . ,(ispec-rule-id sp)) (key . ,(ispec-key sp)) (binding-key . ,(ispec-binding-key sp))
                (target . ,(ispec-target sp)) (reason . ,reason) (window . ,(list ws we))
                (samples . ,samples) (windows . ,windows) (sources . ,sources)
                (params . ,(ispec-params sp)) (emitted-at . ,now) (identity . ,id)
                (summary . ,(make-summary #:interval (interval (min iv-start we) we) #:samples samples
                                          #:merged windows #:sources sources)))
              #:ts (min iv-start we) #:te we #:n 1 #:src 'rules))

(define (sources-of pending)
  (sort (remove-duplicates (for/list ([c pending]) (cons ((current-substream-of) c) (kind-of* c))))
        (λ (a b) (string<? (format "~s" a) (format "~s" b)))))

;; Merge intents released together into one output (OUT.coalesce): the
;; reason of the latest, the hull of their windows, their windows summed.
(define (release in intents now newest-of emit)
  (define sp (inst-spec in))
  (define data (filter (λ (i) (not (eq? (intent-reason i) 'config))) intents))
  (define final (last intents))
  (cond
    [(and (null? data) (eq? (intent-reason final) 'config))
     ;; RL.rejudge: covers the newest contribution of the watched kinds, consumes nothing
     (define newest (newest-of sp))
     (emit (rule-output sp 'config (if newest (event-ts newest) 'none) (if newest (event-te newest) now) 0 1 '() now))
     (struct-copy inst in [last now] [held '()] [rejudge? #f])]
    [else
     (define pending (inst-pending in))
     (define reason (intent-reason (last data)))
     (define-values (ws we)
       (if (intent-from-pending? (last data))
           (values (if (null? pending) 'none (apply min (map event-ts pending)))
                   (if (null? pending) now (apply max (map event-te pending))))
           (values (let ([starts (filter real? (map intent-ws data))])
                     (if (and (pair? starts) (andmap real? (map intent-ws data))) (apply min starts) (intent-ws (car data))))
                   (apply max (map intent-we data)))))
     (emit (rule-output sp reason ws we (pending-samples pending) (apply + (map intent-windows data))
                        (sources-of pending) now))
     ;; a retained flush consumes nothing (CON.retain)
     (define retain? (and (eq? reason 'max_delay) (modifier sp 'flush-retain? #f)))
     (struct-copy inst in [last now] [held '()] [rejudge? #f] [pending (if retain? pending '())])]))

;; OUT.rate-limit: emit now, or hold until the cap ends (defer), or drop.
(define (offer in new-intents now newest-of emit count!)
  (cond
    [(null? new-intents) in]
    [else
     (define cap (modifier (inst-spec in) 'rate-limit))
     (define Δc (and cap (car cap)))
     (define mode (if cap (cdr cap) 'defer))
     ;; a data output supersedes a pending config output
     (define held (if (ormap (λ (i) (not (eq? (intent-reason i) 'config))) new-intents)
                      (filter (λ (i) (not (eq? (intent-reason i) 'config))) (inst-held in))
                      (inst-held in)))
     (define all (append held new-intents))
     (cond
       [(or (not Δc) (not (inst-last in)) (>= now (+ (inst-last in) Δc))) (release in all now newest-of emit)]
       [(eq? mode 'drop) (count! 'rate-limited) (struct-copy inst in [held held])]
       [else (struct-copy inst in [held all])])]))

(define (inst-deadline in)
  (define sp (inst-spec in))
  (define b (behaviour-of sp))
  (define cap (modifier sp 'rate-limit))
  (define ds (filter values
                     (list ((behaviour-deadline b) (inst-cstate in) sp (inst-pending in))
                           (and cap (pair? (inst-held in)) (inst-last in) (+ (inst-last in) (car cap))))))
  (and (pair? ds) (apply min ds)))

;; Evaluate an instance at `now`: timers, readiness, and held outputs.
(define (evaluate in now newest-of emit count!)
  (define sp (inst-spec in))
  (define b (behaviour-of sp))
  (define-values (cs intents) ((behaviour-on-timer b) (inst-cstate in) sp now (inst-pending in)))
  (define in1 (struct-copy inst in [cstate cs]))
  (define ready ((behaviour-evaluate b) cs sp (inst-pending in1)))
  ;; an accumulate output already waiting under the cap is not offered twice
  (define fresh (filter (λ (i) (not (and (intent-from-pending? i)
                                         (for/or ([h (inst-held in1)]) (intent-from-pending? h)))))
                        (append intents ready)))
  (define rejudge (if (and (inst-rejudge? in1) (behaviour-rejudge? b)) (list (intent 'config #f #f 1 #f)) '()))
  (define in2 (offer (struct-copy inst in1 [rejudge? #f]) (append fresh rejudge) now newest-of emit count!))
  ;; release held outputs whose cap has ended
  (define cap (modifier sp 'rate-limit))
  (if (and cap (pair? (inst-held in2)) (inst-last in2) (>= now (+ (inst-last in2) (car cap))))
      (release in2 (inst-held in2) now newest-of emit)
      in2))

(define (contribute in c now newest-of emit count!)
  (define sp (inst-spec in))
  (define b (behaviour-of sp))
  (define-values (cs intents) ((behaviour-on-contribution b) (inst-cstate in) c sp now))
  (define in1 (struct-copy inst in [cstate cs] [pending (append (inst-pending in) (list c))]))
  (define in2 (offer in1 intents now newest-of emit count!))
  (evaluate in2 now newest-of emit count!))

;; ===========================================================================
;; RL.semantics, RL.carry, RL.rejudge: reconciling a key's instances with the
;; rules in force after a push.

(define (reconcile old-insts desired now has-data? newest-of emit count!)
  (for/fold ([out (hash)]) ([(id sp) (in-hash desired)])
    (define old (hash-ref old-insts id #f))
    (define b (behaviour-of sp))
    (cond
      [(not old)
       ;; a new instance; on a key that already has data an accumulate
       ;; instance re-judges (RL.rejudge)
       (define in (struct-copy inst (new-instance sp now) [rejudge? (and has-data? (behaviour-rejudge? b))]))
       (hash-set out id (evaluate in now newest-of emit count!))]
      [else
       (case (classify-edit (inst-spec old) sp)
         [(none) (hash-set out id old)]
         [(job-only)
          ;; progress kept; accumulate re-judges, others ignore job-only edits
          (define in (struct-copy inst old [spec sp] [rejudge? (behaviour-rejudge? b)]))
          (hash-set out id (evaluate in now newest-of emit count!))]
         [(semantic)
          (define same-kind? (eq? (condition-kind (ispec-condition (inst-spec old))) (condition-kind (ispec-condition sp))))
          (define fresh (new-instance sp now))
          (cond
            [(and (eq? (ispec-on-update sp) 'continue) same-kind? ((behaviour-carry? b) (inst-spec old) sp))
             ;; progress means the same: carry it, and evaluate at once
             (hash-set out id (evaluate (struct-copy inst old [spec sp]) now newest-of emit count!))]
            [(eq? (ispec-on-update sp) 'continue)
             ;; progress restarts but the rate-limit state carries
             (hash-set out id (struct-copy inst fresh [last (inst-last old)]))]
            [else (hash-set out id fresh)])])])))

;; ===========================================================================
;; The rules node

;; global: (vector sources rules bindings)
;; key state: (vector activation newest-by-kind has-data? instances)
(define (rules-operator #:key key #:name [name 'rules]
                        #:kind [kind (λ (e) (event-attr/default e 'kind #f))]
                        #:substream [substream (λ (e) (event-attr/default e 'substream #f))])
  (define (with-keying thunk) (parameterize ([current-kind-of kind] [current-substream-of substream]) (thunk)))
  (define op (rules-operator* key name))
  ;; every callback reads kinds and sub-streams through the keying
  (struct-copy operator op
               [coalesce coalesce-rule-outputs]
               [on-event (λ (g s e c) (with-keying (λ () ((operator-on-event op) g s e c))))]
               [on-timer (λ (g s c) (with-keying (λ () ((operator-on-timer op) g s c))))]
               [init-key (λ (g k c) (with-keying (λ () ((operator-init-key op) g k c))))]
               [on-control (λ (g keys msg c) (with-keying (λ () ((operator-on-control op) g keys msg c))))]
               [deadline (λ (s) (with-keying (λ () ((operator-deadline op) s))))]))

(define (rules-operator* key name)
  (define (newest-of-key ks)
    (λ (sp)
      (define by-kind (vector-ref ks 1))
      (define cs (for/list ([(k c) (in-hash by-kind)] #:when (or (not (ispec-watch sp)) (member k (ispec-watch sp)))) c))
      (and (pair? cs) (argmax event-te cs))))
  (define (emitter c) (λ (e) (emit! c e)))
  (define (counter c) (λ (name) (count! c name)))
  (define (desired g k)
    (define-values (insts dups) (instances-for (vector-ref g 1) (vector-ref g 2) k))
    insts)
  (make-operator
   name
   #:key key
   #:lifecycle? #t
   #:config? #t
   #:monotone? #f
   #:init-global (λ () (vector (hash) (hash) '()))
   #:init-key (λ (g k c)
                (define now (ctx-now c))
                (vector now (hash) #f
                        (for/hash ([(id sp) (in-hash (desired g k))]) (values id (new-instance sp now)))))
   #:on-event
   (λ (g ks e c)
     (define now (ctx-now c))
     (define ks1 (vector (vector-ref ks 0)
                         (hash-set (vector-ref ks 1) (kind-of* e) e)
                         #t (vector-ref ks 3)))
     (define newest (newest-of-key ks1))
     (vector (vector-ref ks1 0) (vector-ref ks1 1) #t
             (for/hash ([(id in) (in-hash (vector-ref ks1 3))])
               (values id (if (watches? (inst-spec in) e)
                              (contribute in e now newest (emitter c) (counter c))
                              in)))))
   #:deadline (λ (ks) (define ds (filter values (map inst-deadline (hash-values (vector-ref ks 3)))))
                (and (pair? ds) (apply min ds)))
   #:on-timer
   (λ (g ks c)
     (define now (ctx-now c))
     (define newest (newest-of-key ks))
     (vector (vector-ref ks 0) (vector-ref ks 1) (vector-ref ks 2)
             (for/hash ([(id in) (in-hash (vector-ref ks 3))])
               (values id (evaluate in now newest (emitter c) (counter c))))))
   #:on-control
   (λ (g keys msg c)
     (cond
       [(rule-source-msg? msg)
        (define sources (apply-source-msg (vector-ref g 0) msg))
        (define-values (rules bindings reports) (rules-in-force sources))
        (define g2 (vector sources rules bindings))
        (define old-reports (let-values ([(r b rep) (rules-in-force (vector-ref g 0))]) rep))
        (for ([r reports] #:unless (member r old-reports)) (report! c r))
        (define now (ctx-now c))
        (values g2
                (for/hash ([(k ks) (in-hash keys)])
                  (values k (vector (vector-ref ks 0) (vector-ref ks 1) (vector-ref ks 2)
                                    (reconcile (vector-ref ks 3) (desired g2 k) now (vector-ref ks 2)
                                               (newest-of-key ks) (emitter c) (counter c))))))]
       [else (values g keys)]))
   #:instance-of (λ (e) (list (event-attr e 'rule) (event-attr e 'key) (event-attr e 'binding-key)))
   #:coalesce coalesce-rule-outputs
   #:judge (λ (g keys e)
             (hash-has-key? (desired g (event-attr e 'key))
                            (list (event-attr e 'rule) (event-attr e 'key) (event-attr e 'binding-key))))))

(provide
 (contract-out
  [rules-operator (->* (#:key (-> event? any/c)) (#:name symbol? #:kind (-> event? any/c) #:substream (-> event? any/c))
                       operator?)]
  [struct intent ([reason symbol?] [ws any/c] [we any/c] [windows exact-nonnegative-integer?] [from-pending? boolean?])]
  [struct inst ([spec ispec?] [cstate any/c] [pending list?] [last (or/c #f time?)] [held list?] [rejudge? boolean?])]))

;; A script step pushing a rule source (a configuration push, §14).
(require "dataflow.rkt")
(define (rule-source-push name items #:mode [mode 'replace] #:at [t #f])
  (control (rule-source-msg name items mode) #:at t #:config? #t))

(provide
 (contract-out
  [rule-source-push (->* (any/c list?) (#:mode (or/c 'replace 'patch) #:at (or/c #f time?)) step:control?)]))

;; OUT.coalesce for rule outputs held back during a replay: one output per
;; instance with the latest reason and params, the hull of the windows, and
;; samples and windows summed.
(define (coalesce-rule-outputs es)
  (define latest (last es))
  (define (attr e k) (event-attr e k))
  (define starts (filter real? (map (λ (e) (car (attr e 'window))) es)))
  (define ws (if (= (length starts) (length es)) (apply min starts) (car (attr (car es) 'window))))
  (define we (apply max (map (λ (e) (cadr (attr e 'window))) es)))
  (define samples (apply + (map (λ (e) (attr e 'samples)) es)))
  (define windows (apply + (map (λ (e) (attr e 'windows)) es)))
  (define sources (remove-duplicates (append* (map (λ (e) (attr e 'sources)) es))))
  (define reason (attr latest 'reason))
  (define iv (interval (if (real? ws) (min ws we) we) we))
  (event-update latest
                #:payload (for/fold ([p (event-payload latest)])
                                    ([kv (list (cons 'window (list ws we)) (cons 'samples samples) (cons 'windows windows)
                                               (cons 'sources sources) (cons 'merged (length es))
                                               (cons 'identity (output-identity #:instance (list (attr latest 'rule) (attr latest 'key)
                                                                                                 (attr latest 'binding-key))
                                                                                #:key (attr latest 'key) #:reason reason
                                                                                #:interval iv #:samples samples))
                                               (cons 'summary (make-summary #:interval iv #:samples samples #:merged windows
                                                                            #:sources sources)))])
                            (hash-set p (car kv) (cdr kv)))
                #:ts (interval-start iv) #:te we))

(provide
 (contract-out
  [coalesce-rule-outputs (-> (non-empty-listof event?) event?)]))
