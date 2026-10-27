#lang racket/base
;; Pattern policies (spec §12): selection strategies, consumption, output,
;; skipping, and evaluation timing.
(require racket/contract
         racket/list
         "../time.rkt"
         "../event.rkt"
         "ast.rkt"
         "reference.rkt"
         "nfa.rkt"
         "../scheduler.rkt")

;; ===========================================================================
;; §12.1 Selection strategies (decision 0012)
;;
;; A strategy decides which events may be skipped between consecutive steps.
;; Every event in a match records the leaf it was taken for and the end
;; point (te . index) of the step before it, or none at the start of the
;; pattern. A strategy is a condition on each event that follows a step:
;;   SEL.strict     its global arrival index is the next one after the step
;;   SEL.partition  its index within the key's partition is the next one
;;   SEL.next       no event qualifying for the same leaf (type, predicate,
;;                  and ts > te(step)) arrived between the step and it
;;   SEL.any        no condition
;; After ACT (index −1), "next" means the instance's first event. On a single
;; key the match sets nest: strict ⊆ partition ⊆ next ⊆ any.

(define (qualifies? leaf e)
  (and (or (not (p:primitive-type leaf)) (eq? (event-type e) (p:primitive-type leaf)))
       (or (not (p:primitive-pred leaf)) ((p:primitive-pred leaf) e))))

(define (pred-pindex m pred index pindex)
  (define gi (cdr pred))
  (if (< gi 0)
      -1
      (for/first ([e (in-list (pmatch-events m))] #:when (= (hash-ref index (event-id e)) gi))
        (hash-ref pindex (event-id e)))))

(define ((strategy-check strategy) m index pindex qual-of)
  (for/and ([u (in-list (pmatch-leaves m))] #:when (leaf-use-pred u))
    (define e (leaf-use-event u))
    (define pred (leaf-use-pred u))
    (define gi (hash-ref index (event-id e)))
    (case strategy
      [(strict) (if (< (cdr pred) 0)
                    (= (hash-ref pindex (event-id e)) 0)
                    (= gi (add1 (cdr pred))))]
      [(partition) (= (hash-ref pindex (event-id e)) (add1 (pred-pindex m pred index pindex)))]
      [(next) (not (for/or ([e2 (in-list (qual-of (leaf-use-leaf u)))])
                     (define g2 (hash-ref index (event-id e2) #f))
                     (and g2 (< (cdr pred) g2 gi) (> (event-ts e2) (car pred)))))]
      [else #t])))

(for ([s '(strict partition next)]) (register-selection-check! s (strategy-check s)))

;; The oracle's version: filter ⟦P⟧(H) by a strategy, with H's order as the
;; arrival order within the key. #:global-index maps each event's id to its
;; arrival index over all keys, which strict contiguity reads; by default it
;; is H's order (one key, so global and partition indices coincide). The
;; oracle records a step's end point by its position in H, so with a global
;; index the end points are translated first.
(define (select-matches ms strategy history #:global-index [global #f])
  (define pindex (for/hash ([e (in-list history)] [i (in-naturals)]) (values (event-id e) i)))
  (define index (or global pindex))
  (define pos->index (for/vector ([e (in-list history)]) (hash-ref index (event-id e))))
  (define (translate m)
    (if global
        (pmatch (pmatch-events m) (pmatch-bindings m) (pmatch-ts m) (pmatch-te m)
                (for/list ([u (in-list (pmatch-leaves m))])
                  (define pred (leaf-use-pred u))
                  (if (and pred (>= (cdr pred) 0))
                      (leaf-use (leaf-use-leaf u) (leaf-use-event u) (cons (car pred) (vector-ref pos->index (cdr pred))))
                      u)))
        m))
  (define check (strategy-check strategy))
  (define (qual-of leaf) (filter (λ (e) (qualifies? leaf e)) history))
  (filter (λ (m) (check (translate m) index pindex qual-of)) ms))

;; OUT.maximal over a complete set of matches: those not strictly contained,
;; by events, in another.
(define (maximal-matches ms)
  (define sets (for/list ([m (in-list ms)]) (for/hash ([e (in-list (pmatch-events m))]) (values (event-id e) #t))))
  (define (strictly-within? a b)
    (and (< (hash-count a) (hash-count b)) (for/and ([k (in-hash-keys a)]) (hash-ref b k #f))))
  (for/list ([m (in-list ms)] [a (in-list sets)]
             #:unless (for/or ([b (in-list sets)]) (strictly-within? a b)))
    m))

(define selection-strategies '(strict partition next any))

(provide
 selection-strategies
 (contract-out
  [qualifies? (-> p:primitive? event? boolean?)]
  [select-matches (->* ((listof pmatch?) (or/c 'strict 'partition 'next 'any) (listof event?))
                       (#:global-index (or/c #f hash?))
                       (listof pmatch?))]
  [maximal-matches (-> (listof pmatch?) (listof pmatch?))]))

;; ===========================================================================
;; §12.2–§12.3 The policy layer (decision 0012)
;;
;; The engine produces candidate matches; the policy layer decides which are
;; reported, in this order:
;;   1. evaluation timing: a candidate is released at once (EVAL.immediate),
;;      or when the watermark passes the end of its scope (EVAL.deferred; the
;;      scope ends at ts + Δ under #:within Δ, else at te), which last and
;;      maximal output force;
;;   2. skipping: candidates whose initiator lies in a range discarded by an
;;      earlier match are dropped (SKIP.*);
;;   3. consumption: candidates using consumed events are dropped, and the
;;      policy picks among the candidates completed by one terminator
;;      (CON.*);
;;   4. output: every, first per initiator, last per initiator, or maximal
;;      (OUT.*).
;; The initiator of a match is its earliest-arriving event (ACT for a match
;; without events); its terminator is its latest-arriving event.

(struct match-policy (consumption output skip eval within timeout retain) #:transparent)

(define (make-match-policy #:consumption [consumption 'unrestricted] #:output [output 'every]
                           #:skip [skip 'no-skip] #:eval [eval #f] #:within [within #f]
                           #:timeout-partial [timeout #f] #:retain [retain '()])
  (match-policy consumption output skip eval within timeout retain))

;; A candidate: the match, its initiator and terminator indices, the latest
;; initiator index when it completed (for CON.recent), its release time,
;; and its reason.
(struct cand (m init term latest release reason) #:transparent)

(struct pstate (pending consumed first-seen last-held seen skips partials emitted-inits) #:transparent)

(define (policy-init pol) (pstate '() (hash) (hash) (hash) '() '() (hash) (hash)))

(define (match-index-range m index)
  (define is (for/list ([e (in-list (pmatch-events m))]) (hash-ref index (event-id e))))
  (if (null? is) (values -1 -1) (values (apply min is) (apply max is))))

(define (scope-end pol m)
  (if (match-policy-within pol) (+ (pmatch-ts m) (match-policy-within pol)) (pmatch-te m)))

(define (deferred? pol) (or (eq? (match-policy-eval pol) 'deferred) (memq (match-policy-output pol) '(last maximal))))

;; latest: the index of the newest event that could begin the pattern
(define (make-cand pol m index latest reason)
  (define-values (lo hi) (match-index-range m index))
  (cand m lo hi latest (if (deferred? pol) (strictly-after (scope-end pol m)) #f) reason))

(define (released? c W) (or (not (cand-release c)) (deadline-due? (cand-release c) W)))

(define (consumed? ps m) (for/or ([e (pmatch-events m)]) (hash-ref (pstate-consumed ps) (event-id e) #f)))
(define (consume ps m)
  (struct-copy pstate ps [consumed (for/fold ([h (pstate-consumed ps)]) ([e (pmatch-events m)]) (hash-set h (event-id e) #t))]))
(define (skipped? ps c) (for/or ([r (pstate-skips ps)]) (and (<= (car r) (cand-init c)) (< (cand-init c) (cdr r)))))

;; SKIP.*: the initiator range a reported match discards
(define (skip-range pol c index)
  (define m (cand-m c))
  (define (bound-index x first?)
    (define v (hash-ref (pmatch-bindings m) x #f))
    (define es (cond [(event? v) (list v)] [(list? v) (filter event? v)] [else '()]))
    (define is (map (λ (e) (hash-ref index (event-id e))) es))
    (if (null? is) (cand-init c) (if first? (apply min is) (apply max is))))
  (match (match-policy-skip pol)
    ['no-skip #f]
    ['to-next (cons (cand-init c) (add1 (cand-init c)))]
    ['past-last (cons (cand-init c) (add1 (cand-term c)))]
    [(list 'to-first x) (cons (cand-init c) (bound-index x #t))]
    [(list 'to-last x) (cons (cand-init c) (bound-index x #f))]))

(require racket/match)

;; Report one match: apply the skip rule, consumption, and the output hooks.
;; With #:defer-consumption, the caller consumes later (CON.continuous
;; consumes once the terminator has completed every initiator).
(define (report pol ps c index emit #:defer-consumption [defer? #f])
  (emit (if (eq? (cand-reason c) 'match) (cand-m c)
            (struct-copy pmatch (cand-m c) [bindings (hash-set (pmatch-bindings (cand-m c)) 'reason (cand-reason c))])))
  (define r (skip-range pol c index))
  (define ps2 (struct-copy pstate ps [skips (if r (cons r (pstate-skips ps)) (pstate-skips ps))]
                           [emitted-inits (hash-set (pstate-emitted-inits ps) (cand-init c) #t)]))
  ;; unrestricted and recent consumption never consume (CON.recent: detection
  ;; does not consume the initiator); retained reasons consume nothing
  (if (or defer? (memq (match-policy-consumption pol) '(unrestricted recent)) (memq (cand-reason c) (match-policy-retain pol)))
      ps2
      (consume ps2 (cand-m c))))

;; Union of several matches (CON.cumulative).
(define (union-all ctx ms) (for/fold ([acc (car ms)]) ([m (cdr ms)]) (pmatch-union ctx acc m)))

;; Process released candidates, grouped by terminator.
(define (process pol ps cs index W emit)
  (define ctx (oracle-ctx '() index #f +inf.0))
  ;; within a terminator's group: by initiator, larger matches first
  (define (order<? a b)
    (or (< (cand-init a) (cand-init b))
        (and (= (cand-init a) (cand-init b))
             (> (length (pmatch-events (cand-m a))) (length (pmatch-events (cand-m b)))))))
  (define groups (map (λ (g) (sort g order<?)) (group-by cand-term (sort cs < #:key cand-term))))
  (for/fold ([ps ps]) ([g (in-list groups)])
    (define live (filter (λ (c) (and (not (skipped? ps c)) (not (consumed? ps (cand-m c))))) g))
    (define chosen
      (case (match-policy-consumption pol)
        [(unrestricted) live]
        [(recent) (filter (λ (c) (>= (cand-init c) (cand-latest c))) live)]
        [(chronicle) (if (null? live) '() (list (argmin cand-init live)))]
        [(continuous) (for/list ([grp (group-by cand-init live)]) (car grp))]
        [(cumulative) (if (null? live) '()
                          (list (struct-copy cand (car live) [m (union-all ctx (map cand-m live))]
                                             [init (apply min (map cand-init live))])))]))
    (define continuous? (eq? (match-policy-consumption pol) 'continuous))
    (define ps-done
     (for/fold ([ps ps]) ([c (in-list chosen)]
                         ;; an earlier report in this group may have discarded or consumed it
                         #:unless (or (skipped? ps c) (consumed? ps (cand-m c))))
      (case (match-policy-output pol)
        [(every) (report pol ps c index emit #:defer-consumption continuous?)]
        [(first) (if (hash-ref (pstate-first-seen ps) (cand-init c) #f)
                     ps
                     (report pol (struct-copy pstate ps [first-seen (hash-set (pstate-first-seen ps) (cand-init c) #t)])
                             c index emit #:defer-consumption continuous?))]
        ;; the last match of an initiator: the latest terminator, the larger on a tie
        [(last) (define held (hash-ref (pstate-last-held ps) (cand-init c) #f))
                (if (and held (or (> (cand-term held) (cand-term c))
                                  (and (= (cand-term held) (cand-term c))
                                       (>= (length (pmatch-events (cand-m held))) (length (pmatch-events (cand-m c)))))))
                    ps
                    (struct-copy pstate ps [last-held (hash-set (pstate-last-held ps) (cand-init c) c)]))]
        [(maximal)
         (define ids (map event-id (pmatch-events (cand-m c))))
         (define contained?
           (for/or ([o (in-list (pstate-seen ps))])
             (define oids (map event-id (pmatch-events (cand-m o))))
             (and (> (length oids) (length ids)) (andmap (λ (i) (member i oids)) ids))))
         (if contained? ps (report pol ps c index emit #:defer-consumption continuous?))])))
    ;; CON.continuous: the completed initiators are consumed together
    (if continuous? (for/fold ([ps ps-done]) ([c chosen]) (consume ps (cand-m c))) ps-done)))

;; policy-step: new candidates from the engine. info: (hasheq 'index … 'latest …)
(define (policy-step pol ps ms info W now emit)
  (define index (hash-ref info 'index))
  (define new (for/list ([m (in-list ms)]) (make-cand pol m index (hash-ref info 'latest -1) 'match)))
  (define ps1 (struct-copy pstate ps [pending (append (pstate-pending ps) new)]
                           [seen (append (pstate-seen ps) new)]))
  (policy-flush pol ps1 info W now emit))

;; Release what is due at W (event time) and now (processing time).
(define (policy-flush pol ps info W now emit)
  (define index (hash-ref info 'index))
  (define-values (due waiting) (partition (λ (c) (released? c W)) (pstate-pending ps)))
  (define ps1 (process pol (struct-copy pstate ps [pending waiting]) due index W emit))
  ;; OUT.last: an initiator's last match is reported once its scope has passed
  (define-values (lasts-due lasts-left)
    (for/fold ([due '()] [left (hash)]) ([(i c) (in-hash (pstate-last-held ps1))])
      (if (released? c W) (values (cons c due) left) (values due (hash-set left i c)))))
  (define ps2 (for/fold ([ps (struct-copy pstate ps1 [last-held lasts-left])])
                        ([c (sort lasts-due < #:key cand-init)])
                (report pol ps c index emit)))
  ;; OUT.timeout-partial: partial matches past their time bound
  (flush-partials pol ps2 info W now emit))

;; ---------------------------------------------------------------------------
;; OUT.timeout-partial: with #:timeout-partial (Δ clock), the policy also
;; receives partial matches (matches of the pattern's body that its where
;; clause has not yet accepted). A partial whose initiator has had no
;; reported match for Δ (since the initiator's arrival on the processing
;; clock, or since its ts on the event-time clock) is reported as a timed-out
;; match with reason `timeout`; with CON.retain for that reason it consumes
;; nothing.

(define (policy-partials pol ps ms info W now emit)
  (define index (hash-ref info 'index))
  (define arrival (hash-ref info 'arrival (hash)))
  (define-values (Δ clock) (values (car (match-policy-timeout pol)) (cadr (match-policy-timeout pol))))
  (define partials
    (for/fold ([h (pstate-partials ps)]) ([m (in-list ms)] #:unless (consumed? ps m))
      (define c (make-cand pol m index -1 'timeout))
      (define start (if (eq? clock 'processing-time)
                        (hash-ref arrival (cand-init c) now)
                        (pmatch-ts m)))
      (define old (hash-ref h (cand-init c) #f))
      ;; keep the largest partial per initiator, due Δ after it began
      (if (and old (>= (length (pmatch-events (cand-m (cdr old)))) (length (pmatch-events m))))
          h
          (hash-set h (cand-init c) (cons (+ start Δ) (struct-copy cand c [release #f]))))))
  (flush-partials pol (struct-copy pstate ps [partials partials]) info W now emit))

(define (flush-partials pol ps info W now emit)
  (cond
    [(not (match-policy-timeout pol)) ps]
    [else
     (define clock (cadr (match-policy-timeout pol)))
     (define index (hash-ref info 'index))
     (for/fold ([ps ps]) ([(i entry) (in-hash (pstate-partials ps))])
       (define-values (due-at c) (values (car entry) (cdr entry)))
       (cond
         [(or (hash-ref (pstate-emitted-inits ps) i #f) (consumed? ps (cand-m c)))
          (struct-copy pstate ps [partials (hash-remove (pstate-partials ps) i)])]
         [(>= (if (eq? clock 'processing-time) now W) due-at)
          ;; the reported partial covers every pending partial within it
          (define ids (map event-id (pmatch-events (cand-m c))))
          (define left (for/hash ([(j e) (in-hash (pstate-partials ps))]
                                  #:unless (= j i)
                                  #:unless (andmap (λ (x) (member (event-id x) ids)) (pmatch-events (cand-m (cdr e)))))
                         (values j e)))
          (report pol (struct-copy pstate ps [partials left]) c index emit)]
         [else ps]))]))

;; The next moment the policy needs attention: deferred releases (event
;; time) and partial timeouts (their clock).
(define (policy-deadline pol ps clock)
  (define ds
    (append (if (eq? clock 'event-time)
                (filter values (map cand-release (append (pstate-pending ps) (hash-values (pstate-last-held ps)))))
                '())
            (if (and (match-policy-timeout pol) (eq? clock (cadr (match-policy-timeout pol))))
                (map car (hash-values (pstate-partials ps)))
                '())))
  (and (pair? ds) (car (sort ds (λ (a b) (< (deadline-time a) (deadline-time b)))))))

(provide
 (contract-out
  [struct match-policy ([consumption symbol?] [output symbol?] [skip any/c] [eval (or/c #f symbol?)]
                        [within (or/c #f duration?)] [timeout any/c] [retain (listof symbol?)])]
  [make-match-policy (->* () (#:consumption (or/c 'unrestricted 'recent 'chronicle 'continuous 'cumulative)
                              #:output (or/c 'every 'first 'last 'maximal)
                              #:skip any/c #:eval (or/c #f 'immediate 'deferred)
                              #:within (or/c #f duration?)
                              #:timeout-partial (or/c #f (list/c duration? (or/c 'event-time 'processing-time)))
                              #:retain (listof symbol?))
                          match-policy?)]
  [policy-init (-> match-policy? any/c)]
  [policy-step (-> match-policy? any/c (listof pmatch?) hash? time? time? procedure? any/c)]
  [policy-flush (-> match-policy? any/c hash? time? time? procedure? any/c)]
  [policy-partials (-> match-policy? any/c (listof pmatch?) hash? time? time? procedure? any/c)]
  [policy-deadline (-> match-policy? any/c symbol? (or/c #f real? strictly-after?))]))

;; The leaves that can begin a match of p (its initiators' steps).
(define (initial-leaves p)
  (cond
    [(p:primitive? p) (list p)]
    [(p:act? p) '()]
    [(p:seq? p) (append (initial-leaves (p:seq-left p))
                        (if (nullable? (p:seq-left p)) (initial-leaves (p:seq-right p)) '()))]
    [(p:neg-between? p) (initial-leaves (p:neg-between-left p))]
    [(p:absence? p) (initial-leaves (p:absence-body p))]
    [(p:unless? p) (initial-leaves (p:unless-body p))]
    [(p:aperiodic? p) (initial-leaves (p:aperiodic-open p))]
    [(p:periodic? p) (initial-leaves (p:periodic-open p))]
    [else (append-map initial-leaves (children p))]))

(provide
 (contract-out
  [initial-leaves (-> pat? (listof p:primitive?))]))
