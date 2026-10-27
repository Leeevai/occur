#lang racket/base
;; The reference oracle: a direct transcription of the semantics of §11.3.
;;
;; ⟦P⟧(H) is computed by enumerating candidate matches over a finite history
;; H (a list of events in arrival order). It is exponential and serves only
;; as a test oracle for the incremental engine (pattern/nfa.rkt). These are
;; the most permissive semantics: every qualifying combination counts
;; (skip-till-any-match, no consumption).
;;
;; A match is a set of events with bindings and an interval. It also records
;; which leaf each event was taken for and the end point of the step before
;; it (#f at the start of the pattern), which the selection strategies of
;; §12 need.
(require racket/contract
         racket/list
         "../time.rkt"
         "../event.rkt"
         "ast.rkt")

;; events: list sorted by arrival index; bindings: hasheq name -> event, list
;; of events, or (for PER*) list of tick times; leaves: list of leaf-use.
(struct pmatch (events bindings ts te leaves) #:transparent)
;; pred: #f, or the end point (te . arrival-index) of the preceding step
(struct leaf-use (leaf event pred) #:transparent)

;; The evaluation context: the history, its arrival indices, the activation
;; instant (for ACT), and the horizon (for periodic ticks without a close).
(struct oracle-ctx (history index activation horizon) #:transparent)

(define (index-of-event ctx e) (hash-ref (oracle-ctx-index ctx) (event-id e)))

(define empty-match (pmatch '() (hasheq) +inf.0 -inf.0 '()))

(define (pmatch-ids m) (map event-id (pmatch-events m)))

;; The end point of a match: its end time and the arrival index of its last
;; event (−1 when it has none, as for ACT).
(define (pmatch-end ctx m)
  (cons (pmatch-te m)
        (for/fold ([i -1]) ([e (in-list (pmatch-events m))]) (max i (index-of-event ctx e)))))

(define (merge-events ctx a b)
  (sort (remove-duplicates (append a b) (λ (x y) (equal? (event-id x) (event-id y))))
        < #:key (λ (e) (index-of-event ctx e))))

(define (merge-bindings a b)
  (for/fold ([h a]) ([(k v) (in-hash b)])
    (define old (hash-ref h k #f))
    (cond [(not (hash-has-key? h k)) (hash-set h k v)]
          [(and (list? old) (list? v)) (hash-set h k (append old v))]
          [else h])))

;; m1 ∪ m2. With pred, the leaves that begin m2 get pred as their
;; predecessor (m2 follows a step that ended at pred).
(define (pmatch-union ctx m1 m2 #:pred [pred #f])
  (pmatch (merge-events ctx (pmatch-events m1) (pmatch-events m2))
         (merge-bindings (pmatch-bindings m1) (pmatch-bindings m2))
         (time-min (pmatch-ts m1) (pmatch-ts m2))
         (time-max (pmatch-te m1) (pmatch-te m2))
         (append (pmatch-leaves m1)
                 (if pred
                     (for/list ([u (in-list (pmatch-leaves m2))])
                       (if (leaf-use-pred u) u (struct-copy leaf-use u [pred pred])))
                     (pmatch-leaves m2)))))

(define (disjoint? m1 m2)
  (define ids (map event-id (pmatch-events m1)))
  (not (for/or ([e (in-list (pmatch-events m2))]) (member (event-id e) ids))))

;; A canonical key: two derivations of the same match are one match.
(define (pmatch-key m)
  (list (pmatch-ids m)
        (sort (for/list ([(k v) (in-hash (pmatch-bindings m))])
                (cons k (cond [(event? v) (event-id v)]
                              [(list? v) (map (λ (x) (if (event? x) (event-id x) x)) v)]
                              [else v])))
              symbol<? #:key car)
        (pmatch-ts m) (pmatch-te m)))

(define (dedupe ms) (remove-duplicates ms #:key pmatch-key))

;; ---------------------------------------------------------------------------
;; Each operator registers a handler: (ctx pattern denote) -> list of matches.

(define handlers '())
(define (register! pred handler) (set! handlers (append handlers (list (cons pred handler)))))

(define (denote* ctx p)
  (define h (for/first ([x (in-list handlers)] #:when ((car x) p)) (cdr x)))
  (unless h (raise-arguments-error 'denote "no semantics for this pattern" "pattern" p))
  (dedupe (h ctx p (λ (q) (denote* ctx q)))))

(define (denote p history #:activation [activation #f] #:horizon [horizon #f])
  (define index (for/hash ([e (in-list history)] [i (in-naturals)]) (values (event-id e) i)))
  (define ctx (oracle-ctx history index activation
                          (or horizon (for/fold ([h -inf.0]) ([e history]) (time-max h (event-te e))))))
  (sort (denote* ctx p) pmatch<?))

(define (pmatch<? a b)
  (or (< (pmatch-ts a) (pmatch-ts b))
      (and (= (pmatch-ts a) (pmatch-ts b))
           (or (< (pmatch-te a) (pmatch-te b))
               (and (= (pmatch-te a) (pmatch-te b))
                    (string<? (format "~a" (pmatch-key a)) (format "~a" (pmatch-key b))))))))

;; ===========================================================================
;; PT.primitive: ⟦x:τ[p]⟧ = { {e} : e ∈ H, type(e) = τ, p(e) }, binding x ↦ e

(register!
 p:primitive?
 (λ (ctx p denote)
   (for/list ([e (in-list (oracle-ctx-history ctx))]
              #:when (or (not (p:primitive-type p)) (eq? (event-type e) (p:primitive-type p)))
              #:when (or (not (p:primitive-pred p)) ((p:primitive-pred p) e)))
     (pmatch (list e)
            (if (p:primitive-binding p) (hasheq (p:primitive-binding p) e) (hasheq))
            (event-ts e) (event-te e)
            (list (leaf-use p e #f))))))

;; ===========================================================================
;; PT.activation: ⟦ACT⟧ = { ∅ } with ts = te = the activation instant

(register!
 p:act?
 (λ (ctx p denote)
   (define t0 (oracle-ctx-activation ctx))
   (if t0 (list (pmatch '() (hasheq) t0 t0 '())) '())))

(provide
 (contract-out
  [struct pmatch ([events (listof event?)] [bindings hash?] [ts time?] [te time?] [leaves (listof leaf-use?)])]
  [struct leaf-use ([leaf p:primitive?] [event event?] [pred (or/c #f pair?)])]
  [struct oracle-ctx ([history (listof event?)] [index hash?] [activation (or/c #f time?)] [horizon time?])]
  [empty-match pmatch?]
  [pmatch-ids (-> pmatch? (listof any/c))]
  [pmatch-key (-> pmatch? list?)]
  [pmatch-end (-> oracle-ctx? pmatch? pair?)]
  [index-of-event (-> oracle-ctx? event? exact-nonnegative-integer?)]
  [denote (->* (pat? (listof event?)) (#:activation (or/c #f time?) #:horizon (or/c #f time?))
               (listof pmatch?))]))
;; ===========================================================================
;; PT.sequence: ⟦P₁ ; P₂⟧ = { m₁ ∪ m₂ : m₁ ∈ ⟦P₁⟧, m₂ ∈ ⟦P₂⟧, te(m₁) < ts(m₂) }.
;; Events with equal timestamps are not in sequence (no tiebreak, decision
;; 0010). The steps that begin m₂ follow the end of m₁.

;; The predecessor point a match gives the step after it; an empty match
;; (ε) constrains nothing.
(define (pred-of ctx m)
  (if (and (null? (pmatch-events m)) (eqv? (pmatch-te m) -inf.0)) #f (pmatch-end ctx m)))

(register!
 p:seq?
 (λ (ctx p denote)
   (define L (denote (p:seq-left p)))
   (define R (denote (p:seq-right p)))
   (for*/list ([m1 (in-list L)] [m2 (in-list R)] #:when (< (pmatch-te m1) (pmatch-ts m2)))
     (pmatch-union ctx m1 m2 #:pred (pred-of ctx m1)))))

;; ===========================================================================
;; PT.conjunction: ⟦P₁ ∧ P₂⟧ = { m₁ ∪ m₂ : m₁ ∩ m₂ = ∅ }, in any order.

(register!
 p:and?
 (λ (ctx p denote)
   (define L (denote (p:and-left p)))
   (define R (denote (p:and-right p)))
   (for*/list ([m1 (in-list L)] [m2 (in-list R)] #:when (disjoint? m1 m2))
     (pmatch-union ctx m1 m2))))
