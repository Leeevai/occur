#lang racket/base
;; The incremental pattern engine (spec §11, §17).
;;
;; A pattern compiles to a tree of incremental match automata, one per AST
;; node, as in an NFA with match buffers (NFA^b): each node keeps buffers of
;; the complete sub-matches it has seen and, when an input arrives, returns
;; only its new matches (a delta). Sequence, conjunction and Kleene nodes
;; join a child's new matches with the other child's buffer (delta joins), so
;; events may arrive in any order. Non-monotone nodes (negation, absence,
;; span absence, aperiodic and periodic forms) hold candidates and decide them
;; when the watermark guarantees no event can still change the answer
;; (EVAL.deferred); a hybrid-clock absence decides on the processing clock.
;; See docs/decisions/0011-incremental-engine.md.
;;
;; The engine state is immutable, so a pattern operator can keep it per key
;; and a checkpoint can hold it by reference.
(require racket/contract
         racket/list
         "../time.rkt"
         "../event.rkt"
         "../scheduler.rkt"
         "ast.rkt"
         "reference.rkt")

;; index: event-id -> global arrival index; pindex: event-id -> index within
;; the key's partition; nodes: path -> node state; W: the watermark; lastW:
;; the newest finite watermark; now: the processing time; maxte: the newest
;; end time seen. At the end of the stream, periodic ticks without a close
;; run up to max(lastW, maxte), the evaluation horizon (decision 0011).
(struct engine (pattern activation index pindex pcount W lastW now maxte nodes selection) #:transparent)

(define (make-engine p #:activation [activation #f] #:selection [selection 'any])
  (define problem (pattern-problem p))
  (when problem (raise-arguments-error 'make-engine problem))
  (engine p activation (hash) (hash) 0 -inf.0 -inf.0 -inf.0 -inf.0 (hash) selection))

(define (engine-horizon e) (time-max (engine-lastW e) (engine-maxte e)))

;; ---------------------------------------------------------------------------
;; Inputs: '(start), (list 'event e), (list 'advance)

(define (engine-start e) (engine-step e '(start)))

(define (engine-event e ev gidx)
  (define e2 (struct-copy engine e
                          [index (hash-set (engine-index e) (event-id ev) gidx)]
                          [pindex (hash-set (engine-pindex e) (event-id ev) (engine-pcount e))]
                          [pcount (add1 (engine-pcount e))]
                          [maxte (time-max (engine-maxte e) (event-te ev))]))
  (engine-step e2 (list 'event ev)))

;; last-finite: the newest finite watermark of the whole stream, which a
;; keyed operator tracks across keys
(define (engine-advance e W now #:last-finite [last-finite -inf.0])
  (engine-step (struct-copy engine e [W (time-max W (engine-W e))] [now (time-max now (engine-now e))]
                            [lastW (time-max last-finite
                                             (if (< W +inf.0) (time-max W (engine-lastW e)) (engine-lastW e)))])
               '(advance)))

;; ---------------------------------------------------------------------------
;; Node state helpers

(struct node (stores keys) #:transparent)   ; stores: hasheq tag -> value; keys: hash key -> #t

(define (engine-step e0 input)
  (define nodes (engine-nodes e0))
  (define ctx (oracle-ctx '() (engine-index e0) (engine-activation e0) +inf.0))
  (define W (engine-W e0))
  (define now (engine-now e0))
  ;; the horizon for periodic ticks: the watermark, but at the end of the
  ;; stream the newest end time seen (as in the oracle)
  (define horizon (if (eqv? W +inf.0) (engine-horizon e0) W))
  (define selection (engine-selection e0))
  (define (get path) (hash-ref nodes path (λ () (node (hasheq) (hash)))))
  (define (store path tag [default '()]) (hash-ref (node-stores (get path)) tag default))
  (define (set-store! path tag v)
    (define n (get path))
    (set! nodes (hash-set nodes path (struct-copy node n [stores (hash-set (node-stores n) tag v)]))))
  ;; keep only matches this node has not produced before
  (define (fresh! path ms)
    (define n (get path))
    (define-values (out keys)
      (for/fold ([out '()] [keys (node-keys n)]) ([m (in-list ms)])
        (define k (pmatch-key m))
        (if (hash-ref keys k #f) (values out keys) (values (cons m out) (hash-set keys k #t)))))
    (set! nodes (hash-set nodes path (struct-copy node (get path) [keys keys])))
    (reverse out))
  (define (pred-of m) (if (and (null? (pmatch-events m)) (eqv? (pmatch-te m) -inf.0)) #f (pmatch-end ctx m)))
  ;; m1 ; m2 joined, subject to the selection strategy for m2's first steps
  (define (join-seq m1 m2)
    (define u (pmatch-union ctx m1 m2 #:pred (pred-of m1)))
    (and (selected? u) u))
  (define (selected? m)
    ((selection-check selection) m (engine-index e0) (engine-pindex e0) (λ (leaf) (store (leaf-path leaf) 'qual))))
  (define (lift m) (struct-copy pmatch m [bindings (for/hasheq ([(k v) (in-hash (pmatch-bindings m))])
                                                      (values k (if (list? v) v (list v))))]))
  (define (go p path)
    (cond
      ;; PT.primitive
      [(p:primitive? p)
       (set! leaf-paths (hash-set leaf-paths (p:primitive-id p) path))
       (cond
         [(and (eq? (car input) 'event)
               (let ([ev (cadr input)])
                 (and (or (not (p:primitive-type p)) (eq? (event-type ev) (p:primitive-type p)))
                      (or (not (p:primitive-pred p)) ((p:primitive-pred p) ev)))))
          (define ev (cadr input))
          (set-store! path 'qual (cons ev (store path 'qual)))
          (list (pmatch (list ev) (if (p:primitive-binding p) (hasheq (p:primitive-binding p) ev) (hasheq))
                        (event-ts ev) (event-te ev) (list (leaf-use p ev #f))))]
         [else '()])]
      ;; PT.activation
      [(p:act? p)
       (if (and (eq? (car input) 'start) (engine-activation e0))
           (list (pmatch '() (hasheq) (engine-activation e0) (engine-activation e0) '()))
           '())]
      ;; PT.sequence and PT.conjunction: delta joins
      [(or (p:seq? p) (p:and? p))
       (define seq? (p:seq? p))
       (define dL (go (if seq? (p:seq-left p) (p:and-left p)) (cons 0 path)))
       (define dR (go (if seq? (p:seq-right p) (p:and-right p)) (cons 1 path)))
       (define L (store path 'L))
       (define R (store path 'R))
       (define (pair l r)
         (if seq?
             (and (< (pmatch-te l) (pmatch-ts r)) (join-seq l r))
             (and (disjoint? l r) (pmatch-union ctx l r))))
       (define new (filter values (append (for*/list ([l dL] [r (append R dR)]) (pair l r))
                                          (for*/list ([l L] [r dR]) (pair l r)))))
       (set-store! path 'L (append L dL))
       (set-store! path 'R (append R dR))
       (fresh! path new)]
      ;; PT.disjunction
      [(p:or? p)
       (fresh! path (append (go (p:or-left p) (cons 0 path)) (go (p:or-right p) (cons 1 path))))]
      ;; PT.negation: pairs wait until W >= ts(m₂), then any q in the gap blocks
      [(p:neg-between? p)
       (define dL (go (p:neg-between-left p) (cons 0 path)))
       (define dQ (go (p:neg-between-neg p) (cons 1 path)))
       (define dR (go (p:neg-between-right p) (cons 2 path)))
       (define L (store path 'L)) (define R (store path 'R))
       (define Q (append (store path 'Q) dQ))
       ;; candidates remember their gap: (match te(m₁) ts(m₂))
       (define (pair l r)
         (and (< (pmatch-te l) (pmatch-ts r))
              (let ([u (join-seq l r)]) (and u (list u (pmatch-te l) (pmatch-ts r))))))
       (define new-pending (filter values (append (for*/list ([l dL] [r (append R dR)]) (pair l r))
                                                  (for*/list ([l L] [r dR]) (pair l r)))))
       (define pending (append (store path 'pending) new-pending))
       (define-values (due waiting) (partition (λ (x) (>= W (caddr x))) pending))
       (set-store! path 'L (append L dL)) (set-store! path 'R (append R dR)) (set-store! path 'Q Q)
       (set-store! path 'pending waiting)
       (fresh! path (for/list ([x due]
                               #:unless (for/or ([q Q]) (and (< (cadr x) (pmatch-ts q)) (< (pmatch-te q) (caddr x)))))
                      (car x)))]
      ;; PT.absence: m waits until W > te(m) + Δ (or now >= it, hybrid)
      [(p:absence? p)
       (define dP (go (p:absence-body p) (cons 0 path)))
       (define dQ (go (p:absence-neg p) (cons 1 path)))
       (define Q (append (store path 'Q) dQ))
       (define Δ (p:absence-window p))
       (define hybrid? (eq? (p:absence-clock p) 'hybrid))
       (define pending (append (store path 'pending) dP))
       (define (due? m) (if hybrid? (>= now (+ (pmatch-te m) Δ)) (> W (+ (pmatch-te m) Δ))))
       (define-values (due waiting) (partition due? pending))
       (set-store! path 'Q Q) (set-store! path 'pending waiting)
       (fresh! path (for/list ([m due]
                               #:unless (for/or ([q Q]) (and (< (pmatch-te m) (pmatch-ts q))
                                                             (<= (pmatch-ts q) (+ (pmatch-te m) Δ)))))
                      (struct-copy pmatch m [te (+ (pmatch-te m) Δ)])))]
      ;; PT.span-absence: m waits until W >= te(m)
      [(p:unless? p)
       (define dP (go (p:unless-body p) (cons 0 path)))
       (define dQ (go (p:unless-neg p) (cons 1 path)))
       (define Q (append (store path 'Q) dQ))
       (define pending (append (store path 'pending) dP))
       (define-values (due waiting) (partition (λ (m) (>= W (pmatch-te m))) pending))
       (set-store! path 'Q Q) (set-store! path 'pending waiting)
       (fresh! path (for/list ([m due]
                               #:unless (for/or ([q Q]) (and (not (part-of? q m)) (< (pmatch-ts m) (pmatch-ts q))
                                                             (< (pmatch-te q) (pmatch-te m)))))
                      m))]
      ;; PT.kleene-plus, PT.kleene-star, PT.repeat: chains grow around each new copy
      [(p:repeat? p)
       (define lo (p:repeat-min p)) (define hi (p:repeat-max p))
       (define names (map car (pattern-bindings (p:repeat-body p) #t)))
       (define eps (struct-copy pmatch empty-match [bindings (for/hasheq ([n names]) (values n '()))]))
       (define dO (map lift (go (p:repeat-body p) (cons 0 path))))
       (define-values (chains new)
         (for/fold ([chains (store path 'chains)] [new '()]) ([x (in-list dO)])
           ;; the empty prefix and suffix carry no bindings: a chain binds only
           ;; the names its copies bound (as in the oracle)
           (define pre (cons (cons 0 empty-match) (filter (λ (c) (< (pmatch-te (cdr c)) (pmatch-ts x))) chains)))
           (define suf (cons (cons 0 empty-match) (filter (λ (c) (> (pmatch-ts (cdr c)) (pmatch-te x))) chains)))
           (define made
             (for*/list ([pc (in-list pre)] [sc (in-list suf)]
                         #:when (<= (+ (car pc) 1 (car sc)) hi)
                         [xs (in-value (join-seq x (cdr sc)))] #:when xs
                         [m (in-value (join-seq (cdr pc) xs))] #:when m)
               (cons (+ (car pc) 1 (car sc)) m)))
           (values (append chains made) (append new made))))
       (set-store! path 'chains chains)
       (fresh! path (append (if (and (eq? (car input) 'start) (zero? lo)) (list eps) '())
                            (for/list ([c new] #:when (>= (car c) lo)) (cdr c))))]
      ;; PT.any
      [(p:any? p)
       (define parts (p:any-parts p))
       (define deltas (for/list ([q parts] [i (in-naturals)]) (go q (cons i path))))
       (define olds (for/list ([i (in-range (length parts))]) (store path i)))
       (define new
         (for*/list ([combo (in-list (combinations (range (length parts)) (p:any-k p)))]
                     [choice (in-list (apply cartesian-product
                                             (for/list ([i combo])
                                               (append (map (λ (m) (cons 'old m)) (list-ref olds i))
                                                       (map (λ (m) (cons 'new m)) (list-ref deltas i))))))]
                     #:when (ormap (λ (c) (eq? (car c) 'new)) choice)
                     [ms (in-value (map cdr choice))]
                     #:when (for*/and ([(x i) (in-indexed ms)] [(y j) (in-indexed ms)] #:when (< i j)) (disjoint? x y)))
           (for/fold ([acc (car ms)]) ([m (cdr ms)]) (pmatch-union ctx acc m))))
       (for ([i (in-range (length parts))]) (set-store! path i (append (list-ref olds i) (list-ref deltas i))))
       (fresh! path new)]
      ;; PT.within
      [(p:within? p)
       (fresh! path (filter (λ (m) (<= (- (pmatch-te m) (pmatch-ts m)) (p:within-duration p)))
                            (go (p:within-body p) (cons 0 path))))]
      ;; PT.where
      [(p:where? p)
       (fresh! path (filter (λ (m) ((p:where-pred p) (pmatch-bindings m))) (go (p:where-body p) (cons 0 path))))]
      ;; PT.aperiodic, PT.aperiodic-cumulative
      [(p:aperiodic? p)
       (define dO (go (p:aperiodic-open p) (cons 0 path)))
       (define dB (go (p:aperiodic-body p) (cons 1 path)))
       (define dC (go (p:aperiodic-close p) (cons 2 path)))
       (define opens (append (store path 'opens) dO))
       (define bodies (append (store path 'bodies) dB))
       (define closes (append (store path 'closes) dC))
       (set-store! path 'opens opens) (set-store! path 'bodies bodies) (set-store! path 'closes closes)
       (cond
         [(not (p:aperiodic-cumulative? p))
          ;; (e₁, e₂) pairs wait until W > te(e₂); a close in between blocks
          (define cands (append (for*/list ([o dO] [b bodies] #:when (< (pmatch-te o) (pmatch-ts b))) (cons o b))
                                (for*/list ([o (store path 'opens)] [b dB] #:when (< (pmatch-te o) (pmatch-ts b))) (cons o b))))
          (define pending (append (store path 'pending) cands))
          (define-values (due waiting) (partition (λ (x) (> W (pmatch-te (cdr x)))) pending))
          (set-store! path 'pending waiting)
          (fresh! path (for/list ([x due]
                                  #:unless (for/or ([c closes]) (and (> (pmatch-ts c) (pmatch-te (car x)))
                                                                     (<= (pmatch-ts c) (pmatch-te (cdr x))))))
                         (pmatch-union ctx (car x) (cdr x))))]
         [else
          ;; an open interval reports when W passes its first closer
          (define done (store path 'done (hash)))
          (define names (map car (pattern-bindings (p:aperiodic-body p) #t)))
          (define-values (out done2)
            (for/fold ([out '()] [done done]) ([o opens] #:unless (hash-ref done (pmatch-key o) #f))
              (define c (first-closer-of ctx o closes))
              (cond
                [(and c (> W (pmatch-ts c)))
                 ;; a list binding is time-ordered (§11.1)
                 (define inside (sort (filter (λ (b) (and (< (pmatch-te o) (pmatch-ts b)) (< (pmatch-te b) (pmatch-ts c)))) bodies)
                                      (λ (x y) (or (< (pmatch-ts x) (pmatch-ts y))
                                                   (and (= (pmatch-ts x) (pmatch-ts y)) (< (pmatch-te x) (pmatch-te y)))))))
                 (define base (pmatch-union ctx o (struct-copy pmatch empty-match [bindings (for/hasheq ([n names]) (values n '()))])))
                 (values (cons (pmatch-union ctx (for/fold ([acc base]) ([b inside]) (pmatch-union ctx acc (lift b))) c) out)
                         (hash-set done (pmatch-key o) #t))]
                [else (values out done)])))
          (set-store! path 'done done2)
          (fresh! path (reverse out))])]
      ;; PT.periodic, PT.periodic-cumulative
      [(p:periodic? p)
       (define dO (go (p:periodic-open p) (cons 0 path)))
       (define dC (go (p:periodic-close p) (cons 1 path)))
       (define opens (append (store path 'opens) dO))
       (define closes (append (store path 'closes) dC))
       (set-store! path 'opens opens) (set-store! path 'closes closes)
       (define Δ (p:periodic-period p))
       (define emitted (store path 'emitted (hash)))          ; open key -> ticks emitted (or 'done)
       (define-values (out emitted2)
         (for/fold ([out '()] [emitted emitted]) ([o opens])
           (define k (pmatch-key o))
           (define c (first-closer-of ctx o closes))
           (define limit (if c (pmatch-ts c) +inf.0))
           (cond
             [(eq? (hash-ref emitted k 0) 'done) (values out emitted)]
             [(not (p:periodic-cumulative? p))
              ;; tick j is decided once W > it, if it is before the close
              (let loop ([j (add1 (hash-ref emitted k 0))] [out out])
                (define t (+ (pmatch-te o) (* j Δ)))
                (cond [(and (> W t) (< t limit) (<= t horizon))
                       (loop (add1 j) (cons (struct-copy pmatch o [te t]) out))]
                      [else (values out (hash-set emitted k (sub1 j)))]))]
             [(and c (> W (pmatch-ts c)))
              (define ticks (for/list ([j (in-naturals 1)] #:break (>= (+ (pmatch-te o) (* j Δ)) limit))
                              (+ (pmatch-te o) (* j Δ))))
              (define m (pmatch-union ctx o c))
              (values (cons (struct-copy pmatch m [bindings (hash-set (pmatch-bindings m) 'ticks ticks)]) out)
                      (hash-set emitted k 'done))]
             [else (values out emitted)])))
       (set-store! path 'emitted emitted2)
       (fresh! path (reverse out))]
      [else (raise-arguments-error 'engine "unknown pattern node" "pattern" p)]))
  (define leaf-paths (hash))
  (define (leaf-path leaf) (hash-ref leaf-paths (p:primitive-id leaf)))
  (define delta (go (engine-pattern e0) '()))
  (values (struct-copy engine e0 [nodes nodes]) delta))

(define (part-of? q m)
  (define ids (map event-id (pmatch-events m)))
  (and (pair? (pmatch-events q)) (andmap (λ (e) (member (event-id e) ids)) (pmatch-events q))))

(define (disjoint? m1 m2)
  (define ids (map event-id (pmatch-events m1)))
  (not (for/or ([e (in-list (pmatch-events m2))]) (member (event-id e) ids))))

(define (first-closer-of ctx o closes)
  (define after (filter (λ (c) (> (pmatch-ts c) (pmatch-te o))) closes))
  (and (pair? after)
       (car (sort after (λ (x y) (or (< (pmatch-ts x) (pmatch-ts y))
                                     (and (= (pmatch-ts x) (pmatch-ts y))
                                          (or (< (pmatch-te x) (pmatch-te y))
                                              (and (= (pmatch-te x) (pmatch-te y))
                                                   (string<? (closer-ids x) (closer-ids y)))))))))))

(define (in-indexed l) (in-parallel (in-list l) (in-naturals)))

;; ---------------------------------------------------------------------------
;; Selection strategies (§12.1) are checked where a step follows another:
;; see policies.rkt. Skip-till-any-match accepts every candidate.

(define current-selection-checks (make-hasheq))
(define (selection-check s)
  (hash-ref current-selection-checks s
            (λ () (if (eq? s 'any)
                      (λ (m index pindex qual-of) #t)
                      (raise-arguments-error 'engine "unknown selection strategy" "strategy" s)))))
(define (register-selection-check! name f) (hash-set! current-selection-checks name f))

;; ---------------------------------------------------------------------------
;; The next moment the engine needs attention without new data: the
;; earliest pending decision, as an event-time deadline and as a
;; processing-time deadline (hybrid absence). Only undecided work is
;; reported; at the final watermark every event-time decision has been made.

(define (engine-deadlines e)
  (define evt '())
  (define proc '())
  (define final? (eqv? (engine-W e) +inf.0))
  (define ctx (oracle-ctx '() (engine-index e) (engine-activation e) +inf.0))
  (define (add-evt! d) (unless final? (set! evt (cons d evt))))
  (let walk ([p (engine-pattern e)] [path '()])
    (define (st tag [default '()])
      (hash-ref (node-stores (hash-ref (engine-nodes e) path (λ () (node (hasheq) (hash))))) tag default))
    (cond
      [(p:neg-between? p) (for ([x (st 'pending)]) (add-evt! (caddr x)))]
      [(p:absence? p)
       (for ([m (st 'pending)])
         (define t (+ (pmatch-te m) (p:absence-window p)))
         (if (eq? (p:absence-clock p) 'hybrid) (set! proc (cons t proc)) (add-evt! (strictly-after t))))]
      [(p:unless? p) (for ([m (st 'pending)]) (add-evt! (pmatch-te m)))]
      [(p:aperiodic? p)
       (if (p:aperiodic-cumulative? p)
           (for ([o (st 'opens)] #:unless (hash-ref (st 'done (hash)) (pmatch-key o) #f))
             (define c (first-closer-of ctx o (st 'closes)))
             (when c (add-evt! (strictly-after (pmatch-ts c)))))
           (for ([x (st 'pending)]) (add-evt! (strictly-after (pmatch-te (cdr x))))))]
      [(p:periodic? p)
       (for ([o (st 'opens)])
         (define k (hash-ref (st 'emitted (hash)) (pmatch-key o) 0))
         (define c (first-closer-of ctx o (st 'closes)))
         (unless (eq? k 'done)
           (cond
             [(p:periodic-cumulative? p) (when c (add-evt! (strictly-after (pmatch-ts c))))]
             [else
              (define t (+ (pmatch-te o) (* (add1 k) (p:periodic-period p))))
              (when (or (not c) (< t (pmatch-ts c))) (add-evt! (strictly-after t)))])))]
      [else (void)])
    (for ([c (children p)] [i (in-naturals)]) (walk c (cons i path))))
  (values (and (pair? evt) (car (sort evt (λ (a b) (< (deadline-time a) (deadline-time b))))))
          (and (pair? proc) (apply min proc))))

(provide
 (contract-out
  [engine? (-> any/c boolean?)]
  [engine-W (-> engine? time?)]
  [engine-index (-> engine? hash?)]
  [engine-pindex (-> engine? hash?)]
  [make-engine (->* (pat?) (#:activation (or/c #f time?) #:selection symbol?) engine?)]
  [engine-horizon (-> engine? time?)]
  [engine-start (-> engine? (values engine? (listof pmatch?)))]
  [engine-event (-> engine? event? exact-nonnegative-integer? (values engine? (listof pmatch?)))]
  [engine-advance (->* (engine? time? time?) (#:last-finite time?) (values engine? (listof pmatch?)))]
  [engine-deadlines (-> engine? (values (or/c #f deadline/c) (or/c #f time?)))]
  [register-selection-check! (-> symbol? procedure? void?)]))

;; Ties between closers break by their event ids, which do not depend on
;; arrival order (decision 0010).
(define (closer-ids m) (format "~a" (sort (map (λ (e) (format "~a" (event-id e))) (pmatch-events m)) string<?)))
