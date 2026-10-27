#lang racket/base
;; The pattern AST (spec §11.2).
;;
;;   P ::= x:τ[p] | ACT | P ; P | P ∧ P | P ∨ P | P ; ¬Q ; P | P ; ¬Q within Δ
;;       | P unless Q | P+ | P* | P{n,m} | ANY(k; P₁, …, Pₙ) | P within Δ
;;       | P where θ | APER(P₁, P₂, P₃) | APER*(…) | PER(P₁, Δ, P₃) | PER*(…)
;;
;; Every primitive carries a unique leaf id, so selection strategies (§12)
;; can tell which step an event was taken for.
(require racket/contract
         racket/list
         "../time.rkt"
         "../event.rkt")

(struct pat () #:transparent)
;; x:τ[p]: binding (symbol or #f), type (symbol, or #f for any type), pred
;; (event -> any/c, or #f), id (unique leaf id)
(struct p:primitive pat (binding type pred id) #:transparent)
(struct p:act pat () #:transparent)                                   ; ACT
(struct p:seq pat (left right) #:transparent)                         ; P ; P
(struct p:and pat (left right) #:transparent)                         ; P ∧ P
(struct p:or pat (left right) #:transparent)                          ; P ∨ P
(struct p:neg-between pat (left neg right) #:transparent)             ; P ; ¬Q ; P
(struct p:absence pat (body neg window clock) #:transparent)          ; P ; ¬Q within Δ
(struct p:unless pat (body neg) #:transparent)                        ; P unless Q
(struct p:repeat pat (body min max) #:transparent)                    ; P{n,m}; P+ = {1,∞}, P* = {0,∞}
(struct p:any pat (k parts) #:transparent)                            ; ANY(k; P₁, …, Pₙ)
(struct p:within pat (body duration) #:transparent)                   ; P within Δ
(struct p:where pat (body pred) #:transparent)                        ; P where θ (θ: bindings -> any/c)
(struct p:aperiodic pat (open body close cumulative?) #:transparent)  ; APER / APER*
(struct p:periodic pat (open period close cumulative?) #:transparent) ; PER / PER*

;; ---------------------------------------------------------------------------
;; Constructors

(define leaf-counter 0)
(define (fresh-leaf-id)
  (set! leaf-counter (add1 leaf-counter))
  (string->symbol (format "leaf~a" leaf-counter)))

(define (prim type #:bind [x #f] #:when [pred #f] #:id [id #f])
  (p:primitive x type pred (or id (fresh-leaf-id))))
(define act (p:act))

(define (seq p . ps) (foldl (λ (q acc) (p:seq acc q)) p ps))
(define (conj p . ps) (foldl (λ (q acc) (p:and acc q)) p ps))
(define (disj p . ps) (foldl (λ (q acc) (p:or acc q)) p ps))
(define (neg-between left neg right) (p:neg-between left neg right))
(define (absence body neg window #:clock [clock 'event-time]) (p:absence body neg window clock))
(define (span-absence body neg) (p:unless body neg))
(define (kleene+ p) (p:repeat p 1 +inf.0))
(define (kleene* p) (p:repeat p 0 +inf.0))
(define (repeat p n [m n]) (p:repeat p n m))
(define (any-of k . ps) (p:any k ps))
(define (within p d) (p:within p d))
(define (where p θ) (p:where p θ))
(define (aperiodic open body close) (p:aperiodic open body close #f))
(define (aperiodic* open body close) (p:aperiodic open body close #t))
(define (periodic open period close) (p:periodic open period close #f))
(define (periodic* open period close) (p:periodic open period close #t))

;; ---------------------------------------------------------------------------
;; Analysis

;; A pattern is nullable when it can match without any event (ACT, P*, P{0,m}).
(define (nullable? p)
  (cond
    [(p:primitive? p) #f]
    [(p:act? p) #t]
    [(p:seq? p) (and (nullable? (p:seq-left p)) (nullable? (p:seq-right p)))]
    [(p:and? p) (and (nullable? (p:and-left p)) (nullable? (p:and-right p)))]
    [(p:or? p) (or (nullable? (p:or-left p)) (nullable? (p:or-right p)))]
    [(p:neg-between? p) #f]
    [(p:absence? p) (nullable? (p:absence-body p))]
    [(p:unless? p) (nullable? (p:unless-body p))]
    [(p:repeat? p) (or (zero? (p:repeat-min p)) (nullable? (p:repeat-body p)))]
    [(p:any? p) (>= (count nullable? (p:any-parts p)) (p:any-k p))]
    [(p:within? p) (nullable? (p:within-body p))]
    [(p:where? p) (nullable? (p:where-body p))]
    [else #f]))

;; Bindings: an association list name -> 'single or 'list. Bindings under a
;; repetition, or of the body of a cumulative aperiodic, are lists.
(define (pattern-bindings p [in-list? #f])
  (define (kind) (if in-list? 'list 'single))
  (remove-duplicates
   (cond
     [(p:primitive? p) (if (p:primitive-binding p) (list (cons (p:primitive-binding p) (kind))) '())]
     [(p:act? p) '()]
     [(p:seq? p) (append (pattern-bindings (p:seq-left p) in-list?) (pattern-bindings (p:seq-right p) in-list?))]
     [(p:and? p) (append (pattern-bindings (p:and-left p) in-list?) (pattern-bindings (p:and-right p) in-list?))]
     [(p:or? p) (append (pattern-bindings (p:or-left p) in-list?) (pattern-bindings (p:or-right p) in-list?))]
     [(p:neg-between? p) (append (pattern-bindings (p:neg-between-left p) in-list?)
                                 (pattern-bindings (p:neg-between-right p) in-list?))]
     [(p:absence? p) (pattern-bindings (p:absence-body p) in-list?)]
     [(p:unless? p) (pattern-bindings (p:unless-body p) in-list?)]
     [(p:repeat? p) (pattern-bindings (p:repeat-body p) #t)]
     [(p:any? p) (append-map (λ (q) (pattern-bindings q in-list?)) (p:any-parts p))]
     [(p:within? p) (pattern-bindings (p:within-body p) in-list?)]
     [(p:where? p) (pattern-bindings (p:where-body p) in-list?)]
     [(p:aperiodic? p) (append (pattern-bindings (p:aperiodic-open p) in-list?)
                               (pattern-bindings (p:aperiodic-body p) (or in-list? (p:aperiodic-cumulative? p)))
                               (if (p:aperiodic-cumulative? p) (pattern-bindings (p:aperiodic-close p) in-list?) '()))]
     [(p:periodic? p) (append (pattern-bindings (p:periodic-open p) in-list?)
                              (if (p:periodic-cumulative? p) (pattern-bindings (p:periodic-close p) in-list?) '()))]
     [else '()])))

;; Monotone patterns never invalidate an earlier match when events are added
;; (§2.10): no negation, absence, span absence, or aperiodic/periodic
;; closing. Only those may be evaluated immediately (EVAL.immediate).
(define (monotone? p)
  (cond
    [(or (p:neg-between? p) (p:absence? p) (p:unless? p) (p:aperiodic? p) (p:periodic? p)) #f]
    [(p:seq? p) (and (monotone? (p:seq-left p)) (monotone? (p:seq-right p)))]
    [(p:and? p) (and (monotone? (p:and-left p)) (monotone? (p:and-right p)))]
    [(p:or? p) (and (monotone? (p:or-left p)) (monotone? (p:or-right p)))]
    [(p:repeat? p) (monotone? (p:repeat-body p))]
    [(p:any? p) (andmap monotone? (p:any-parts p))]
    [(p:within? p) (monotone? (p:within-body p))]
    [(p:where? p) (monotone? (p:where-body p))]
    [else #t]))

;; Patterns with negation or Kleene closure and no time bound can hold state
;; forever (§17 "State bounds").
(define (unbounded? p)
  (define (has? pred q)
    (or (pred q)
        (for/or ([c (children q)]) (has? pred c))))
  (and (has? (λ (q) (or (p:repeat? q) (p:neg-between? q) (p:unless? q))) p)
       (not (has? (λ (q) (or (p:within? q) (p:absence? q))) p))))

(define (children p)
  (cond
    [(p:seq? p) (list (p:seq-left p) (p:seq-right p))]
    [(p:and? p) (list (p:and-left p) (p:and-right p))]
    [(p:or? p) (list (p:or-left p) (p:or-right p))]
    [(p:neg-between? p) (list (p:neg-between-left p) (p:neg-between-neg p) (p:neg-between-right p))]
    [(p:absence? p) (list (p:absence-body p) (p:absence-neg p))]
    [(p:unless? p) (list (p:unless-body p) (p:unless-neg p))]
    [(p:repeat? p) (list (p:repeat-body p))]
    [(p:any? p) (p:any-parts p)]
    [(p:within? p) (list (p:within-body p))]
    [(p:where? p) (list (p:where-body p))]
    [(p:aperiodic? p) (list (p:aperiodic-open p) (p:aperiodic-body p) (p:aperiodic-close p))]
    [(p:periodic? p) (list (p:periodic-open p) (p:periodic-close p))]
    [else '()]))

;; The primitive leaves, left to right.
(define (pattern-leaves p)
  (if (p:primitive? p) (list p) (append-map pattern-leaves (children p))))

;; Validation: problems that make a pattern ill-formed (docs/decisions/0010).
(define (pattern-problem p)
  (cond
    [(and (p:neg-between? p) (or (nullable? (p:neg-between-left p)) (nullable? (p:neg-between-right p))))
     "the neighbours of a negation must not be nullable"]
    [(and (p:repeat? p) (nullable? (p:repeat-body p))) "a repeated pattern must not be nullable"]
    [(and (p:repeat? p) (> (p:repeat-min p) (p:repeat-max p))) "a repetition's minimum exceeds its maximum"]
    [(and (p:any? p) (not (<= 1 (p:any-k p) (length (p:any-parts p)))))
     "ANY(k; …) needs 1 <= k <= the number of patterns"]
    [else (for/or ([c (children p)]) (pattern-problem c))]))

(provide
 (contract-out
  [struct pat ()]
  [struct (p:primitive pat) ([binding (or/c #f symbol?)] [type (or/c #f symbol?)] [pred (or/c #f procedure?)] [id symbol?])]
  [struct (p:act pat) ()]
  [struct (p:seq pat) ([left pat?] [right pat?])]
  [struct (p:and pat) ([left pat?] [right pat?])]
  [struct (p:or pat) ([left pat?] [right pat?])]
  [struct (p:neg-between pat) ([left pat?] [neg pat?] [right pat?])]
  [struct (p:absence pat) ([body pat?] [neg pat?] [window duration?] [clock clock?])]
  [struct (p:unless pat) ([body pat?] [neg pat?])]
  [struct (p:repeat pat) ([body pat?] [min exact-nonnegative-integer?] [max (or/c exact-positive-integer? +inf.0)])]
  [struct (p:any pat) ([k exact-positive-integer?] [parts (listof pat?)])]
  [struct (p:within pat) ([body pat?] [duration duration?])]
  [struct (p:where pat) ([body pat?] [pred procedure?])]
  [struct (p:aperiodic pat) ([open pat?] [body pat?] [close pat?] [cumulative? boolean?])]
  [struct (p:periodic pat) ([open pat?] [period (and/c real? positive?)] [close pat?] [cumulative? boolean?])]
  [prim (->* ((or/c #f symbol?)) (#:bind (or/c #f symbol?) #:when (or/c #f procedure?) #:id (or/c #f symbol?)) p:primitive?)]
  [act p:act?]
  [seq (->* (pat?) () #:rest (listof pat?) pat?)]
  [conj (->* (pat?) () #:rest (listof pat?) pat?)]
  [disj (->* (pat?) () #:rest (listof pat?) pat?)]
  [neg-between (-> pat? pat? pat? pat?)]
  [absence (->* (pat? pat? duration?) (#:clock clock?) pat?)]
  [span-absence (-> pat? pat? pat?)]
  [kleene+ (-> pat? pat?)]
  [kleene* (-> pat? pat?)]
  [repeat (->* (pat? exact-nonnegative-integer?) ((or/c exact-positive-integer? +inf.0)) pat?)]
  [any-of (->* (exact-positive-integer? pat?) () #:rest (listof pat?) pat?)]
  [within (-> pat? duration? pat?)]
  [where (-> pat? procedure? pat?)]
  [aperiodic (-> pat? pat? pat? pat?)]
  [aperiodic* (-> pat? pat? pat? pat?)]
  [periodic (-> pat? (and/c real? positive?) pat? pat?)]
  [periodic* (-> pat? (and/c real? positive?) pat? pat?)]
  [nullable? (-> pat? boolean?)]
  [pattern-bindings (->* (pat?) (boolean?) (listof (cons/c symbol? (or/c 'single 'list))))]
  [monotone? (-> pat? boolean?)]
  [unbounded? (-> pat? boolean?)]
  [children (-> pat? (listof pat?))]
  [pattern-leaves (-> pat? (listof p:primitive?))]
  [pattern-problem (-> pat? (or/c #f string?))]))
