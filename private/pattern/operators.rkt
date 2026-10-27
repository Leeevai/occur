#lang racket/base
;; Pattern operators: running a pattern over a keyed stream and deriving
;; events from its matches.
;;
;; The reference operator evaluates the oracle (pattern/reference.rkt) per
;; key once the stream is complete (the final watermark): it is the
;; executable specification, used to check the incremental engine and as
;; #:engine reference in #lang occur.
(require racket/contract
         racket/list
         "../time.rkt"
         "../event.rkt"
         "../operator.rkt"
         "ast.rkt"
         "reference.rkt"
         "nfa.rkt"
         "policies.rkt"
         "../scheduler.rkt")

;; DR.derive: from a match m, the event ⟨fresh id, τ′, f(bindings(m)),
;; ts(m), te(m), 1, op, m⟩. A match without events (ACT-based) carries a
;; summary instead of constituents.
(define (derive-match m #:type [type 'Match] #:payload [payload-fn default-match-payload] #:src [src 'pattern])
  (define payload (payload-fn (pmatch-bindings m)))
  (make-event type
              (if (null? (pmatch-events m)) (hash-set payload 'summary (hasheq 'events 0)) payload)
              #:ts (if (eqv? (pmatch-ts m) +inf.0) (pmatch-te m) (pmatch-ts m))
              #:te (pmatch-te m)
              #:n 1 #:src src #:constituents (pmatch-events m)))

;; By default the payload names the events bound to each binding.
(define (default-match-payload bindings)
  (for/hasheq ([(k v) (in-hash bindings)])
    (values k (cond [(event? v) (event-id v)]
                    [(list? v) (map (λ (x) (if (event? x) (event-id x) x)) v)]
                    [else v]))))

(define (reference-pattern-operator p #:key [key #f] #:derive [derive (λ (m) (derive-match m))]
                                    #:horizon [horizon #f]
                                    #:selection [selection 'any] #:output [output 'every])
  (define problem (pattern-problem p))
  (when problem (raise-arguments-error 'reference-pattern-operator problem))
  (make-operator
   'pattern
   #:monotone? #f
   #:key key
   #:lifecycle? #t
   #:init-global (λ () -inf.0)                            ; the newest finite watermark
   ;; (activation instant, history newest first, id -> arrival index over all keys)
   #:init-key (λ (g k c) (list (ctx-now c) '() (hash)))
   #:on-event (λ (g st e c) (list (car st) (cons e (cadr st)) (hash-set (caddr st) (event-id e) (ctx-seq c))))
   #:on-watermark (λ (g keys W c) (values (if (< W +inf.0) (time-max g W) g) keys))
   #:event-deadline (λ (st) +inf.0)
   #:on-timer
   (λ (g st c)
     (cond
       [(eqv? (ctx-watermark c) +inf.0)
        (define history (reverse (cadr st)))
        ;; the evaluation horizon: the newest finite watermark or end time
        (define h (or horizon (time-max g (for/fold ([m -inf.0]) ([e history]) (time-max m (event-te e))))))
        (define ms (select-matches (denote p history #:activation (car st) #:horizon h) selection history
                                   #:global-index (caddr st)))
        (for ([m (in-list (if (eq? output 'maximal) (maximal-matches ms) ms))])
          (emit! c (derive m)))
        #f]
       [else st]))))

(provide
 (contract-out
  [derive-match (->* (pmatch?) (#:type symbol? #:payload (-> hash? hash?) #:src any/c) event?)]
  [default-match-payload (-> hash? hash?)]
  [reference-pattern-operator (->* (pat?) (#:key (or/c #f (-> event? any/c)) #:derive (-> pmatch? event?)
                                          #:horizon (or/c #f time?)
                                          #:selection (or/c 'strict 'partition 'next 'any)
                                          #:output (or/c 'every 'maximal))
                                   operator?)]))

;; ---------------------------------------------------------------------------
;; The incremental operator: one engine per key, created at the key's
;; activation (its first event, or an entity activation signal), fed each
;; event with its arrival sequence number, and advanced on its own deadlines:
;; event-time deadlines for deferred decisions, processing-time deadlines for
;; hybrid absence. Candidates pass through the policy layer of §12
;; (policies.rkt). With timed-out partials, the engine runs on the pattern's
;; body and the top-level where clause is applied here, so matches it has not
;; yet accepted are the partial matches.

(struct ist (engine policy latest arrivals) #:transparent)

(define (split-where p)
  (cond [(p:within? p) (let-values ([(b θ) (split-where (p:within-body p))])
                         (values (and θ (within b (p:within-duration p))) θ))]
        [(p:where? p) (values (p:where-body p) (p:where-pred p))]
        [else (values #f #f)]))

(define (top-within p) (and (p:within? p) (p:within-duration p)))

(define (incremental-pattern-operator p #:key [key #f] #:derive [derive (λ (m) (derive-match m))]
                                      #:selection [selection 'any]
                                      #:policy [pol (make-match-policy)])
  (define problem (pattern-problem p))
  (when problem (raise-arguments-error 'incremental-pattern-operator problem))
  (define pol* (struct-copy match-policy pol [within (or (match-policy-within pol) (top-within p))]))
  (define-values (body θ) (if (match-policy-timeout pol*) (split-where p) (values #f #f)))
  (when (and (match-policy-timeout pol*) (not θ))
    (raise-arguments-error 'incremental-pattern-operator "timed-out partial matches need a pattern with a where clause"))
  (define run-pattern (or body p))
  (define initials (initial-leaves p))
  (define (info st) (hasheq 'index (engine-index (ist-engine st)) 'latest (ist-latest st) 'arrival (ist-arrivals st)))
  (define (deliver st ms c)
    (define emit (λ (m) (emit! c (derive m))))
    (define W (ctx-watermark c))
    (define now (ctx-now c))
    (cond
      [θ (define-values (done partial) (partition (λ (m) (θ (pmatch-bindings m))) ms))
         (define ps1 (policy-step pol* (ist-policy st) done (info st) W now emit))
         (struct-copy ist st [policy (policy-partials pol* ps1 partial (info st) W now emit)])]
      [else (struct-copy ist st [policy (policy-step pol* (ist-policy st) ms (info st) W now emit)])]))
  (define (advance g st c)
    (define-values (e2 ms) (engine-advance (ist-engine st) (ctx-watermark c) (ctx-now c) #:last-finite g))
    (define st2 (deliver (struct-copy ist st [engine e2]) ms c))
    (struct-copy ist st2 [policy (policy-flush pol* (ist-policy st2) (info st2) (ctx-watermark c) (ctx-now c)
                                               (λ (m) (emit! c (derive m))))]))
  (make-operator
   'pattern
   #:monotone? (monotone? p)
   #:key key
   #:lifecycle? #t
   #:init-global (λ () -inf.0)                            ; the newest finite watermark
   #:on-watermark (λ (g keys W c) (values (if (< W +inf.0) (time-max g W) g) keys))
   #:init-key (λ (g k c)
                (define-values (e ms) (engine-start (make-engine run-pattern #:activation (ctx-now c) #:selection selection)))
                (deliver (ist e (policy-init pol*) -1 (hash)) ms c))
   #:on-event (λ (g st e c)
                (define seq (ctx-seq c))
                (define st1 (struct-copy ist st
                                         [latest (if (for/or ([l initials]) (qualifies? l e)) seq (ist-latest st))]
                                         [arrivals (hash-set (ist-arrivals st) seq (ctx-now c))]))
                (define-values (e2 ms) (engine-event (ist-engine st1) e seq))
                (advance g (deliver (struct-copy ist st1 [engine e2]) ms c) c))
   #:on-signal (λ (g st sig c) st)
   #:event-deadline (λ (st)
                      (define-values (evt proc) (engine-deadlines (ist-engine st)))
                      (earliest evt (policy-deadline pol* (ist-policy st) 'event-time)))
   #:deadline (λ (st)
                (define-values (evt proc) (engine-deadlines (ist-engine st)))
                (earliest proc (policy-deadline pol* (ist-policy st) 'processing-time)))
   #:on-timer (λ (g st c) (advance g st c))))

(define (earliest a b)
  (cond [(and a b) (if (< (deadline-time a) (deadline-time b)) a b)] [else (or a b)]))

;; ---------------------------------------------------------------------------
;; The pattern operator used by #lang occur. The reference engine evaluates
;; the oracle once the stream is complete, so it implements every selection
;; strategy with every or maximal output, under unrestricted consumption and
;; no skipping; other policies need the incremental engine.

(define default-policies
  (hasheq 'selection 'any 'consumption 'unrestricted 'output 'every 'skip 'no-skip 'eval #f))

(define (pattern-operator p #:key [key #f] #:derive [derive (λ (m) (derive-match m))]
                          #:policies [policies (hasheq)] #:engine [engine 'incremental])
  (define ps (for/fold ([h default-policies]) ([(k v) (in-hash policies)]) (hash-set h k v)))
  (case engine
    [(incremental)
     (incremental-pattern-operator
      p #:key key #:derive derive #:selection (hash-ref ps 'selection)
      #:policy (make-match-policy #:consumption (hash-ref ps 'consumption) #:output (hash-ref ps 'output)
                                  #:skip (hash-ref ps 'skip) #:eval (hash-ref ps 'eval #f)
                                  #:timeout-partial (hash-ref ps 'timeout-partial #f)
                                  #:retain (hash-ref ps 'retain '())))]
    [(reference)
     (for ([(k v) (in-hash ps)])
       (unless (or (equal? v (hash-ref default-policies k #f))
                   (and (eq? k 'selection) (memq v selection-strategies))
                   (and (eq? k 'output) (eq? v 'maximal))
                   (and (eq? k 'eval) (eq? v 'deferred)))
         (raise-arguments-error 'pattern-operator
                                (string-append "the reference engine implements the selection strategies with every or maximal"
                                               " output, under unrestricted consumption and no skipping; use the incremental engine")
                                "policy" k "value" v)))
     (reference-pattern-operator p #:key key #:derive derive
                                 #:selection (hash-ref ps 'selection) #:output (hash-ref ps 'output))]
    [else (raise-arguments-error 'pattern-operator "unknown engine" "engine" engine)]))

(provide
 (contract-out
  [default-policies hash?]
  [pattern-operator (->* (pat?) (#:key (or/c #f (-> event? any/c)) #:derive (-> pmatch? event?)
                                 #:policies hash? #:engine symbol?)
                         operator?)]
  [incremental-pattern-operator (->* (pat?) (#:key (or/c #f (-> event? any/c)) #:derive (-> pmatch? event?)
                                             #:selection symbol? #:policy match-policy?)
                                     operator?)]))
