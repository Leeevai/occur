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
         "nfa.rkt")

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
                                    #:horizon [horizon #f])
  (define problem (pattern-problem p))
  (when problem (raise-arguments-error 'reference-pattern-operator problem))
  (make-operator
   'pattern
   #:monotone? #f
   #:key key
   #:lifecycle? #t
   #:init-global (λ () -inf.0)                            ; the newest finite watermark
   #:init-key (λ (g k c) (cons (ctx-now c) '()))          ; (activation instant . history, newest first)
   #:on-event (λ (g st e c) (cons (car st) (cons e (cdr st))))
   #:on-watermark (λ (g keys W c) (values (if (< W +inf.0) (time-max g W) g) keys))
   #:event-deadline (λ (st) +inf.0)
   #:on-timer
   (λ (g st c)
     (cond
       [(eqv? (ctx-watermark c) +inf.0)
        (define history (reverse (cdr st)))
        ;; the evaluation horizon: the newest finite watermark or end time
        (define h (or horizon (time-max g (for/fold ([m -inf.0]) ([e history]) (time-max m (event-te e))))))
        (for ([m (in-list (denote p history #:activation (car st) #:horizon h))])
          (emit! c (derive m)))
        #f]
       [else st]))))

(provide
 (contract-out
  [derive-match (->* (pmatch?) (#:type symbol? #:payload (-> hash? hash?) #:src any/c) event?)]
  [default-match-payload (-> hash? hash?)]
  [reference-pattern-operator (->* (pat?) (#:key (or/c #f (-> event? any/c)) #:derive (-> pmatch? event?)
                                          #:horizon (or/c #f time?))
                                   operator?)]))

;; ---------------------------------------------------------------------------
;; The incremental operator: one engine per key, created at the key's
;; activation (its first event, or an entity activation signal), fed each
;; event with its arrival sequence number, and advanced on its own deadlines:
;; event-time deadlines for deferred decisions, processing-time deadlines for
;; hybrid absence. The candidates pass through `emit-matches`, which applies
;; the output policies (policies.rkt).

(define current-match-policy (make-parameter #f))

(define (incremental-pattern-operator p #:key [key #f] #:derive [derive (λ (m) (derive-match m))]
                                      #:selection [selection 'any]
                                      #:policy [policy #f])
  (define problem (pattern-problem p))
  (when problem (raise-arguments-error 'incremental-pattern-operator problem))
  ;; policy: #f (report every match) or (vector init step flush deadline)
  (define (deliver! c pstate ms)
    (cond
      [(not policy) (for ([m (in-list ms)]) (emit! c (derive m))) pstate]
      [else ((vector-ref policy 1) pstate ms c (λ (m) (emit! c (derive m))))]))
  (define (advance g st c)
    (define-values (e2 ms) (engine-advance (car st) (ctx-watermark c) (ctx-now c) #:last-finite g))
    (define ps (deliver! c (cdr st) ms))
    (cons e2 (if policy ((vector-ref policy 2) ps c (λ (m) (emit! c (derive m)))) ps)))
  (make-operator
   'pattern
   #:monotone? (monotone? p)
   #:key key
   #:lifecycle? #t
   #:init-global (λ () -inf.0)                            ; the newest finite watermark
   #:on-watermark (λ (g keys W c) (values (if (< W +inf.0) (time-max g W) g) keys))
   #:init-key (λ (g k c)
                (define-values (e ms) (engine-start (make-engine p #:activation (ctx-now c) #:selection selection)))
                (define ps0 (if policy ((vector-ref policy 0)) #f))
                (cons e (deliver! c ps0 ms)))
   #:on-event (λ (g st e c)
                (define-values (e2 ms) (engine-event (car st) e (ctx-seq c)))
                (advance g (cons e2 (deliver! c (cdr st) ms)) c))
   #:on-signal (λ (g st sig c) st)
   #:event-deadline (λ (st)
                      (define-values (evt proc) (engine-deadlines (car st)))
                      (define pd (and policy ((vector-ref policy 3) (cdr st) 'event-time)))
                      (cond [(and evt pd) (if (< (deadline-time* evt) (deadline-time* pd)) evt pd)] [else (or evt pd)]))
   #:deadline (λ (st)
                (define-values (evt proc) (engine-deadlines (car st)))
                (define pd (and policy ((vector-ref policy 3) (cdr st) 'processing-time)))
                (cond [(and proc pd) (min proc pd)] [else (or proc pd)]))
   #:on-timer (λ (g st c) (advance g st c))))

(define (deadline-time* d) (if (real? d) d (strictly-after-time* d)))
(define (strictly-after-time* d) (vector-ref (struct->vector d) 1))

;; ---------------------------------------------------------------------------
;; The pattern operator used by #lang occur. The reference engine implements
;; the most permissive semantics (SEL.any, CON.unrestricted, OUT.every,
;; SKIP.no-skip, EVAL.deferred); other policies need the incremental engine.

(define default-policies
  (hasheq 'selection 'any 'consumption 'unrestricted 'output 'every 'skip 'no-skip 'eval 'deferred))

(define (pattern-operator p #:key [key #f] #:derive [derive (λ (m) (derive-match m))]
                          #:policies [policies (hasheq)] #:engine [engine 'incremental])
  (define ps (for/fold ([h default-policies]) ([(k v) (in-hash policies)]) (hash-set h k v)))
  (case engine
    [(incremental)
     (for ([(k v) (in-hash ps)] #:unless (memq k '(selection)))
       (unless (equal? v (hash-ref default-policies k))
         (raise-arguments-error 'pattern-operator "this policy needs the policy layer (§12)" "policy" k "value" v)))
     (incremental-pattern-operator p #:key key #:derive derive #:selection (hash-ref ps 'selection))]
    [(reference)
     (for ([(k v) (in-hash ps)])
       (unless (equal? v (hash-ref default-policies k))
         (raise-arguments-error 'pattern-operator
                                "the reference engine implements only the most permissive semantics; use the incremental engine"
                                "policy" k "value" v)))
     (reference-pattern-operator p #:key key #:derive derive)]
    [else (raise-arguments-error 'pattern-operator "unknown engine" "engine" engine)]))

(provide
 (contract-out
  [default-policies hash?]
  [pattern-operator (->* (pat?) (#:key (or/c #f (-> event? any/c)) #:derive (-> pmatch? event?)
                                 #:policies hash? #:engine symbol?)
                         operator?)]
  [incremental-pattern-operator (->* (pat?) (#:key (or/c #f (-> event? any/c)) #:derive (-> pmatch? event?)
                                             #:selection symbol? #:policy (or/c #f vector?))
                                     operator?)]))
