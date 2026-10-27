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
         "reference.rkt")

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
   #:init-key (λ (g k c) (cons (ctx-now c) '()))          ; (activation instant . history, newest first)
   #:on-event (λ (g st e c) (cons (car st) (cons e (cdr st))))
   #:event-deadline (λ (st) +inf.0)
   #:on-timer
   (λ (g st c)
     (cond
       [(eqv? (ctx-watermark c) +inf.0)
        (for ([m (in-list (denote p (reverse (cdr st)) #:activation (car st) #:horizon horizon))])
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
;; The pattern operator used by #lang occur. The reference engine implements
;; the most permissive semantics (SEL.any, CON.unrestricted, OUT.every,
;; SKIP.no-skip, EVAL.deferred); other policies need the incremental engine.

(define default-policies
  (hasheq 'selection 'any 'consumption 'unrestricted 'output 'every 'skip 'no-skip 'eval 'deferred))

(define (pattern-operator p #:key [key #f] #:derive [derive (λ (m) (derive-match m))]
                          #:policies [policies (hasheq)] #:engine [engine 'reference])
  (define ps (for/fold ([h default-policies]) ([(k v) (in-hash policies)]) (hash-set h k v)))
  (case engine
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
                         operator?)]))
