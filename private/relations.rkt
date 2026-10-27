#lang racket/base
;; Relations on the event space (spec §2.8): precedence ≺, membership ∈ and
;; its transitive closure ∈*, and causality →. Together with the observed
;; events they form Luckham's event cloud, a partially ordered set.
(require racket/contract
         racket/list
         racket/set
         "event.rkt")

;; ES.precedes: a ≺ b iff te(a) < ts(b). A strict partial order.
(define (precedes? a b) (< (event-te a) (event-ts b)))
;; a ∥ b: neither precedes the other (concurrent or overlapping).
(define (concurrent? a b) (not (or (precedes? a b) (precedes? b a))))

;; ES.membership: a ∈ C(c), compared by id.
(define (constituent-of? a c)
  (for/or ([x (in-list (event-constituents c))]) (equal? (event-id x) (event-id a))))

;; ∈*: the transitive closure, the event hierarchy below c.
(define (provenance c)
  (let loop ([todo (event-constituents c)] [seen (set)] [acc '()])
    (cond [(null? todo) (reverse acc)]
          [(set-member? seen (event-id (car todo))) (loop (cdr todo) seen acc)]
          [else (define x (car todo))
                (loop (append (event-constituents x) (cdr todo))
                      (set-add seen (event-id x))
                      (cons x acc))])))

(define (member*? a c)
  (for/or ([x (in-list (provenance c))]) (equal? (event-id x) (event-id a))))

;; ES.causality: a → b, recorded (b.cause = a.id) or inferred by a rule.
;; A causality is a set of (cause-id . effect-id) edges with its transitive
;; closure precomputed.
(struct causality (edges closure) #:transparent)

(define (transitive-closure edges)
  (define succ (for/fold ([h (hash)]) ([e (in-list edges)])
                 (hash-update h (car e) (λ (s) (set-add s (cdr e))) (set))))
  (for/hash ([a (in-hash-keys succ)])
    (values a
            (let loop ([todo (set->list (hash-ref succ a))] [seen (set)])
              (cond [(null? todo) seen]
                    [(set-member? seen (car todo)) (loop (cdr todo) seen)]
                    [else (loop (append (set->list (hash-ref succ (car todo) (set))) (cdr todo))
                                (set-add seen (car todo)))])))))

(define (make-causality edges)
  (causality (remove-duplicates edges) (transitive-closure (remove-duplicates edges))))

;; Recorded causes come from a `cause` attribute holding the cause's id (or a
;; list of ids); inferred causes come from an optional rule (a b -> boolean).
(define (causality-from-events events #:infer [infer #f])
  (define recorded
    (for*/list ([b (in-list events)]
                [c (in-list (let ([v (event-attr/default b 'cause #f)])
                              (cond [(not v) '()] [(list? v) v] [else (list v)])))])
      (cons c (event-id b))))
  (define inferred
    (if infer
        (for*/list ([a (in-list events)] [b (in-list events)]
                    #:unless (eq? a b) #:when (infer a b))
          (cons (event-id a) (event-id b)))
        '()))
  (make-causality (append recorded inferred)))

(define (causes? g a b)
  (set-member? (hash-ref (causality-closure g) (event-id a) (set)) (event-id b)))

;; A causality must be a strict partial order (no a →⁺ a) and respect time
;; (a → b implies ts(a) <= ts(b)). Returns the list of violations.
(define (causality-problems g events)
  (define by-id (for/hash ([e (in-list events)]) (values (event-id e) e)))
  (append
   (for/list ([(a succs) (in-hash (causality-closure g))] #:when (set-member? succs a))
     (format "cycle: ~a causes itself" a))
   (for/list ([e (in-list (causality-edges g))]
              #:when (and (hash-has-key? by-id (car e)) (hash-has-key? by-id (cdr e))
                          (> (event-ts (hash-ref by-id (car e))) (event-ts (hash-ref by-id (cdr e))))))
     (format "~a → ~a but ts(~a) > ts(~a)" (car e) (cdr e) (car e) (cdr e)))))

;; The event cloud (observed events, ≺, →, ∈), with relations as id pairs.
(struct event-cloud (events precedence causality membership) #:transparent)

(define (make-event-cloud events #:causality [g (causality-from-events events)])
  (event-cloud events
               (for*/list ([a (in-list events)] [b (in-list events)] #:when (precedes? a b))
                 (cons (event-id a) (event-id b)))
               g
               (for*/list ([c (in-list events)] [a (in-list (event-constituents c))])
                 (cons (event-id a) (event-id c)))))

(provide
 (contract-out
  [precedes? (-> event? event? boolean?)]
  [concurrent? (-> event? event? boolean?)]
  [constituent-of? (-> event? event? boolean?)]
  [provenance (-> event? (listof event?))]
  [member*? (-> event? event? boolean?)]
  [struct causality ([edges (listof pair?)] [closure hash?])]
  [make-causality (-> (listof pair?) causality?)]
  [causality-from-events (->* ((listof event?)) (#:infer (or/c #f (-> event? event? any/c)))
                              causality?)]
  [causes? (-> causality? event? event? boolean?)]
  [causality-problems (-> causality? (listof event?) (listof string?))]
  [struct event-cloud ([events (listof event?)] [precedence (listof pair?)]
                       [causality causality?] [membership (listof pair?)])]
  [make-event-cloud (->* ((listof event?)) (#:causality causality?) event-cloud?)]))
