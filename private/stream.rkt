#lang racket/base
;; Streams, histories, keys, and sub-streams (spec §2.9).
(require racket/contract
         racket/list
         "time.rkt"
         "event.rkt")

;; ===========================================================================
;; ES.stream: a sequence of events in arrival order with declared types T_S.
;; Finite here; the runner feeds unbounded streams step by step.

(struct event-stream (types events) #:transparent)

(define (make-event-stream types events)
  (for ([e (in-list events)])
    (unless (memq (event-type e) types)
      (raise-arguments-error 'make-event-stream "event type is not declared by the stream"
                             "type" (event-type e) "declared" types)))
  (event-stream types events))

;; An event is out of order when it arrives after an event with a later end
;; time: arrival order disagrees with event-time order.
(define (out-of-order-events s)
  (let loop ([es (event-stream-events s)] [max-te -inf-time] [acc '()])
    (cond [(null? es) (reverse acc)]
          [else (define e (car es))
                (loop (cdr es) (time-max max-te (event-te e))
                      (if (< (event-te e) max-te) (cons e acc) acc))])))

(define (stream-in-order? s) (null? (out-of-order-events s)))

;; ===========================================================================
;; ES.history: H_S(t) = { e ∈ S : te(e) <= t }, in arrival order.

(define (history s t)
  (filter (λ (e) (<= (event-te e) t)) (event-stream-events s)))

;; ===========================================================================
;; ES.keyed: a key function κ partitions S into substreams S_k.

(define (partition-stream s κ)
  ;; Returns an association list (k . S_k) in order of first appearance.
  (define-values (order groups)
    (for/fold ([order '()] [groups (hash)]) ([e (in-list (event-stream-events s))])
      (define k (κ e))
      (values (if (hash-has-key? groups k) order (cons k order))
              (hash-update groups k (λ (es) (cons e es)) '()))))
  (for/list ([k (in-list (reverse order))])
    (cons k (event-stream (event-stream-types s) (reverse (hash-ref groups k))))))

(define (substream s κ k)
  (event-stream (event-stream-types s)
                (filter (λ (e) (equal? (κ e) k)) (event-stream-events s))))

(define (attribute-key attr) (λ (e) (event-attr e attr)))

;; ===========================================================================
;; ES.substream: hierarchical keys. An entity k owns sub-streams s, and each
;; sub-stream has a kind m(s). A keying names the three projections.

(struct keying (entity substream kind) #:transparent)

(define (attribute-keying #:entity entity #:substream [sub #f] #:kind [kind #f])
  (keying (attribute-key entity)
          (if sub (attribute-key sub) (λ (e) #f))
          (if kind (attribute-key kind) (λ (e) #f))))

(define (entity-of ky e) ((keying-entity ky) e))
(define (substream-of ky e) ((keying-substream ky) e))
(define (kind-of ky e) ((keying-kind ky) e))

;; Keep only events whose sub-stream kind is in M (#f watches every kind).
(define (watching ky kinds events)
  (if kinds
      (filter (λ (e) (member (kind-of ky e) kinds)) events)
      events))

;; The sub-streams of one entity: an association list (s . events).
(define (entity-substreams s ky k)
  (partition-stream (substream s (keying-entity ky) k) (keying-substream ky)))

(define (entity-kinds s ky k)
  (remove-duplicates
   (map (λ (e) (kind-of ky e)) (event-stream-events (substream s (keying-entity ky) k)))))

(provide
 (contract-out
  [struct event-stream ([types (listof symbol?)] [events (listof event?)])]
  [make-event-stream (-> (listof symbol?) (listof event?) event-stream?)]
  [out-of-order-events (-> event-stream? (listof event?))]
  [stream-in-order? (-> event-stream? boolean?)]
  [history (-> event-stream? time? (listof event?))]
  [partition-stream (-> event-stream? (-> event? any/c) (listof (cons/c any/c event-stream?)))]
  [substream (-> event-stream? (-> event? any/c) any/c event-stream?)]
  [attribute-key (-> symbol? (-> event? any/c))]
  [struct keying ([entity (-> event? any/c)] [substream (-> event? any/c)] [kind (-> event? any/c)])]
  [attribute-keying (->* (#:entity symbol?) (#:substream (or/c #f symbol?) #:kind (or/c #f symbol?))
                         keying?)]
  [entity-of (-> keying? event? any/c)]
  [substream-of (-> keying? event? any/c)]
  [kind-of (-> keying? event? any/c)]
  [watching (-> keying? (or/c #f list?) (listof event?) (listof event?))]
  [entity-substreams (-> event-stream? keying? any/c (listof (cons/c any/c event-stream?)))]
  [entity-kinds (-> event-stream? keying? any/c list?)]))
