#lang racket/base
;; Joins and correlation (spec §9).
;;
;; A join reads a stream s on port 0 and a stream (or table) r on port 1,
;; partitioned by a key that both sides share (an equi-join), and emits a
;; derived event with C = {s, r}, or C = {s} for an unmatched outer result.
;; θ(s, r) refines the match. The payload is (combine s r): by default the
;; union of both payloads, s winning on conflicts; an unmatched side
;; contributes its declared attributes as #f (null).
(require racket/contract
         racket/list
         "time.rkt"
         "event.rkt"
         "operator.rkt"
         "window.rkt")

(define (default-combine s r)
  (for/fold ([p (event-payload r)]) ([(k v) (in-hash (event-payload s))]) (hash-set p k v)))

(define (nulls attrs) (for/hasheq ([a (in-list attrs)]) (values a #f)))

(define (joined type combine s r)
  (make-composite type (combine s r) (list s r) #:src 'join))

;; An unmatched event of one side, with the other side's attributes null. A
;; custom combine (a join's #:select) sees the other side as an event whose
;; attributes are all null.
(define (unmatched type combine e other-attrs side)
  (define payload
    (cond
      [(eq? combine default-combine)
       (for/fold ([p (nulls other-attrs)]) ([(k v) (in-hash (event-payload e))]) (hash-set p k v))]
      [else
       (define none (make-event 'Null (nulls other-attrs) #:ts (event-ts e) #:te (event-te e)))
       (if (eq? side 'left) (combine e none) (combine none e))]))
  (make-composite type payload (list e) #:src (if (eq? side 'left) 'left-outer-join 'right-outer-join)))

(define outer/c (or/c #f 'left 'right 'full))
(define (keeps-left? outer) (memq outer '(left full)))
(define (keeps-right? outer) (memq outer '(right full)))

;; ===========================================================================
;; JN.window: s ⋈ r if θ(s, r) and both are in the same window. Each pair is
;; emitted once, as soon as its second event arrives (inner joins are
;; monotone). Outer results are emitted when the window closes: an event
;; unmatched in every window it belongs to.

(struct wj (windows pairs matched) #:transparent)
;; windows: hash start -> (cons lefts rights); pairs: set of (s-id . r-id) emitted;
;; matched: hash id -> #t for events that matched at least once

(define (window-join-operator g θ #:key key #:type [type 'Joined] #:combine [combine default-combine]
                              #:outer [outer #f] #:left-attributes [lattrs '()] #:right-attributes [rattrs '()])
  (define size (grid-size g))
  (make-operator
   'window-join
   #:arity 2
   #:monotone? (not outer)
   #:key key
   #:init-key (λ (gl k c) (wj (hash) (hash) (hash)))
   #:on-event
   (λ (gl st e c)
     (define left? (= (ctx-port c) 0))
     (for/fold ([st st]) ([s0 (in-list (grid-starts-at g (event-ts e)))])
       (cond
         [(<= (+ s0 size) (ctx-watermark c)) (count! c 'late) st]
         [else
          (define w (hash-ref (wj-windows st) s0 (cons '() '())))
          (define others (if left? (cdr w) (car w)))
          (define-values (pairs matched)
            (for/fold ([pairs (wj-pairs st)] [matched (wj-matched st)]) ([o (in-list (reverse others))])
              (define s (if left? e o))
              (define r (if left? o e))
              (define id (cons (event-id s) (event-id r)))
              (cond [(and (θ s r) (not (hash-ref pairs id #f)))
                     (emit! c (joined type combine s r))
                     (values (hash-set pairs id #t) (hash-set (hash-set matched (event-id s) #t) (event-id r) #t))]
                    [else (values pairs matched)])))
          (wj (hash-set (wj-windows st) s0 (if left? (cons (cons e (car w)) (cdr w)) (cons (car w) (cons e (cdr w)))))
              pairs matched)])))
   #:event-deadline (λ (st) (and (positive? (hash-count (wj-windows st)))
                                 (+ (apply min (hash-keys (wj-windows st))) size)))
   #:on-timer
   (λ (gl st c)
     (define W (ctx-watermark c))
     (define-values (closing open) (partition (λ (s0) (<= (+ s0 size) W)) (sort (hash-keys (wj-windows st)) <)))
     (define remaining (for/hash ([s0 open]) (values s0 (hash-ref (wj-windows st) s0))))
     ;; an event is unmatched once none of its windows remains open
     (define (still-open? e) (for/or ([s0 (grid-starts-at g (event-ts e))]) (hash-has-key? remaining s0)))
     (define seen (make-hash))
     (for* ([s0 closing] [side '(left right)]
            [e (in-list (reverse ((if (eq? side 'left) car cdr) (hash-ref (wj-windows st) s0))))])
       (when (and (if (eq? side 'left) (keeps-left? outer) (keeps-right? outer))
                  (not (hash-ref (wj-matched st) (event-id e) #f))
                  (not (still-open? e))
                  (not (hash-ref seen (event-id e) #f)))
         (hash-set! seen (event-id e) #t)
         (emit! c (unmatched type combine e (if (eq? side 'left) rattrs lattrs) side))))
     (and (positive? (hash-count remaining)) (struct-copy wj st [windows remaining])))))

;; ===========================================================================
;; JN.interval: s ⋈ r if θ(s, r) and t(r) ∈ [t(s) − Δ₁, t(s) + Δ₂]. Pairs
;; are emitted as soon as both have arrived. An s can be forgotten once the
;; watermark passes t(s) + Δ₂, an r once it passes t(r) + Δ₁; an outer
;; join emits the unmatched ones then.

(struct ij (lefts rights matched) #:transparent)

(define (interval-join-operator lower upper θ #:key key #:type [type 'Joined] #:combine [combine default-combine]
                                #:outer [outer #f] #:left-attributes [lattrs '()] #:right-attributes [rattrs '()])
  (define (in-range? s r) (<= (- (event-ts s) lower) (event-ts r) (+ (event-ts s) upper)))
  (make-operator
   'interval-join
   #:arity 2
   #:monotone? (not outer)
   #:key key
   #:init-key (λ (gl k c) (ij '() '() (hash)))
   #:on-event
   (λ (gl st e c)
     (define left? (= (ctx-port c) 0))
     (define others (if left? (ij-rights st) (ij-lefts st)))
     (define matched
       (for/fold ([m (ij-matched st)]) ([o (in-list (reverse others))])
         (define s (if left? e o))
         (define r (if left? o e))
         (cond [(and (in-range? s r) (θ s r))
                (emit! c (joined type combine s r))
                (hash-set (hash-set m (event-id s) #t) (event-id r) #t)]
               [else m])))
     (if left?
         (ij (cons e (ij-lefts st)) (ij-rights st) matched)
         (ij (ij-lefts st) (cons e (ij-rights st)) matched)))
   #:event-deadline
   (λ (st) (define ds (append (map (λ (s) (+ (event-ts s) upper)) (ij-lefts st))
                              (map (λ (r) (+ (event-ts r) lower)) (ij-rights st))))
     (and (pair? ds) (apply min ds)))
   #:on-timer
   (λ (gl st c)
     (define W (ctx-watermark c))
     (define-values (old-l keep-l) (partition (λ (s) (< (+ (event-ts s) upper) W)) (ij-lefts st)))
     (define-values (old-r keep-r) (partition (λ (r) (< (+ (event-ts r) lower) W)) (ij-rights st)))
     (when (keeps-left? outer)
       (for ([s (reverse old-l)] #:unless (hash-ref (ij-matched st) (event-id s) #f))
         (emit! c (unmatched type combine s rattrs 'left))))
     (when (keeps-right? outer)
       (for ([r (reverse old-r)] #:unless (hash-ref (ij-matched st) (event-id r) #f))
         (emit! c (unmatched type combine r lattrs 'right))))
     (if (and (null? keep-l) (null? keep-r)) #f (ij keep-l keep-r (ij-matched st))))))

(provide
 outer/c
 (contract-out
  [default-combine (-> event? event? hash?)]
  [window-join-operator (->* (grid? (-> event? event? any/c) #:key (-> event? any/c))
                             (#:type symbol? #:combine (-> event? event? hash?) #:outer outer/c
                              #:left-attributes (listof symbol?) #:right-attributes (listof symbol?))
                             operator?)]
  [interval-join-operator (->* (duration? duration? (-> event? event? any/c) #:key (-> event? any/c))
                               (#:type symbol? #:combine (-> event? event? hash?) #:outer outer/c
                                #:left-attributes (listof symbol?) #:right-attributes (listof symbol?))
                               operator?)]))

;; ===========================================================================
;; JN.table: a stream–table (lookup) join. Port 1 carries the table: each
;; event upserts the row of its key (a retraction deletes it). Each s on
;; port 0 is matched against the table's current state for its key. With
;; #:outer 'left an s with no row passes with the table's attributes null;
;; otherwise it is dropped and counted. Enriching outputs with an entity
;; directory's labels is such a lookup.

(define (table-join-operator #:key key #:type [type 'Joined] #:combine [combine default-combine]
                             #:outer [outer #f] #:right-attributes [rattrs '()])
  (make-operator
   'table-join
   #:arity 2
   #:key key
   #:init-key (λ (gl k c) 'no-row)
   #:on-event
   (λ (gl row e c)
     (cond
       [(= (ctx-port c) 1) e]                                    ; upsert
       [(event? row) (emit! c (joined type combine e row)) row]
       [(keeps-left? outer) (emit! c (unmatched type combine e rattrs 'left)) row]
       [else (count! c 'unmatched) row]))
   #:on-retract (λ (gl row e c) (if (and (event? row) (equal? (event-id row) (event-id e))) 'no-row row))))

;; ===========================================================================
;; JN.as-of: each s is matched with the version of r valid at t(s): for its
;; key, the r with the greatest valid_from <= t(s) (valid_from is ts(r)
;; unless #:valid-from says otherwise). A version may still arrive late, so
;; s waits until the watermark (the minimum over both inputs) reaches t(s).

(struct asof (versions pending) #:transparent)   ; versions: list sorted by valid-from; pending: s events

(define (as-of-join-operator #:key key #:valid-from [valid-from event-ts] #:type [type 'Joined]
                             #:combine [combine default-combine] #:outer [outer #f]
                             #:right-attributes [rattrs '()])
  (define (version-at versions t)
    (for/last ([r (in-list versions)] #:when (<= (valid-from r) t)) r))
  (make-operator
   'as-of-join
   #:arity 2
   #:monotone? #f
   #:key key
   #:init-key (λ (gl k c) (asof '() '()))
   #:on-event
   (λ (gl st e c)
     (if (= (ctx-port c) 1)
         (asof (sort (cons e (asof-versions st)) < #:key valid-from) (asof-pending st))
         (asof (asof-versions st) (append (asof-pending st) (list e)))))
   #:event-deadline (λ (st) (and (pair? (asof-pending st)) (apply min (map event-ts (asof-pending st)))))
   #:on-timer
   (λ (gl st c)
     (define W (ctx-watermark c))
     (define-values (due later) (partition (λ (s) (<= (event-ts s) W)) (asof-pending st)))
     (for ([s (in-list (sort due < #:key event-ts))])          ; released in event-time order
       (define r (version-at (asof-versions st) (event-ts s)))
       (cond [r (emit! c (joined type combine s r))]
             [(keeps-left? outer) (emit! c (unmatched type combine s rattrs 'left))]
             [else (count! c 'unmatched)]))
     (asof (asof-versions st) later))))

(provide
 (contract-out
  [table-join-operator (->* (#:key (-> event? any/c))
                            (#:type symbol? #:combine (-> event? event? hash?) #:outer (or/c #f 'left)
                             #:right-attributes (listof symbol?))
                            operator?)]
  [as-of-join-operator (->* (#:key (-> event? any/c))
                            (#:valid-from (-> event? time?) #:type symbol? #:combine (-> event? event? hash?)
                             #:outer (or/c #f 'left) #:right-attributes (listof symbol?))
                            operator?)]))
