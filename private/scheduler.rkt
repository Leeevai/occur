#lang racket/base
;; The deadline scheduler (spec §17, "No polling loop").
;;
;; Each operator instance reports the next moment it needs attention without
;; new data. A single min-heap of deadlines wakes instances when they are due.
;; An instance arms a deadline only if it is earlier than the one it already
;; holds, so the heap grows with deadlines, not with arrivals. A wake-up that
;; turns out to be early is harmless, because evaluation always re-checks the
;; condition against the current time.
;;
;; The heap is an immutable pairing heap so that the whole scheduler can be
;; saved in a checkpoint by reference. Entries with equal times pop in the
;; order they were armed (FIFO), which keeps runs deterministic.
(require racket/contract)

;; ---------------------------------------------------------------------------
;; Immutable pairing heap of entries #(time seq token), ordered by (time, seq).

(struct ph (entry subs))   ; subs: list of ph

(define (entry<? a b)
  (or (< (vector-ref a 0) (vector-ref b 0))
      (and (= (vector-ref a 0) (vector-ref b 0))
           (< (vector-ref a 1) (vector-ref b 1)))))

(define (ph-merge a b)
  (cond [(not a) b]
        [(not b) a]
        [(entry<? (ph-entry a) (ph-entry b)) (ph (ph-entry a) (cons b (ph-subs a)))]
        [else (ph (ph-entry b) (cons a (ph-subs b)))]))

(define (ph-merge-pairs subs)
  (cond [(null? subs) #f]
        [(null? (cdr subs)) (car subs)]
        [else (ph-merge (ph-merge (car subs) (cadr subs)) (ph-merge-pairs (cddr subs)))]))

(define (ph-insert h e) (ph-merge (ph e '()) h))
(define (ph-pop h) (ph-merge-pairs (ph-subs h)))

;; ---------------------------------------------------------------------------
;; Deadlines: the heap, the armed deadline per token, and a sequence counter.

(struct deadlines (heap armed seq size))

(define empty-deadlines (deadlines #f (hash) 0 0))

;; Arm `token` at time t, unless it already holds an earlier (or equal) deadline.
(define (deadlines-arm d token t)
  (define held (hash-ref (deadlines-armed d) token #f))
  (if (and held (<= held t))
      d
      (deadlines (ph-insert (deadlines-heap d) (vector t (deadlines-seq d) token))
                 (hash-set (deadlines-armed d) token t)
                 (add1 (deadlines-seq d))
                 (add1 (deadlines-size d)))))

(define (deadlines-armed-at d token) (hash-ref (deadlines-armed d) token #f))

;; The earliest live deadline, skipping superseded entries.
(define (deadlines-next d)
  (define h (deadlines-heap d))
  (cond [(not h) #f]
        [else (define e (ph-entry h))
              (if (equal? (hash-ref (deadlines-armed d) (vector-ref e 2) #f) (vector-ref e 0))
                  (vector-ref e 0)
                  (deadlines-next (deadlines (ph-pop h) (deadlines-armed d) (deadlines-seq d)
                                             (sub1 (deadlines-size d)))))]))

;; Pop the earliest live deadline at or before `limit`.
;; Returns (values token time d') or (values #f #f d') when none is due.
;; An entry is live when it is the deadline its token currently holds;
;; superseded entries are discarded without waking anyone.
(define (deadlines-pop-due d limit)
  (let loop ([d d])
    (define h (deadlines-heap d))
    (cond
      [(not h) (values #f #f d)]
      [else
       (define e (ph-entry h))
       (define t (vector-ref e 0))
       (define token (vector-ref e 2))
       (define live? (equal? (hash-ref (deadlines-armed d) token #f) t))
       (cond
         [(not live?)
          (loop (deadlines (ph-pop h) (deadlines-armed d) (deadlines-seq d) (sub1 (deadlines-size d))))]
         [(> t limit) (values #f #f d)]
         [else (values token t (deadlines (ph-pop h)
                                          (hash-remove (deadlines-armed d) token)
                                          (deadlines-seq d)
                                          (sub1 (deadlines-size d))))])])))

;; Drop every pending deadline (used when timers are suspended and resumed).
(define (deadlines-tokens d) (hash-keys (deadlines-armed d)))

(provide
 empty-deadlines
 (contract-out
  [deadlines? (-> any/c boolean?)]
  [deadlines-size (-> deadlines? exact-nonnegative-integer?)]
  [deadlines-arm (-> deadlines? any/c real? deadlines?)]
  [deadlines-armed-at (-> deadlines? any/c (or/c #f real?))]
  [deadlines-next (-> deadlines? (or/c #f real?))]
  [deadlines-pop-due (-> deadlines? real? (values any/c (or/c #f real?) deadlines?))]
  [deadlines-tokens (-> deadlines? list?)]))
