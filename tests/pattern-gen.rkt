#lang racket/base
;; Random patterns and histories for differential tests of the engine.
(require racket/list
         rackcheck
         "../private/event.rkt"
         "../private/pattern/ast.rkt")

(provide random-pattern gen:history gen:point-history)

;; A random well-formed pattern from a seed. Leaves are over types A, B, C
;; with optional predicates; the negated types are V and A.
(define (random-pattern seed #:depth [depth 3] #:fragment [fragment 'all])
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng]) (random-seed seed))
  (define (pick xs) (list-ref xs (random (length xs) rng)))
  (define counter 0)
  (define (fresh-name) (set! counter (add1 counter)) (string->symbol (format "x~a" counter)))
  (define (leaf)
    (define t (pick '(A B C)))
    (define pred (pick (list #f #f (λ (e) (even? (event-attr e 'v))) (λ (e) (> (event-attr e 'v) 1)))))
    (prim t #:bind (fresh-name) #:when pred))
  (define (neg) (prim (pick '(V A))))
  (define (strict-leafy d) ; never nullable
    (if (or (zero? d) (< (random rng) 0.4))
        (leaf)
        ((pick (list (λ () (seq (strict-leafy (sub1 d)) (strict-leafy (sub1 d))))
                     (λ () (conj (strict-leafy (sub1 d)) (leaf)))
                     (λ () (disj (strict-leafy (sub1 d)) (strict-leafy (sub1 d))))
                     (λ () (kleene+ (leaf))))))))
  (define (gen d)
    (cond
      [(or (zero? d) (< (random rng) 0.25)) (leaf)]
      [else
       (define ops
         (case fragment
           [(selection) ; the fragment selection strategies apply to
            (list (λ () (seq (gen (sub1 d)) (gen (sub1 d))))
                  (λ () (kleene+ (leaf)))
                  (λ () (seq (leaf) (kleene* (leaf)) (leaf)))
                  (λ () (repeat (leaf) 1 2))
                  (λ () (neg-between (strict-leafy (sub1 d)) (neg) (strict-leafy (sub1 d)))))]
           [else
            (list (λ () (seq (gen (sub1 d)) (gen (sub1 d))))
                  (λ () (conj (gen (sub1 d)) (gen (sub1 d))))
                  (λ () (disj (gen (sub1 d)) (gen (sub1 d))))
                  (λ () (neg-between (strict-leafy (sub1 d)) (neg) (strict-leafy (sub1 d))))
                  (λ () (absence (gen (sub1 d)) (neg) (add1 (random 4 rng))))
                  (λ () (absence act (neg) (add1 (random 6 rng))))
                  (λ () (span-absence (gen (sub1 d)) (neg)))
                  (λ () (kleene+ (strict-leafy 1)))
                  (λ () (seq (leaf) (kleene* (leaf)) (leaf)))
                  (λ () (repeat (leaf) (random 3 rng) 2))
                  (λ () (any-of (add1 (random 2 rng)) (gen (sub1 d)) (gen (sub1 d))))
                  (λ () (within (gen (sub1 d)) (random 6 rng)))
                  (λ () (where (gen (sub1 d)) (λ (bs) (<= (hash-count bs) 2))))
                  (λ () (aperiodic (leaf) (leaf) (leaf)))
                  (λ () (aperiodic* (leaf) (leaf) (leaf)))
                  (λ () (periodic (leaf) (add1 (random 3 rng)) (leaf)))
                  (λ () (periodic* (leaf) (add1 (random 3 rng)) (leaf))))]))
       ((pick ops))]))
  (let retry ()
    (define p (gen depth))
    (if (pattern-problem p) (retry) p)))

;; Histories: events of types A B C V with small times and values.
(define (make-history specs)
  (for/list ([s specs] [i (in-naturals)])
    (define t (list-ref s 1))
    (make-event (list-ref s 0) `((v . ,(list-ref s 3))) #:id (format "e~a" i) #:ts t #:te (+ t (list-ref s 2)))))

(define gen:history
  (gen:let ([specs (gen:list (gen:tuple (gen:one-of '(A B C V)) (gen:integer-in 0 9) (gen:one-of '(0 0 0 1 2))
                                        (gen:integer-in 0 3))
                             #:max-length 6)])
    (make-history specs)))

(define gen:point-history
  (gen:let ([specs (gen:list (gen:tuple (gen:one-of '(A B C V)) (gen:integer-in 0 9) (gen:const 0) (gen:integer-in 0 3))
                             #:max-length 6)])
    (sort (make-history specs) < #:key event-ts)))
