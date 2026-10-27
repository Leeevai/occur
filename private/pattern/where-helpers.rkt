#lang racket/base
;; List helpers visible in #:where and #:emit expressions of define-pattern.
;; They accept the lists a Kleene binding stands for, so a where clause can
;; read like the spec: (>= (count b) 3), (> (sum b.amount) 1000),
;; (increasing b.amount), (>= (span b) 60). min and max also keep working on
;; numbers.
(require racket/list
         (for-syntax racket/base)
         "../event.rkt"
         "families.rkt"
         (only-in "../aggregate.rkt" aggregate-events median-agg percentile-agg))

(provide with-where-helpers)

;; count: the multiplicity sum of a list of events (AG.multiplicity), or the
;; length of a list of values.
(define (count* xs)
  (if (andmap event? xs) (apply + (map event-n xs)) (length xs)))
(define (sum* xs) (apply + xs))
(define (avg* xs) (and (pair? xs) (/ (apply + xs) (length xs))))
(define (extreme f) (case-lambda [(xs) (and (pair? xs) (apply f xs))] [(x . ys) (apply f x ys)]))
(define min* (extreme min))
(define max* (extreme max))
;; span: max te − min ts over a list of events
(define (span* es) (if (null? es) 0 (- (apply max (map event-te es)) (apply min (map event-ts es)))))
(define (median* xs)
  (define s (sort xs <))
  (define n (length s))
  (cond [(zero? n) #f] [(odd? n) (list-ref s (quotient n 2))]
        [else (/ (+ (list-ref s (sub1 (quotient n 2))) (list-ref s (quotient n 2))) 2)]))

;; (with-where-helpers lexical body): the helpers are bound in the lexical
;; context of `lexical`, the user's syntax.
(define-syntax (with-where-helpers stx)
  (syntax-case stx ()
    [(_ lexical body)
     (with-syntax ([(count sum avg min max span median increasing decreasing non-increasing non-decreasing stable mixed)
                    (map (λ (s) (datum->syntax #'lexical s))
                         '(count sum avg min max span median increasing decreasing non-increasing non-decreasing stable mixed))])
       #'(let ([count count*] [sum sum*] [avg avg*] [min min*] [max max*] [span span*] [median median*]
               [increasing increasing?] [decreasing decreasing?] [non-increasing non-increasing?]
               [non-decreasing non-decreasing?] [stable stable?] [mixed mixed?])
           body))]))
