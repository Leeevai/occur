#lang racket/base
;; Temporal relations: Allen's interval algebra (spec §10).
;;
;; For proper intervals a = [a⁻, a⁺] and b = [b⁻, b⁺] (a⁻ < a⁺, b⁻ < b⁺),
;; exactly one of 13 relations holds. Between two points only before, equals
;; and after are distinguishable; between a point p and an interval b there
;; are five relations (before, starts, during, finishes, after), and their
;; inverses between an interval and a point. Arguments may be intervals or
;; events (their occurrence intervals [ts, te]).
(require racket/contract
         "time.rkt"
         "event.rkt")

(define (bounds x)
  (cond [(interval? x) (values (interval-start x) (interval-end x))]
        [(event? x) (values (event-ts x) (event-te x))]
        [(and (pair? x) (real? (car x)) (real? (cdr x))) (values (car x) (cdr x))]
        [else (raise-argument-error 'allen "(or/c interval? event? (cons/c real? real?))" x)]))

(define relations
  '(before after meets met-by overlaps overlapped-by starts started-by during contains
    finishes finished-by equals))

(define inverses
  (hasheq 'before 'after 'after 'before 'meets 'met-by 'met-by 'meets
          'overlaps 'overlapped-by 'overlapped-by 'overlaps 'starts 'started-by 'started-by 'starts
          'during 'contains 'contains 'during 'finishes 'finished-by 'finished-by 'finishes
          'equals 'equals))

(define (allen-inverse r) (hash-ref inverses r))

;; The relation between two proper intervals.
(define (interval-relation a- a+ b- b+)
  (cond
    [(< a+ b-) 'before]
    [(> a- b+) 'after]
    [(= a+ b-) 'meets]
    [(= a- b+) 'met-by]
    [(and (= a- b-) (= a+ b+)) 'equals]
    [(and (= a- b-) (< a+ b+)) 'starts]
    [(and (= a- b-) (> a+ b+)) 'started-by]
    [(and (= a+ b+) (> a- b-)) 'finishes]
    [(and (= a+ b+) (< a- b-)) 'finished-by]
    [(and (> a- b-) (< a+ b+)) 'during]
    [(and (< a- b-) (> a+ b+)) 'contains]
    [(< a- b-) 'overlaps]            ; a⁻ < b⁻ < a⁺ < b⁺
    [else 'overlapped-by]))           ; b⁻ < a⁻ < b⁺ < a⁺

;; The relation between a point p and a proper interval b.
(define (point-interval-relation p b- b+)
  (cond [(< p b-) 'before] [(= p b-) 'starts] [(< p b+) 'during] [(= p b+) 'finishes] [else 'after]))

;; allen-relation : x y -> symbol, for proper intervals, points, or a mix.
(define (allen-relation x y)
  (define-values (a- a+) (bounds x))
  (define-values (b- b+) (bounds y))
  (define a-point? (= a- a+))
  (define b-point? (= b- b+))
  (cond
    [(and a-point? b-point?) (cond [(< a- b-) 'before] [(= a- b-) 'equals] [else 'after])]
    [a-point? (point-interval-relation a- b- b+)]
    [b-point? (allen-inverse (point-interval-relation b- a- a+))]
    [else (interval-relation a- a+ b- b+)]))

(define ((relation? r) x y) (eq? (allen-relation x y) r))
(define before? (relation? 'before))
(define after? (relation? 'after))
(define meets? (relation? 'meets))
(define met-by? (relation? 'met-by))
(define overlaps? (relation? 'overlaps))
(define overlapped-by? (relation? 'overlapped-by))
(define starts? (relation? 'starts))
(define started-by? (relation? 'started-by))
(define during? (relation? 'during))
(define contains? (relation? 'contains))
(define finishes? (relation? 'finishes))
(define finished-by? (relation? 'finished-by))
(define equals? (relation? 'equals))

(define span/c (or/c interval? event? (cons/c real? real?)))
(define relation/c (-> span/c span/c boolean?))

(provide
 relations
 (contract-out
  [allen-relation (-> span/c span/c symbol?)]
  [allen-inverse (-> symbol? symbol?)]
  [before? relation/c] [after? relation/c] [meets? relation/c] [met-by? relation/c]
  [overlaps? relation/c] [overlapped-by? relation/c] [starts? relation/c] [started-by? relation/c]
  [during? relation/c] [contains? relation/c] [finishes? relation/c] [finished-by? relation/c]
  [equals? relation/c]))
