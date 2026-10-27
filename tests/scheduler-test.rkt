#lang racket/base
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/scheduler.rkt")

(define (drain d limit)
  (let loop ([d d] [acc '()])
    (define-values (tok t d2) (deadlines-pop-due d limit))
    (if tok (loop d2 (cons (cons tok t) acc)) (reverse acc))))

(test-case "deadlines pop in time order, FIFO for equal times"
  (define d (for/fold ([d empty-deadlines]) ([p '((a . 30) (b . 10) (c . 20) (d . 10))])
              (deadlines-arm d (car p) (cdr p))))
  (check-equal? (deadlines-next d) 10)
  (check-equal? (drain d 25) '((b . 10) (d . 10) (c . 20)))
  (check-equal? (drain d 100) '((b . 10) (d . 10) (c . 20) (a . 30))))

(test-case "an instance arms only deadlines earlier than the one it holds"
  (define d1 (deadlines-arm empty-deadlines 'x 50))
  (define d2 (deadlines-arm d1 'x 80))      ; later: ignored
  (check-equal? (deadlines-size d2) 1)
  (check-equal? (deadlines-armed-at d2 'x) 50)
  (define d3 (deadlines-arm d2 'x 20))      ; earlier: armed
  (check-equal? (deadlines-armed-at d3 'x) 20)
  ;; the superseded entry at 50 is discarded without waking x a second time
  (check-equal? (drain d3 100) '((x . 20))))

(test-case "after a token fires it may arm again"
  (define d (deadlines-arm empty-deadlines 'x 10))
  (define-values (tok t d2) (deadlines-pop-due d 10))
  (check-equal? (list tok t) '(x 10))
  (check-false (deadlines-armed-at d2 'x))
  (check-equal? (drain (deadlines-arm d2 'x 40) 100) '((x . 40))))

(test-case "property: the heap grows with deadlines, not arrivals, and pops sorted"
  (check-property
   (make-config #:tests (trials))
   (property ([arms (gen:list (gen:tuple (gen:one-of '(a b c d)) (gen:integer-in 0 100)) #:max-length 30)])
     (define d (for/fold ([d empty-deadlines]) ([p arms]) (deadlines-arm d (first p) (second p))))
     (define popped (drain d 1000))
     ;; each token wakes once, at the earliest deadline it armed
     (define expected
       (for/hash ([tok (remove-duplicates (map first arms))])
         (values tok (apply min (map second (filter (λ (p) (eq? (first p) tok)) arms))))))
     (check-equal? (length popped) (hash-count expected))
     (for ([p popped]) (check-equal? (cdr p) (hash-ref expected (car p))))
     (check-equal? (map cdr popped) (sort (map cdr popped) <)))))

(test-case "a strict deadline is due only once the limit exceeds it"
  (define d (deadlines-arm empty-deadlines 'x (strictly-after 10)))
  (define-values (t1 _1 d1) (deadlines-pop-due d 10))
  (check-false t1)
  (define-values (t2 at d2) (deadlines-pop-due d1 11))
  (check-equal? (list t2 at) '(x 10))
  ;; a plain deadline at the same time comes first and supersedes the strict one
  (define both (deadlines-arm (deadlines-arm empty-deadlines 'y (strictly-after 5)) 'y 5))
  (check-equal? (deadlines-armed-at both 'y) 5))
