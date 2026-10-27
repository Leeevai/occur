#lang racket/base
(require rackunit
         "util.rkt"
         "../private/event.rkt"
         "../private/operator.rkt")

(spec-test 'ES.operator "an operator bundles callbacks over immutable state, with defaults"
  (define op (make-operator 'id))
  (check-equal? (operator-arity op) 1)
  (check-true (operator-monotone? op))
  (define c (make-ctx #:node 'id #:key #f #:now 5 #:watermark -inf.0))
  (define e (make-event 'X '() #:at 1))
  ;; the default on-event passes the event through
  (check-equal? ((operator-on-event op) #f #f e c) #f)
  (check-equal? (ctx-emissions c) (list (cons 0 e))))

(test-case "the context accumulates emissions, counters, and reports in order"
  (define c (make-ctx #:node 'n #:key 'k #:now 5 #:watermark 3 #:port 1 #:clock 'event-time))
  (define e (make-event 'X '() #:at 1))
  (emit! c e)
  (emit! c (retraction e) 2)
  (count! c 'late 3)
  (count! c 'late)
  (report! c "dangling binding")
  (check-equal? (map car (ctx-emissions c)) '(0 2))
  (check-true (retraction? (cdr (cadr (ctx-emissions c)))))
  (check-equal? (ctx-count-list c) '((late . 3) (late . 1)))
  (check-equal? (ctx-report-list c) '("dangling binding"))
  (check-equal? (list (ctx-node c) (ctx-key c) (ctx-port c) (ctx-clock c) (ctx-now c) (ctx-watermark c))
                '(n k 1 event-time 5 3)))

(test-case "a stateless operator emits from a plain procedure"
  (define op (stateless-operator 'double (λ (e c) (emit! c e) (emit! c e)) #:monotone? #t))
  (define c (make-ctx #:node 'double #:key #f #:now 0 #:watermark 0))
  ((operator-on-event op) #f #f (make-event 'X '() #:at 0) c)
  (check-equal? (length (ctx-emissions c)) 2))
