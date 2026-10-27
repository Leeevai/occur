#lang racket/base
(require rackunit
         racket/port
         "../private/event.rkt"
         "../private/operator.rkt"
         "../private/dataflow.rkt"
         "../private/io.rkt"
         "../private/registry.rkt")

(define Swipe (make-event-type 'Swipe '((card . String))))
(define (sw id t) (make-event 'Swipe '((card . "c1")) #:id id #:at t))

(test-case "a registry merges source data by processing time into a script"
  (define r (make-registry))
  (registry-add-type! r Swipe)
  (define a (make-source 'a #:types (list Swipe) #:data (λ () (list (sw "a1" 5) (arrival 1 (sw "a2" 9))))))
  (define b (make-source 'b #:types (list Swipe) #:data (λ () (list (sw "b1" 5)))))
  (registry-add-source! r a)
  (registry-add-source! r b)
  (registry-add-node! r (make-node 'all (make-operator 'all #:arity 2) (list a b) #:sink? #t))
  (check-equal? (map (λ (s) (list (step:arrive-time s) (event-id (step:arrive-event s)))) (registry-steps r))
                '((1 "a2") (5 "a1") (5 "b1")))
  (define out (with-output-to-string (λ () (print-run-result (run-registry r)))))
  (check-equal? out "@1 all: Swipe [9, 9] card=\"c1\"\n@5 all: Swipe [5, 5] card=\"c1\"\n@5 all: Swipe [5, 5] card=\"c1\"\n"))

(test-case "setup steps run first and an explicit script replaces the data"
  (define r (make-registry))
  (define a (make-source 'a #:data (λ () (list (sw "a1" 5)))))
  (registry-add-source! r a)
  (registry-add-node! r (make-node 'out (make-operator 'out) (list a) #:sink? #t))
  (registry-add-setup! r (list (control 'hello)))
  (registry-set-script! r (list (arrive 2 'a (sw "x" 2))))
  (check-equal? (length (registry-steps r)) 2)
  (check-equal? (map event-id (run-result-events (run-registry r))) '("x")))
