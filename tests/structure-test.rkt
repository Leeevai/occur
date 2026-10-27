#lang racket/base
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/time.rkt"
         "../private/event.rkt"
         "../private/operator.rkt"
         "../private/dataflow.rkt"
         "../private/ops/structure.rkt")

(define (ev id t [payload '()] #:te [te t] #:n [n 1]) (make-event 'X payload #:id id #:ts t #:te te #:n n))
(define (ids es) (map event-id es))

(spec-test 'ST.merge "merge interleaves its inputs by arrival"
  (define a (make-source 'a))
  (define b (make-source 'b))
  (define m (make-node 'm (merge-operator 2) (list a b) #:sink? #t))
  (define r (run (make-program (list m))
                 (list (arrive 1 'a (ev "a1" 1)) (arrive 2 'b (ev "b1" 0)) (arrive 3 'a (ev "a2" 3)))))
  (check-equal? (ids (run-result-events r)) '("a1" "b1" "a2"))
  (check-equal? (map output-at (run-result-outputs r)) '(1 2 3)))

(spec-test '(ST.key-by ES.keyed) "key-by partitions downstream state; re-keying is the same operation"
  (define src (make-source 'in))
  (define by-sender (make-node 'k (key-by-operator (attribute-key-fn 'sender)) (list src)))
  (define count (make-operator 'count #:key (attribute-key-fn 'owner) #:init-key (λ (g k c) 0)
                               #:on-event (λ (g s e c)
                                            (emit! c (make-event 'N `((owner . ,(ctx-key c)) (n . ,(add1 s)))
                                                                 #:at (event-te e)))
                                            (add1 s))))
  (define n (make-node 'n count (list by-sender) #:sink? #t))
  (define r (run (make-program (list n))
                 (for/list ([s '(d1 d2 d1)] [o '(d7 d7 d8)] [i (in-naturals)])
                   (arrive i 'in (ev (format "e~a" i) i `((sender . ,s) (owner . ,o)))))))
  ;; keyed by the owning device, not the device that sent the reading
  (check-equal? (map (λ (e) (list (event-attr e 'owner) (event-attr e 'n))) (run-result-events r))
                '((d7 1) (d7 2) (d8 1)))
  (check-equal? (sort (hash-keys (cdr (hash-ref (run-result-states r) 'n))) symbol<?) '(d7 d8)))
