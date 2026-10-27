#lang racket/base
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/time.rkt"
         "../private/event.rkt"
         "../private/operator.rkt"
         "../private/dataflow.rkt"
         "../private/progress.rkt")

(define (contrib kind ts te n) (make-batch-event 'C `((dev . "d1") (kind . ,kind)) #:ts ts #:te te #:n n))

(spec-test 'TM.keyed-watermark "W^any is the fastest kind and W^all the slowest; unseen kinds are −∞"
  ;; the contributions of trace R3 (§16.7)
  (define steps (list (contrib 'load 0 50 50) (contrib 'position 0 70 70) (contrib 'load 51 130 80)
                      (contrib 'position 71 250 180) (contrib 'load 131 300 170)))
  (define M '(load position))
  (define ws (for/fold ([acc '()] [w empty-keyed-wm] #:result (reverse acc)) ([e steps])
               (define w2 (keyed-wm-observe w (event-attr e 'kind) (event-te e)))
               (values (cons (list (keyed-wm-all w2 M) (keyed-wm-any w2 M)) acc) w2)))
  (check-equal? (map car ws) '(-inf.0 50 70 130 250))
  (check-equal? (map cadr ws) '(50 70 130 250 300))
  ;; with every kind watched, M is the kinds seen so far
  (check-equal? (keyed-wm-all (keyed-wm-observe empty-keyed-wm 'load 5) #f) 5)
  (check-equal? (keyed-wm-any empty-keyed-wm #f) -inf.0))

(spec-test 'TM.keyed-watermark "the operator emits progress per key when W_k advances"
  (define op (keyed-watermark-operator #:key (λ (e) (event-attr e 'dev)) #:kind (λ (e) (event-attr e 'kind))
                                       #:kinds '(load position) #:mode 'all))
  (define src (make-source 'in))
  (define n (make-node 'kw op (list src)))
  (define p (make-node 'progress (make-operator 'p) (list (cons n 1)) #:sink? #t))
  (define r (run (make-program (list p))
                 (for/list ([e (list (contrib 'load 0 50 50) (contrib 'position 0 70 70) (contrib 'load 51 130 80))]
                            [i (in-naturals)])
                   (arrive i 'in e))))
  (check-equal? (map (λ (e) (event-attr e 'watermark)) (run-result-events r)) '(50 70)))

(spec-test 'TM.keyed-watermark "property: W^all <= W^any, and both are monotone"
  (check-property
   (make-config #:tests (trials))
   (property ([obs (gen:list (gen:tuple (gen:one-of '(a b c)) (gen:integer-in 0 100)) #:max-length 20)])
     (for/fold ([w empty-keyed-wm] [pa -inf.0] [pl -inf.0] #:result (void)) ([o obs])
       (define w2 (keyed-wm-observe w (first o) (second o)))
       (define any (keyed-wm-any w2 '(a b c)))
       (define all (keyed-wm-all w2 '(a b c)))
       (check-true (<= all any))
       (check-true (>= any pa))
       (check-true (>= all pl))
       (values w2 any all)))))
