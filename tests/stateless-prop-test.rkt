#lang racket/base
;; Property tests for the single-event operations (spec §4).
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/event.rkt"
         "../private/operator.rkt"
         "../private/dataflow.rkt"
         "../private/ops/stateless.rkt")

(define gen:events
  (gen:let ([vs (gen:list (gen:integer-in 0 20) #:max-length 15)])
    (for/list ([v vs] [i (in-naturals)])
      (make-event 'R `((v . ,v) (w . ,(* 2 v))) #:id (format "e~a" i) #:at i))))

(define (out op events) (run-result-events (run-operator op (list events))))
(define (payloads es) (map event-payload es))

(spec-test 'OP.filter "property: filter is monotone and σ_p ∘ σ_q = σ_{p ∧ q}"
  (check-property
   (make-config #:tests (trials))
   (property ([es gen:events] [k (gen:integer-in 0 15)] [a (gen:integer-in 0 20)] [b (gen:integer-in 0 20)])
     (define p (λ (e) (>= (event-attr e 'v) a)))
     (define q (λ (e) (<= (event-attr e 'v) b)))
     (define full (map event-id (out (filter-operator p) es)))
     (define prefix (map event-id (out (filter-operator p) (take es (min k (length es))))))
     (check-equal? prefix (take full (length prefix)))
     (check-equal? (map event-id (out (filter-operator p) (out (filter-operator q) es)))
                   (map event-id (out (filter-operator (λ (e) (and (p e) (q e)))) es))))))

(spec-test 'OP.project "property: projection is idempotent"
  (check-property
   (make-config #:tests (trials))
   (property ([es gen:events])
     (define once (out (project-operator '(v)) es))
     (check-equal? (payloads (out (project-operator '(v)) once)) (payloads once)))))

(spec-test 'OP.map "property: μ_g ∘ μ_f = μ_{g ∘ f} on payloads"
  (check-property
   (make-config #:tests (trials))
   (property ([es gen:events])
     (define (f e) (hasheq 'v (add1 (event-attr e 'v))))
     (define (g e) (hasheq 'v (* 3 (event-attr e 'v))))
     (check-equal? (payloads (out (map-operator g) (out (map-operator f) es)))
                   (payloads (out (map-operator (λ (e) (g (make-event 'R (f e) #:at 0)))) es))))))

(define (route-ports op events n)
  (define src (make-source 'in))
  (define r (make-node 'route op (list src)))
  (define sinks (for/list ([p n]) (make-node (string->symbol (format "p~a" p)) (make-operator 'id)
                                             (list (cons r p)) #:sink? #t)))
  (define res (run (make-program sinks) (for/list ([e events]) (arrive 1 'in e))))
  (for/list ([p n]) (map event-id (run-result-events res (string->symbol (format "p~a" p))))))

(spec-test 'OP.route "property: exclusive routing with otherwise partitions S; inclusive is σ per port"
  (check-property
   (make-config #:tests (trials))
   (property ([es gen:events] [a (gen:integer-in 0 20)] [b (gen:integer-in 0 20)])
     (define preds (list (λ (e) (>= (event-attr e 'v) a)) (λ (e) (<= (event-attr e 'v) b))))
     (define ex (route-ports (route-operator preds #:otherwise? #t) es 3))
     (check-equal? (sort (append* ex) string<?) (sort (map event-id es) string<?))
     (define in (route-ports (route-operator preds #:mode 'inclusive) es 2))
     (for ([p preds] [ids in])
       (check-equal? ids (map event-id (filter p es)))))))

(spec-test 'OP.split "property: split children all record their parent and get fresh ids"
  (check-property
   (make-config #:tests (trials))
   (property ([es gen:events])
     (define op (split-operator (λ (e) (for/list ([i (event-attr e 'v)])
                                         (make-event 'C `((i . ,i)) #:at (event-te e))))))
     (define kids (out op es))
     (check-equal? (length kids) (apply + (map (λ (e) (event-attr e 'v)) es)))
     (check-equal? (length (remove-duplicates (map event-id kids))) (length kids))
     (for ([k kids]) (check-equal? (length (event-constituents k)) 1)))))
