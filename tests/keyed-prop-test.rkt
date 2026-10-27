#lang racket/base
;; ES.keyed: if no operator reads another key's state, the program scales by
;; partition: runs over disjoint slices of the keys emit exactly what one run
;; emits.
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/event.rkt"
         "../private/operator.rkt"
         "../private/dataflow.rkt"
         "../private/ops/stateless.rkt"
         "../private/ops/structure.rkt")

(define (program)
  (define src (make-source 'in))
  (define k (make-node 'k (key-by-operator (attribute-key-fn 'k)) (list src)))
  (define d (make-node 'd (dedupe-operator (λ (e) (cons (event-attr e 'k) (event-attr e 'v))) #:horizon 3)
                       (list k)))
  (define t (make-node 't (throttle-operator '(every 2) #:key (attribute-key-fn 'k)) (list d) #:sink? #t))
  (define m (make-node 'm (monotonic-merge-operator (attribute-key-fn 'k)) (list k) #:sink? #t))
  (make-program (list t m)))

(define gen:arrivals
  (gen:let ([xs (gen:list (gen:tuple (gen:integer-in 0 5) (gen:integer-in 0 3) (gen:integer-in 0 4))
                          #:max-length 25)])
    (for/list ([x xs] [i (in-naturals)])
      (define t (+ i (car x)))
      (arrive i 'in (make-batch-event 'R `((k . ,(cadr x)) (v . ,(caddr x)))
                                      #:ts t #:te (+ t (caddr x)) #:n (add1 (caddr x)))))))

;; Outputs modulo ids: (processing time, node, payload, interval, multiplicity)
(define (observable r)
  (sort (for/list ([o (run-result-outputs r)])
          (define e (output-item o))
          (list (output-at o) (output-node o) (event-attr e 'k) (event-attr e 'v)
                (event-ts e) (event-te e) (event-n e)))
        (λ (a b) (string<? (format "~s" a) (format "~s" b)))))

(define (observable-union a b)
  (sort (append a b) (λ (x y) (string<? (format "~s" x) (format "~s" y)))))

;; The watermark is the one piece of cross-key information in a run, so the
;; source declares none: every event-time decision is made per key.
(spec-test 'ES.keyed "property: partitioned runs over disjoint key slices emit exactly what one run emits"
  (check-property
   (make-config #:tests (trials))
   (property ([steps gen:arrivals] [cut (gen:integer-in 0 4)])
     (define (slice keep?) (filter (λ (s) (keep? (event-attr (step:arrive-event s) 'k))) steps))
     (define whole (observable (run (program) steps)))
     (define left (observable (run (program) (slice (λ (k) (< k cut))))))
     (define right (observable (run (program) (slice (λ (k) (>= k cut))))))
     (check-equal? (observable-union left right) whole))))
