#lang racket/base
;; A module registry collects the declarations of one #lang occur module
;; (types, sources, streams, control pushes, an optional explicit script)
;; and turns them into a program and a script for the runner. It is plain
;; runtime data, so programs can also be assembled without the #lang.
(require racket/contract
         racket/list
         "time.rkt"
         "event.rkt"
         "operator.rkt"
         "dataflow.rkt"
         "io.rkt")

(struct registry ([types #:mutable] [sources #:mutable] [nodes #:mutable]
                  [setup #:mutable] [script #:mutable] [options #:mutable])
  #:transparent)

(define (make-registry) (registry '() '() '() '() #f (hasheq)))

(define (registry-add-type! r t) (set-registry-types! r (append (registry-types r) (list t))))
(define (registry-add-source! r s) (set-registry-sources! r (append (registry-sources r) (list s))))
(define (registry-add-node! r n) (set-registry-nodes! r (append (registry-nodes r) (list n))))
;; Steps run before the data, such as directory and rule pushes at start.
(define (registry-add-setup! r steps) (set-registry-setup! r (append (registry-setup r) steps)))
;; An explicit script replaces the data-derived arrivals.
(define (registry-set-script! r steps) (set-registry-script! r steps))
(define (registry-set-option! r k v) (set-registry-options! r (hash-set (registry-options r) k v)))

(define (registry-program r)
  (make-program (append (registry-sources r) (registry-nodes r))))

;; The arrivals of every source with data, merged by processing time; ties
;; keep source declaration order, then data order.
(define (source-arrivals s)
  (define d (source-data s))
  (cond [(not d) '()]
        [else (for/list ([x (in-list (d))])
                (if (arrival? x) x (arrival (event-te x) x)))]))

(define (registry-steps r)
  (append
   (registry-setup r)
   (or (registry-script r)
       (let* ([tagged (for*/list ([(s i) (in-parallel (registry-sources r) (in-naturals))]
                                  [(a j) (in-parallel (source-arrivals s) (in-naturals))])
                        (list (arrival-at a) i j (source-name s) (arrival-event a)))]
              [sorted (sort tagged (λ (x y)
                                     (or (< (car x) (car y))
                                         (and (= (car x) (car y))
                                              (or (< (cadr x) (cadr y))
                                                  (and (= (cadr x) (cadr y)) (< (caddr x) (caddr y))))))))])
         (for/list ([x (in-list sorted)]) (arrive (car x) (list-ref x 3) (list-ref x 4)))))))

(define (run-registry r)
  (run (registry-program r) (registry-steps r)
       #:final-watermark? (hash-ref (registry-options r) 'final-watermark? #t)))

(define (print-run-result res [out (current-output-port)])
  (for ([o (in-list (run-result-outputs res))])
    (fprintf out "@~a ~a: ~a\n" (time->string (output-at o)) (output-node o) (item->string (output-item o)))))

(provide
 (contract-out
  [registry? (-> any/c boolean?)]
  [make-registry (-> registry?)]
  [registry-types (-> registry? list?)]
  [registry-sources (-> registry? list?)]
  [registry-nodes (-> registry? list?)]
  [registry-add-type! (-> registry? event-type? void?)]
  [registry-add-source! (-> registry? source? void?)]
  [registry-add-node! (-> registry? node? void?)]
  [registry-add-setup! (-> registry? list? void?)]
  [registry-set-script! (-> registry? list? void?)]
  [registry-set-option! (-> registry? symbol? any/c void?)]
  [registry-program (-> registry? program?)]
  [registry-steps (-> registry? list?)]
  [run-registry (-> registry? run-result?)]
  [print-run-result (->* (run-result?) (output-port?) void?)]))

;; Resolve a data path relative to the directory of the declaring module.
(define (module-relative-path module-source rel)
  (cond [(absolute-path? rel) rel]
        [(path? module-source)
         (define-values (dir _name _must-be-dir?) (split-path module-source))
         (build-path dir rel)]
        [else rel]))

(provide
 (contract-out
  [module-relative-path (-> any/c path-string? path-string?)]))
