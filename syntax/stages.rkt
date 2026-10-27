#lang racket/base
;; The stage compiler behind define-stream (phase 1).
;;
;; A stream is a pipeline of stages. Each stage compiles to one dataflow
;; node; the last node carries the stream's name and is a sink. The compiler
;; threads a context through the stages: the previous node, the shape of the
;; events flowing (for compile-time attribute checks), and the current key
;; expression (set by key-by and used by stateful stages).
(require racket/list
         syntax/parse
         "static.rkt"
         (for-template racket/base
                       "../private/dataflow.rkt"
                       "../private/operator.rkt"
                       "../private/ops/stateless.rkt"))

(provide (struct-out stage-ctx)
         (struct-out stage-out)
         register-stage!
         compile-stages)

;; prev: identifier of the previous node (#f before the first stage)
;; shape: a shape, key: syntax of a key function expression or #f
(struct stage-ctx (name prev shape key) #:transparent)
;; op: operator expression; inputs: list of input expressions (#f = prev)
(struct stage-out (op inputs shape key) #:transparent)

(define stage-table (make-hasheq))
(define (register-stage! name proc) (hash-set! stage-table name proc))

;; compile-stages : identifier (listof syntax) -> (values definitions final-id shape)
(define (compile-stages name-id stages)
  (define name (syntax-e name-id))
  (define n (length stages))
  (let loop ([stages stages] [i 1] [ctx (stage-ctx name #f #f #f)] [defs '()])
    (define stg (car stages))
    (define head (syntax-parse stg [(h:id . _) (syntax-e #'h)] [_ #f]))
    (define proc (hash-ref stage-table head #f))
    (unless proc
      (raise-syntax-error 'define-stream
                          (format "unknown stage; expected one of ~a"
                                  (sort (hash-keys stage-table) symbol<?))
                          stg))
    (when (and (not (stage-ctx-prev ctx)) (not (eq? head 'from)) (not (memq head '(merge))))
      (raise-syntax-error 'define-stream "a stream must start with (from source-or-stream ...)" stg))
    (define out (proc stg ctx))
    (define last? (= i n))
    (define node-id (if last? (car (generate-temporaries (list name-id)))
                        (car (generate-temporaries (list (format "~a/~a" name i))))))
    (define node-name (if last? name (string->symbol (format "~a/~a" name i))))
    (define inputs (or (stage-out-inputs out) (list (stage-ctx-prev ctx))))
    (define def
      #`(define #,node-id
          (make-node '#,node-name #,(stage-out-op out) (list #,@inputs) #:sink? #,last?)))
    (define ctx2 (stage-ctx name node-id (stage-out-shape out) (stage-out-key out)))
    (if last?
        (values (reverse (cons def defs)) node-id (stage-out-shape out))
        (loop (cdr stages) (add1 i) ctx2 (cons def defs)))))

;; ---------------------------------------------------------------------------
;; (from upstream T ...): read a source or stream, optionally only some types.

(register-stage!
 'from
 (λ (stx ctx)
   (syntax-parse stx
     [(_ up:id t:id ...)
      (define v (lookup-upstream #'up))
      (define up-shape
        (if (source-static? v) (apply type-shape (source-static-types v)) (stream-static-shape v)))
      (for ([t (syntax->list #'(t ...))])
        (lookup-type t stx)
        (unless (memq (syntax-e t) (shape-type-names up-shape))
          (raise-syntax-error 'from (format "~a does not carry events of type ~a; it carries ~a"
                                            (syntax-e #'up) (syntax-e t) (shape-type-names up-shape))
                              stx t)))
      (define selected
        (if (null? (syntax->list #'(t ...)))
            up-shape
            (shape (filter (λ (p) (memq (car p) (map syntax-e (syntax->list #'(t ...)))))
                           (shape-types up-shape)))))
      (stage-out (if (null? (syntax->list #'(t ...)))
                     #'(make-operator 'from)
                     #'(select-types-operator '(t ...)))
                 (list (static-runtime-id v))
                 selected
                 #f)])))
