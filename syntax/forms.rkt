#lang racket/base
;; Declaration forms of #lang occur.
(require racket/stxparam
         "registry-param.rkt"
         "../private/time.rkt"
         "../private/event.rkt"
         "../private/dataflow.rkt"
         "../private/io.rkt"
         "../private/registry.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse
                     "static.rkt"
                     "stages.rkt"))

(provide event-type
         source
         define-stream
         script
         jsonl events data)

;; ---------------------------------------------------------------------------
;; (event-type Name [attr : Domain] ... #:derived)          ES.type

(define-syntax (event-type stx)
  (syntax-parse stx
    [(_ name:id (~alt (~optional (~and #:derived derived)) [attr:id (~datum :) dom]) ...)
     (define dup (check-duplicates (syntax->list #'(attr ...)) #:key syntax-e))
     (when dup (raise-syntax-error #f "duplicate attribute name" stx dup))
     (define parsed (for/list ([d (syntax->list #'(dom ...))])
                      (call-with-values (λ () (parse-domain d)) cons)))
     (with-syntax ([rt (format-id #'name "~a-type" #'name)]
                   [(dom-expr ...) (map cdr parsed)]
                   [(dom-datum ...) (map car parsed)]
                   [kind (if (attribute derived) 'derived 'primitive)]
                   [derived? (and (attribute derived) #t)])
       #'(begin
           (define rt (make-event-type 'name (list (cons 'attr dom-expr) ...) #:derived? derived?))
           (define-syntax name (type-static 'name '((attr . dom-datum) ...) 'kind (quote-syntax rt)))
           (registry-add-type! occur-registry rt)))]))

;; ---------------------------------------------------------------------------
;; (source name #:types (T ...) #:from data #:watermark strategy)   ES.stream

(begin-for-syntax
  (define-syntax-class watermark-strategy
    #:description "a watermark strategy: none, ascending, or (bounded-delay d)"
    (pattern (~datum none) #:with expr #''none)
    (pattern (~datum ascending) #:with expr #''ascending)
    (pattern ((~datum bounded-delay) d:expr) #:with expr #'(list 'bounded-delay d))))

(define-syntax (jsonl stx) (raise-syntax-error #f "only allowed in a source's #:from" stx))
(define-syntax (events stx) (raise-syntax-error #f "only allowed in a source's #:from" stx))
(define-syntax (data stx) (raise-syntax-error #f "only allowed in a source's #:from" stx))

(define-syntax (source stx)
  (syntax-parse stx
    [(_ name:id
        (~alt (~once (~seq #:types (t:id ...+)))
              (~optional (~seq #:from from))
              (~optional (~seq #:watermark wm:watermark-strategy)))
        ...)
     (define types (for/list ([t (syntax->list #'(t ...))]) (lookup-type t stx)))
     (define type-names (map type-static-name types))
     (define data-expr
       (if (attribute from)
           (syntax-parse #'from
             [((~literal jsonl) path:str)
              #'(λ () (read-jsonl-arrivals
                       (module-relative-path (variable-reference->module-source (#%variable-reference))
                                             path)
                       (list t ...)))]
             [((~literal events) item ...)
              (for ([item (syntax->list #'(item ...))])
                (syntax-parse item
                  [(h:id . _)
                   (define v (lookup-static #'h))
                   (when (and (type-static? v) (not (memq (type-static-name v) type-names)))
                     (raise-syntax-error 'source
                                         (format "type ~a is not declared by source ~a"
                                                 (type-static-name v) (syntax-e #'name))
                                         item #'h))]
                  [_ (void)]))
              #'(λ () (list item ...))]
             [((~literal data) e:expr) #'(λ () e)]
             [_ (raise-syntax-error 'source "expected (jsonl path), (events item ...), or (data expr)"
                                    #'from)])
           #'#f))
     (with-syntax ([rt (format-id #'name "~a-source" #'name)]
                   [data data-expr]
                   [wm (if (attribute wm) (attribute wm.expr) #''none)])
       #'(begin
           (define rt (make-source 'name #:types (list t ...) #:watermark wm #:data data))
           (define-syntax name (source-static 'name (list (syntax-local-value #'t) ...) (quote-syntax rt)))
           (registry-add-source! occur-registry rt)))]))

;; ---------------------------------------------------------------------------
;; (define-stream name stage ...+)

(define-syntax (define-stream stx)
  (syntax-parse stx
    [(_ name:id stage ...+)
     (define-values (defs final shp) (compile-stages #'name (syntax->list #'(stage ...))))
     (with-syntax ([(def ...) defs]
                   [final final]
                   [shape-datum (shape-types shp)])
       #'(begin
           def ...
           (define-syntax name (stream-static 'name (shape 'shape-datum) (quote-syntax final)))
           (registry-add-node! occur-registry final)))]))

;; ---------------------------------------------------------------------------
;; (script step ...): an explicit script replacing the sources' data.
;;   (at t source event)   (advance-to t)   (watermark source t)   (suspend t1 t2)
;;   or any expression producing a runtime step.

(define-syntax (script stx)
  (define (step s)
    (syntax-parse s
      [((~datum at) t:expr src:id e:expr)
       (lookup-source #'src)
       #'(arrive t 'src e)]
      [((~datum advance-to) t:expr) #'(advance-to t)]
      [((~datum watermark) src:id t:expr)
       (lookup-source #'src)
       #'(watermark-to 'src t)]
      [((~datum suspend) t1:expr t2:expr) #'(suspend t1 t2)]
      [e:expr #'e]))
  (syntax-parse stx
    [(_ s ...)
     (with-syntax ([(st ...) (map step (syntax->list #'(s ...)))])
       #'(registry-set-script! occur-registry (list st ...)))]))
