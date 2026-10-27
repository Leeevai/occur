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
                       "../private/event.rkt"
                       "../private/dataflow.rkt"
                       "../private/operator.rkt"
                       "../private/ops/stateless.rkt"
                       "../private/registry.rkt"
                       "registry-param.rkt"))

(provide stage-out*
         (struct-out stage-ctx)
         (struct-out stage-out)
         register-stage!
         compile-stages
         event-lambda
         check-attributes!
         shape-with-fields)

;; prev: identifier of the previous node (#f before the first stage)
;; shape: a shape, key: syntax of a key function expression or #f
(struct stage-ctx (name prev shape key) #:transparent)
;; op: operator expression; inputs: list of input expressions (#f = prev)
;; extras: #f or (node-id -> list of definitions) spliced after the node
;; no-sink?: #t when the stream's own node must not be a sink (route)
(struct stage-out (op inputs shape key [extras #:auto] [no-sink? #:auto])
  #:auto-value #f #:transparent #:mutable)

(define (stage-out* op inputs shape key #:extras [extras #f] #:sink? [sink? #t])
  (define o (stage-out op inputs shape key))
  (set-stage-out-extras! o extras)
  (set-stage-out-no-sink?! o (not sink?))
  o)

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
    (define sink? (and last? (not (stage-out-no-sink? out))))
    (define def
      #`(define #,node-id
          (make-node '#,node-name #,(stage-out-op out) (list #,@inputs) #:sink? #,sink?)))
    (define extras (if (stage-out-extras out) ((stage-out-extras out) node-id) '()))
    (when (and (stage-out-extras out) (not last?))
      (raise-syntax-error 'define-stream "this stage must be the last stage of a stream" stg))
    (define ctx2 (stage-ctx name node-id (stage-out-shape out) (stage-out-key out)))
    (if last?
        (values (append (reverse (cons def defs)) extras) node-id (stage-out-shape out))
        (loop (cdr stages) (add1 i) ctx2 (cons def defs)))))

;; ---------------------------------------------------------------------------
;; Stage expressions see the attributes of the current event as variables
;; (and the event itself as `it`). With a single-type shape the attributes
;; are read strictly; with several types, an absent attribute reads as #f.

;; `lexical` supplies the lexical context of the variables: the user's stage
;; syntax when the body is assembled by the stage compiler.
(define (event-lambda shp body #:lexical [lexical body])
  (define e (car (generate-temporaries '(e))))
  (define single? (shape-single shp))
  (define binds
    (for/list ([a (shape-attributes shp)])
      #`[#,(datum->syntax lexical a) #,(if single?
                                           #`(event-attr #,e '#,a)
                                           #`(event-attr/default #,e '#,a #f))]))
  #`(λ (#,e) (let* ([#,(datum->syntax lexical 'it) #,e] #,@binds) #,body)))

;; Every attribute must be carried by every type of the shape.
(define (check-attributes! who shp ids stx)
  (for ([a (syntax->list ids)])
    (for ([t (shape-types shp)])
      (unless (assq (syntax-e a) (cdr t))
        (raise-syntax-error who (format "unknown attribute ~a: ~a events carry ~a"
                                        (syntax-e a) (car t) (map car (cdr t)))
                            stx a)))))

;; Add or replace fields in every type of a shape.
(define (shape-with-fields shp fields)
  (shape (for/list ([t (shape-types shp)])
           (cons (car t)
                 (append (filter (λ (f) (not (assq (car f) fields))) (cdr t)) fields)))))

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

;; ---------------------------------------------------------------------------
;; (filter expr)                                                    OP.filter

(register-stage!
 'filter
 (λ (stx ctx)
   (syntax-parse stx
     [(_ pred:expr)
      (stage-out* #`(filter-operator #,(event-lambda (stage-ctx-shape ctx) #'pred))
                  #f (stage-ctx-shape ctx) (stage-ctx-key ctx))])))

;; ---------------------------------------------------------------------------
;; (project a ... maybe-as)                                        OP.project

(register-stage!
 'project
 (λ (stx ctx)
   (syntax-parse stx
     [(_ a:id ...+ (~optional (~seq #:as t:id)))
      (define shp (stage-ctx-shape ctx))
      (check-attributes! 'project shp #'(a ...) stx)
      (define names (map syntax-e (syntax->list #'(a ...))))
      (stage-out* #`(project-operator '(a ...) #:type #,(if (attribute t) #''t #'#f))
                  #f
                  (shape (for/list ([ty (shape-types shp)])
                           (cons (if (attribute t) (syntax-e #'t) (car ty))
                                 (filter (λ (f) (memq (car f) names)) (cdr ty)))))
                  (stage-ctx-key ctx))])))

;; ---------------------------------------------------------------------------
;; (map T [a expr] ... maybe-time) or (map [a expr] ... maybe-time)    OP.map


(register-stage!
 'map
 (λ (stx ctx)
   (define shp (stage-ctx-shape ctx))
   (define (time-args o-ts o-te o-n)
     (append (if o-ts (list #'#:ts (event-lambda shp o-ts)) '())
             (if o-te (list #'#:te (event-lambda shp o-te)) '())
             (if o-n (list #'#:n (event-lambda shp o-n)) '())))
   (syntax-parse stx
     [(_ t:id (~alt (~optional (~seq #:ts ts:expr)) (~optional (~seq #:te te:expr))
                    (~optional (~seq #:n n:expr)) [a:id v:expr])
         ...)
      #:when (type-static? (lookup-static #'t))
      (define ty (lookup-type #'t stx))
      (check-fields! 'map ty #'(a ...) stx)
      (stage-out* #`(map-operator #,(event-lambda shp #'(list (cons 'a v) ...) #:lexical stx)
                                  #:type 't
                                  #,@(time-args (attribute ts) (attribute te) (attribute n)))
                  #f (type-shape ty) (stage-ctx-key ctx))]
     [(_ (~alt (~optional (~seq #:ts ts:expr)) (~optional (~seq #:te te:expr))
               (~optional (~seq #:n n:expr)) [a:id v:expr])
         ...)
      #:fail-when (null? (syntax->list #'(a ...))) "expected at least one [attribute expr] clause"
      (stage-out* #`(map-operator #,(event-lambda shp #`(let ([p (event-payload #,(datum->syntax stx 'it))])
                                                          (for/fold ([p p]) ([k (list 'a ...)] [x (list v ...)])
                                                            (hash-set p k x)))
                                               #:lexical stx)
                                  #,@(time-args (attribute ts) (attribute te) (attribute n)))
                  #f
                  (shape-with-fields shp (for/list ([a (syntax->list #'(a ...))]) (cons (syntax-e a) 'Any)))
                  (stage-ctx-key ctx))])))

;; All of a type's attributes must be given, and no others.
(define (check-fields! who ty ids stx)
  (define fields (type-static-fields ty))
  (define names (map syntax-e (syntax->list ids)))
  (for ([a (syntax->list ids)])
    (unless (assq (syntax-e a) fields)
      (raise-syntax-error who (format "unknown attribute ~a for event type ~a; its attributes are ~a"
                                      (syntax-e a) (type-static-name ty) (map car fields))
                          stx a)))
  (define dup (check-duplicates (syntax->list ids) #:key syntax-e))
  (when dup (raise-syntax-error who "attribute given twice" stx dup))
  (for ([f fields])
    (unless (memq (car f) names)
      (raise-syntax-error who (format "missing attribute ~a for event type ~a" (car f) (type-static-name ty))
                          stx))))

;; ---------------------------------------------------------------------------
;; (enrich #:key expr #:with lookup #:adds ([a : D] ...) maybe-missing)  OP.enrich

(register-stage!
 'enrich
 (λ (stx ctx)
   (define shp (stage-ctx-shape ctx))
   (syntax-parse stx
     [(_ (~alt (~once (~seq #:key k:expr))
               (~once (~seq #:with lookup:expr))
               (~optional (~seq #:adds ([a:id (~datum :) d] ...)))
               (~optional (~seq #:missing (~and m (~or* (~datum keep) (~datum drop))))))
         ...)
      (define added (for/list ([a (or (attribute a) '())] [d (or (attribute d) '())])
                      (define-values (datum _expr) (parse-domain d))
                      (cons (syntax-e a) datum)))
      (stage-out* #`(enrich-operator (as-lookup lookup) #:key #,(event-lambda shp #'k)
                                     #:missing '#,(if (attribute m) (syntax-e #'m) 'keep))
                  #f (shape-with-fields shp added) (stage-ctx-key ctx))])))

;; ---------------------------------------------------------------------------
;; (split T #:for (clause ...) [a expr] ... #:ts e maybe-te maybe-n)   OP.split

(register-stage!
 'split
 (λ (stx ctx)
   (define shp (stage-ctx-shape ctx))
   (syntax-parse stx
     [(_ t:id (~alt (~once (~seq #:for (clause ...)))
                    (~optional (~seq #:ts ts0:expr)) (~optional (~seq #:te te0:expr))
                    (~optional (~seq #:n n0:expr)) [a:id v:expr])
         ...)
      (define ty (lookup-type #'t stx))
      (check-fields! 'split ty #'(a ...) stx)
      (unless (attribute ts0) (raise-syntax-error 'split "each child needs #:ts" stx))
      (with-syntax ([ts (attribute ts0)]
                    [te (or (attribute te0) (attribute ts0))]
                    [n (or (attribute n0) #'1)])
        (stage-out* #`(split-operator
                       #,(event-lambda shp #'(for/list (clause ...)
                                               (make-event 't (list (cons 'a v) ...) #:ts ts #:te te #:n n))
                                       #:lexical stx))
                    #f (type-shape ty) (stage-ctx-key ctx)))])))

;; ---------------------------------------------------------------------------
;; (route maybe-mode [branch pred] ... maybe-otherwise)              OP.route
;; The last stage; each branch becomes a stream of its own.

(register-stage!
 'route
 (λ (stx ctx)
   (define shp (stage-ctx-shape ctx))
   (syntax-parse stx
     [(_ (~optional (~seq #:mode (~and mode (~or* (~datum exclusive) (~datum inclusive)))))
         [b:id p:expr] ...+
         (~optional (~seq #:otherwise other:id)))
      (define branches (append (syntax->list #'(b ...)) (if (attribute other) (list #'other) '())))
      (stage-out* #`(route-operator (list #,@(for/list ([p (syntax->list #'(p ...))]) (event-lambda shp p)))
                                    #:mode '#,(if (attribute mode) (syntax-e #'mode) 'exclusive)
                                    #:otherwise? #,(and (attribute other) #t))
                  #f shp (stage-ctx-key ctx)
                  #:sink? #f
                  #:extras
                  (λ (node-id)
                    (for/list ([br branches] [port (in-naturals)])
                      (define br-node (car (generate-temporaries (list br))))
                      #`(begin
                          (define #,br-node
                            (make-node '#,br (make-operator '#,br) (list (cons #,node-id #,port)) #:sink? #t))
                          (define-syntax #,br (stream-static '#,br (shape '#,(shape-types shp))
                                                             (quote-syntax #,br-node)))
                          (registry-add-node! occur-registry #,br-node)))))])))
