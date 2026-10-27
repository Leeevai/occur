#lang racket/base
;; Declaration forms of #lang occur.
(require racket/stxparam
         "registry-param.rkt"
         "../private/time.rkt"
         "../private/event.rkt"
         "../private/dataflow.rkt"
         "../private/io.rkt"
         "../private/registry.rkt"
         "../private/ops/structure.rkt"
         "../private/progress.rkt"
         "../private/context.rkt"
         "../private/pattern/ast.rkt"
         "../private/pattern/operators.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse
                     (only-in "../private/scheduler.rkt" [parse-cron parse-cron*])
                     "static.rkt"
                     "stages.rkt"
                     "patterns.rkt"))

(provide event-type
         define-pattern
         directory
         entries
         source
         define-stream
         define-schedule
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
    (pattern ((~datum bounded-delay) d:expr) #:with expr #'(list 'bounded-delay d))
    (pattern ((~datum heartbeat) t:id) #:with expr #'(list 'heartbeat 't))))

(define-syntax (jsonl stx) (raise-syntax-error #f "only allowed in a source's #:from" stx))
(define-syntax (events stx) (raise-syntax-error #f "only allowed in a source's #:from" stx))
(define-syntax (data stx) (raise-syntax-error #f "only allowed in a source's #:from" stx))

(define-syntax (source stx)
  (syntax-parse stx
    [(_ name:id
        (~alt (~once (~seq #:types (t:id ...+)))
              (~optional (~seq #:from from))
              (~optional (~seq #:watermark wm:watermark-strategy))
              (~optional (~seq #:key ent:id))
              (~optional (~seq #:substream sub:id))
              (~optional (~seq #:kind kind:id))
              (~optional (~seq #:merge (~and merge (~datum high-water-mark))))
              (~optional (~seq #:skew-guard skew:expr))
              (~optional (~seq #:directory dir:id)))
        ...)
     (define types (for/list ([t (syntax->list #'(t ...))]) (lookup-type t stx)))
     (define src-shape (apply type-shape types))
     (for ([a (list (attribute ent) (attribute sub) (attribute kind))] #:when a)
       (check-attributes! 'source src-shape #`(#,a) stx))
     (define keying
       (for/hasheq ([k '(entity substream kind)] [a (list (attribute ent) (attribute sub) (attribute kind))]
                    #:when a)
         (values k (syntax-e a))))
     (when (and (attribute merge) (not (attribute sub)))
       (raise-syntax-error 'source "#:merge high-water-mark needs #:substream" stx (attribute merge)))
     (when (attribute dir)
       (unless (attribute ent) (raise-syntax-error 'source "#:directory needs #:key" stx #'dir))
       (unless (directory-static? (lookup-static #'dir))
         (raise-syntax-error 'source "not a declared directory" stx #'dir)))
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
                   [head (format-id #'name "~a-merged" #'name)]
                   [guard (format-id #'name "~a-guarded" #'name)]
                   [gate (format-id #'name "~a-gated" #'name)]
                   [data data-expr]
                   [wm (if (attribute wm) (attribute wm.expr) #''none)]
                   [keying keying]
                   [sub-fn (cond [(and (attribute sub) (attribute ent))
                                  #`(λ (e) (cons (event-attr e '#,(attribute ent)) (event-attr e '#,(attribute sub))))]
                                 [(attribute sub) #`(attribute-key-fn '#,(attribute sub))]
                                 [else #'#f])])
       #`(begin
           (define rt (make-source 'name #:types (list t ...) #:watermark wm #:data data
                                   #:meta (hasheq 'keying 'keying)))
           #,@(if (attribute skew)
                  (list #`(define guard (make-node (string->symbol (format "~a/skew-guard" 'name))
                                                   (skew-guard-operator #,(attribute skew)) (list rt))))
                  '())
           #,@(if (attribute dir)
                  (list #`(define gate (make-node (string->symbol (format "~a/directory" 'name))
                                                  (entity-context-operator
                                                   '#,(directory-static-name (lookup-static #'dir))
                                                   #:key (attribute-key-fn '#,(attribute ent)))
                                                  (list #,(if (attribute skew) #'guard #'rt)))))
                  '())
           #,@(if (attribute merge)
                  (list #`(define head (make-node (string->symbol (format "~a/merge" 'name))
                                                  (monotonic-merge-operator sub-fn)
                                                  (list #,(cond [(attribute dir) #'gate]
                                                                [(attribute skew) #'guard]
                                                                [else #'rt])))))
                  '())
           (define-syntax name (make-source-static 'name (list (syntax-local-value #'t) ...) (quote-syntax rt)
                                                   #:keying 'keying
                                                   #:head #,(cond [(attribute merge) #'(quote-syntax head)]
                                                                  [(attribute dir) #'(quote-syntax gate)]
                                                                  [(attribute skew) #'(quote-syntax guard)]
                                                                  [else #'#f])))
           (registry-add-source! occur-registry rt)))]))

;; ---------------------------------------------------------------------------
;; (define-stream name stage ...+)

(define-syntax (define-stream stx)
  (syntax-parse stx
    [(_ name:id stage ...+)
     (define-values (defs final shp key keying) (compile-stages #'name (syntax->list #'(stage ...))))
     (with-syntax ([(def ...) defs]
                   [final final]
                   [shape-datum (shape-types shp)]
                   [key key]
                   [keying keying])
       #'(begin
           def ...
           (define-syntax name (make-stream-static 'name (shape 'shape-datum) (quote-syntax final)
                                                   #:key (quote-syntax key) #:keying 'keying))
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

;; ---------------------------------------------------------------------------
;; (define-schedule name "cron expression")                       TM.schedule
;; A stream of Tick events at every tick, in processing time, read in UTC.
;; Ticks that came due together fire once with their count in `ticks`.

(define-syntax (define-schedule stx)
  (syntax-parse stx
    [(_ name:id expr:str)
     (with-handlers ([exn:fail? (λ (e) (raise-syntax-error #f (exn-message e) stx #'expr))])
       (parse-cron* (syntax-e #'expr)))
     (with-syntax ([rt (format-id #'name "~a-schedule" #'name)])
       #'(begin
           (define rt (make-node 'name (schedule-operator expr) '() #:sink? #t))
           (define-syntax name (make-stream-static 'name (shape '((Tick (ticks . Integer) (first . Time)
                                                                        (last . Time) (expr . String))))
                                                   (quote-syntax rt)))
           (registry-add-node! occur-registry rt)))]))

;; ---------------------------------------------------------------------------
;; (directory name #:from data)                                       CX.entity
;; data: (jsonl path) with {"id": .., "status": ..} lines, (entries [id status] ...),
;; or (data expr) producing a list of (id . status). The directory is pushed
;; when the run starts; scripts push replacements or patches with
;; (directory-push name entries #:mode replace | patch).

(define-syntax (entries stx) (raise-syntax-error #f "only allowed in a directory's #:from" stx))

(define-syntax (directory stx)
  (syntax-parse stx
    [(_ name:id #:from from)
     (define loader
       (syntax-parse #'from
         [((~literal jsonl) path:str)
          #'(λ () (read-jsonl-directory
                   (module-relative-path (variable-reference->module-source (#%variable-reference)) path)))]
         [((~literal entries) [id:expr status:id] ...) #'(λ () (list (cons id 'status) ...))]
         [((~literal data) e:expr) #'(λ () e)]
         [_ (raise-syntax-error 'directory "expected (jsonl path), (entries [id status] ...), or (data expr)" #'from)]))
     (with-syntax ([rt (format-id #'name "~a-directory" #'name)] [loader loader])
       #'(begin
           (define rt (directory-decl 'name loader))
           (define-syntax name (directory-static 'name (quote-syntax rt)))
           (registry-add-setup! occur-registry (list (directory-push 'name (directory-entries rt))))))]))

;; ---------------------------------------------------------------------------
;; (define-pattern name option ... pattern option ...)                    §11-12
;;
;; options: #:from stream, #:key attr, #:within d, #:clock c, #:selection s,
;; #:consumption c, #:output o, #:skip k, #:eval e, #:engine e, #:where θ,
;; #:emit (Type [attr expr] ...)

(begin-for-syntax
  (define policy-names
    (hasheq 'selection (hasheq 'strict 'strict 'strict-contiguity 'strict
                               'partition 'partition 'partition-contiguity 'partition
                               'next 'next 'skip-till-next-match 'next
                               'any 'any 'skip-till-any-match 'any)
            'consumption (hasheq 'unrestricted 'unrestricted 'recent 'recent 'chronicle 'chronicle
                                 'continuous 'continuous 'cumulative 'cumulative)
            'output (hasheq 'every 'every 'first 'first 'last 'last 'maximal 'maximal)
            'eval (hasheq 'immediate 'immediate 'deferred 'deferred)
            'engine (hasheq 'reference 'reference 'incremental 'incremental)))
  (define (policy-value kind id)
    (define table (hash-ref policy-names kind))
    (or (and (identifier? id) (hash-ref table (syntax-e id) #f))
        (raise-syntax-error 'define-pattern
                            (format "unknown ~a policy; expected one of ~a" kind
                                    (sort (hash-keys table) symbol<?))
                            id)))
  ;; split (kw v ... body kw v ...) into a keyword table and the body
  (define (split-options stx items)
    (let loop ([xs items] [opts (hasheq)] [body #f])
      (cond
        [(null? xs) (unless body (raise-syntax-error 'define-pattern "missing the pattern" stx))
                    (values opts body)]
        [(keyword? (syntax-e (car xs)))
         (when (null? (cdr xs)) (raise-syntax-error 'define-pattern "missing a value for this option" stx (car xs)))
         (define k (syntax-e (car xs)))
         (when (hash-has-key? opts k) (raise-syntax-error 'define-pattern "option given twice" stx (car xs)))
         (loop (cddr xs) (hash-set opts k (cadr xs)) body)]
        [body (raise-syntax-error 'define-pattern "only one pattern is allowed" stx (car xs))]
        [else (loop (cdr xs) opts (car xs))])))
  (define known-options '(#:from #:key #:within #:clock #:selection #:consumption #:output #:skip #:eval
                          #:engine #:where #:emit))
  ;; heads that make a pattern non-monotone or unbounded
  (define (pattern-heads p)
    (syntax-parse p
      [(h:id . rest) (cons (syntax-e #'h) (append-map pattern-heads (syntax->list #'rest)))]
      [_ '()]))
  (define (warn! stx msg)
    (eprintf "~a:~a:~a: warning: ~a\n" (syntax-source stx) (syntax-line stx) (syntax-column stx) msg)))

(define-syntax (define-pattern stx)
  (syntax-parse stx
    [(_ name:id item ...+)
     (define-values (opts body) (split-options stx (syntax->list #'(item ...))))
     (for ([k (in-hash-keys opts)])
       (unless (memq k known-options)
         (raise-syntax-error 'define-pattern (format "unknown option ~a" k) stx)))
     (define up (or (hash-ref opts '#:from #f) (raise-syntax-error 'define-pattern "missing #:from" stx)))
     (define v (lookup-upstream up))
     (define shp (if (source-static? v) (apply type-shape (source-static-types v)) (stream-static-shape v)))
     (define input (or (and (source-static? v) (source-static-head v)) (static-runtime-id v)))
     (define keying (if (source-static? v) (source-static-keying v) (stream-static-keying v)))
     (define key
       (cond [(hash-ref opts '#:key #f)
              => (λ (k) (check-attributes! 'define-pattern shp #`(#,k) stx) #`(attribute-key-fn '#,k))]
             [(and keying (hash-ref keying 'entity #f)) => (λ (a) #`(attribute-key-fn '#,a))]
             [else #'#f]))
     (define-values (ast bindings) (parse-pattern body shp))
     ;; policies
     (define policies
       (for/fold ([h (hasheq)]) ([kind '(selection consumption output eval)])
         (define kw (string->keyword (symbol->string kind)))
         (if (hash-ref opts kw #f) (hash-set h kind (policy-value kind (hash-ref opts kw))) h)))
     (define skip
       (cond
         [(hash-ref opts '#:skip #f)
          => (λ (s) (syntax-parse s
                      [(~datum no-skip) ''no-skip] [(~datum to-next) ''to-next] [(~datum past-last) ''past-last]
                      [((~and kind (~or* (~datum to-first) (~datum to-last))) x:id)
                       (unless (findf (λ (b) (eq? (pbinding-name b) (syntax-e #'x))) bindings)
                         (raise-syntax-error 'define-pattern "the skip target is not a binding of the pattern" stx #'x))
                       (list (syntax-e #'kind) (syntax-e #'x))]
                      [_ (raise-syntax-error 'define-pattern
                                             "unknown skip strategy; expected no-skip, to-next, past-last, (to-first x), or (to-last x)"
                                             s)]))]
         [else #f]))
     (define heads (pattern-heads body))
     (define non-monotone? (ormap (λ (h) (memq h '(absent unless aperiodic aperiodic* periodic periodic*))) heads))
     (when (and (eq? (hash-ref policies 'eval #f) 'immediate)
                (or non-monotone? (memq (hash-ref policies 'output #f) '(last maximal))))
       (raise-syntax-error 'define-pattern
                           "#:eval immediate is valid only for monotone patterns with every or first output (EVAL.immediate)"
                           stx (hash-ref opts '#:eval)))
     (define clock (and (hash-ref opts '#:clock #f) (check-clock! (hash-ref opts '#:clock))))
     (when (and (ormap (λ (h) (memq h '(kleene+ kleene* repeat unless absent))) heads)
                (not (hash-ref opts '#:within #f))
                (not (memq 'within heads)))
       (warn! stx (format "pattern ~a has negation or Kleene closure but no #:within; its state can grow without bound (spec §17)"
                          (syntax-e #'name))))
     (define where-expr
       (cond [(hash-ref opts '#:where #f)
              => (λ (θ) (check-binding-uses! θ bindings stx) (bindings-lambda bindings θ #:lexical θ))]
             [else #f]))
     ;; #:emit (Type [attr expr] ...)
     (define-values (derive out-shape)
       (cond
         [(hash-ref opts '#:emit #f)
          => (λ (e)
               (syntax-parse e
                 [(t:id [a:id x:expr] ...)
                  (define ty (lookup-type #'t e))
                  (define fields (map car (type-static-fields ty)))
                  (for ([a (syntax->list #'(a ...))])
                    (unless (memq (syntax-e a) fields)
                      (raise-syntax-error 'define-pattern (format "unknown attribute ~a for event type ~a" (syntax-e a) (syntax-e #'t)) e a)))
                  (for ([f fields])
                    (unless (memq f (map syntax-e (syntax->list #'(a ...))))
                      (raise-syntax-error 'define-pattern (format "missing attribute ~a for event type ~a" f (syntax-e #'t)) e)))
                  (for ([x (syntax->list #'(x ...))]) (check-binding-uses! x bindings e))
                  (values #`(λ (m) (derive-match m #:type 't #:src 'name
                                                 #:payload #,(bindings-lambda bindings #'(make-immutable-hasheq (list (cons 'a x) ...))
                                                                              #:lexical e)))
                          (type-shape ty))]))]
         [else (values #'(λ (m) (derive-match m #:src 'name))
                       (shape (list (cons 'Match (for/list ([b bindings]) (cons (pbinding-name b) 'Any))))))]))
     (define full-ast
       (let* ([a ast]
              [a (if where-expr #`(where #,a #,where-expr) a)]
              [a (if (hash-ref opts '#:within #f) #`(within #,a #,(hash-ref opts '#:within)) a)])
         a))
     (define engine (if (hash-ref opts '#:engine #f) (policy-value 'engine (hash-ref opts '#:engine)) 'reference))
     (with-syntax ([rt (format-id #'name "~a-pattern" #'name)]
                   [ast full-ast]
                   [key key]
                   [input input]
                   [derive derive]
                   [policies #`(hasheq #,@(append* (for/list ([(k v) (in-hash policies)]) (list #`'#,k #`'#,v)))
                                       #,@(if skip (list #''skip #`'#,skip) '()))]
                   [engine engine]
                   [shape-datum (shape-types out-shape)])
       #'(begin
           (define rt (make-node 'name (pattern-operator ast #:key key #:derive derive #:policies policies
                                                         #:engine 'engine)
                                 (list input) #:sink? #t))
           (define-syntax name (make-stream-static 'name (shape 'shape-datum) (quote-syntax rt)
                                                   #:key (quote-syntax key)))
           (registry-add-node! occur-registry rt)))]))
