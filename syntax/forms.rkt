#lang racket/base
;; Declaration forms of #lang occur.
(require racket/stxparam
         racket/list
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
         "../private/derive.rkt"
         "../private/rules.rkt"
         "../private/rules-engine.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse
                     (only-in "../private/scheduler.rkt" [parse-cron parse-cron*])
                     "static.rkt"
                     "stages.rkt"
                     "patterns.rkt"))

(provide event-type
         define-condition
         rule-template
         rule-source
         bind
         pt:accumulate pt:window-close pt:silence pt:schedule
         define-pattern
         define-view
         act
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
           ;; a type is a value of the module, not part of its program, so a
           ;; module of types alone stays a library
           (define-syntax name (type-static 'name '((attr . dom-datum) ...) 'kind (quote-syntax rt)))))]))

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
                                                  (monotonic-merge-operator
                                                   sub-fn #:entity #,(if (attribute ent) #`(attribute-key-fn '#,(attribute ent)) #'#f))
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
    [(_ name:id (~optional (~seq #:recursive bound:exact-positive-integer)) stage ...+)
     (define-values (defs final shp key keying)
       (parameterize ([current-recursive-bound (and (attribute bound) (syntax-e #'bound))])
         (compile-stages #'name (syntax->list #'(stage ...)))))
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
;; (script option ... step ...): an explicit script replacing the sources' data.
;;   (at t source event)   (advance-to t)   (watermark source t)   (suspend t1 t2)
;;   or any expression producing a runtime step.
;; options: #:start t, the processing time the simulated clock starts at (the
;; declarations' setup steps run then), and #:final-watermark? b, whether
;; the run ends by moving every watermark to +∞ (the default).

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
    [(_ (~alt (~optional (~seq #:start t0:expr)) (~optional (~seq #:final-watermark? fw:expr))) ... s ...)
     (with-syntax ([(st ...) (map step (syntax->list #'(s ...)))])
       #`(begin
           #,@(if (attribute t0) (list #'(registry-set-option! occur-registry 'start t0)) '())
           #,@(if (attribute fw) (list #'(registry-set-option! occur-registry 'final-watermark? fw)) '())
           (registry-set-script! occur-registry (list st ...))))]))

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
                          #:engine #:where #:emit #:timeout-partial #:retain))
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
                      [(~datum no-skip) 'no-skip] [(~datum to-next) 'to-next] [(~datum past-last) 'past-last]
                      [((~and kind (~or* (~datum to-first) (~datum to-last))) x:id)
                       (unless (findf (λ (b) (eq? (pbinding-name b) (syntax-e #'x))) bindings)
                         (raise-syntax-error 'define-pattern "the skip target is not a binding of the pattern" stx #'x))
                       (list (syntax-e #'kind) (syntax-e #'x))]
                      [_ (raise-syntax-error 'define-pattern
                                             "unknown skip strategy; expected no-skip, to-next, past-last, (to-first x), or (to-last x)"
                                             s)]))]
         [else #f]))
     ;; #:timeout-partial (d clock): timed-out partial matches; #:retain (reason ...)
     (define timeout
       (cond [(hash-ref opts '#:timeout-partial #f)
              => (λ (t) (syntax-parse t
                          [(d:expr clk:id) #`(list d '#,(check-clock! #'clk '(event-time processing-time)))]
                          [_ (raise-syntax-error 'define-pattern "expected (duration clock)" t)]))]
             [else #f]))
     (when (and timeout (not (hash-ref opts '#:where #f)))
       (raise-syntax-error 'define-pattern "#:timeout-partial needs a #:where clause: its partial matches are those it has not accepted" stx))
     (define retain
       (cond [(hash-ref opts '#:retain #f)
              => (λ (r) (syntax-parse r
                          [(x:id ...) (for ([x (syntax->list #'(x ...))])
                                        (unless (memq (syntax-e x) '(match timeout))
                                          (raise-syntax-error 'define-pattern "unknown output reason; expected match or timeout" r x)))
                                      #''(x ...)]))]
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
     ;; last and maximal output without a bound are final only at the end of the stream
     (define top-within? (syntax-parse body [((~datum within) . _) #t] [_ #f]))
     (when (and (memq (hash-ref policies 'output #f) '(last maximal))
                (not (hash-ref opts '#:within #f)) (not top-within?))
       (warn! stx (format "pattern ~a has ~a output but no #:within; its output waits for the end of the stream (spec §12.3)"
                          (syntax-e #'name) (hash-ref policies 'output))))
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
     (define engine (if (hash-ref opts '#:engine #f) (policy-value 'engine (hash-ref opts '#:engine)) 'incremental))
     (with-syntax ([rt (format-id #'name "~a-pattern" #'name)]
                   [ast full-ast]
                   [key key]
                   [input input]
                   [derive derive]
                   [policies #`(hasheq #,@(append* (for/list ([(k v) (in-hash policies)]) (list #`'#,k #`'#,v)))
                                       #,@(if skip (list #''skip #`'#,skip) '())
                                       #,@(if timeout (list #''timeout-partial timeout) '())
                                       #,@(if retain (list #''retain retain) '()))]
                   [engine engine]
                   [shape-datum (shape-types out-shape)])
       #'(begin
           (define rt (make-node 'name (pattern-operator ast #:key key #:derive derive #:policies policies
                                                         #:engine 'engine)
                                 (list input) #:sink? #t))
           (define-syntax name (make-stream-static 'name (shape 'shape-datum) (quote-syntax rt)
                                                   #:key (quote-syntax key)))
           (registry-add-node! occur-registry rt)))]))

;; ---------------------------------------------------------------------------
;; (define-view name #:from stream maybe-key #:init expr #:update expr
;;              maybe-project maybe-export)                              DR.view
;; #:update sees `state`, `now`, `it`, and the event's attributes. With
;; #:project expr, what is exported is expr, which sees `state`, `now`,
;; `last-arrival` (the processing time of the key's last event), and
;; `newest` (its newest end time), as a health classification needs. The
;; view is queried with (view-ref result 'name key) and, with
;; #:export-every d, emits a View event per key every d of processing time.

(define-syntax (define-view stx)
  (syntax-parse stx
    [(_ name:id (~alt (~once (~seq #:from up:id)) (~optional (~seq #:key k:id))
                      (~once (~seq #:init init:expr)) (~once (~seq #:update upd:expr))
                      (~optional (~seq #:project proj:expr))
                      (~optional (~seq #:export-every d:expr)))
        ...)
     (define v (lookup-upstream #'up))
     (define shp (if (source-static? v) (apply type-shape (source-static-types v)) (stream-static-shape v)))
     (define input (or (and (source-static? v) (source-static-head v)) (static-runtime-id v)))
     (define keying (if (source-static? v) (source-static-keying v) (stream-static-keying v)))
     (define key (cond [(attribute k) (check-attributes! 'define-view shp #'(k) stx) #'(attribute-key-fn 'k)]
                       [(and keying (hash-ref keying 'entity #f)) => (λ (a) #`(attribute-key-fn '#,a))]
                       [else #'#f]))
     (with-syntax ([rt (format-id #'name "~a-view" #'name)]
                   [update (let ([st (datum->syntax #'upd 'state)] [now (datum->syntax #'upd 'now)])
                             #`(λ (#,st e #,now) (#,(event-lambda shp #'upd) e)))]
                   [project (if (attribute proj)
                                (with-syntax ([(st nw la nwst) (for/list ([x '(state now last-arrival newest)])
                                                                 (datum->syntax #'proj x))])
                                  #'(λ (st nw la nwst) proj))
                                #'(λ (s now arrival newest) s))]
                   [key key] [input input]
                   [export (or (attribute d) #'#f)])
       #'(begin
           (define rt (make-node 'name (view-operator #:key key #:init (λ () init) #:update update
                                                      #:project project #:export-every export)
                                 (list input) #:sink? #t))
           (define-syntax name (make-stream-static 'name (shape '((View (key . Any) (view . Any))))
                                                   (quote-syntax rt) #:key (quote-syntax key)))
           (registry-add-node! occur-registry rt)))]))

;; ---------------------------------------------------------------------------
;; (act name #:on stream #:do expr maybe-completion)                        DR.act
;; Runs expr (seeing the event's attributes and it) for every event. With
;; #:completion Type, emits a Type event with the result for each action.

(define-syntax (act stx)
  (syntax-parse stx
    [(_ name:id (~alt (~once (~seq #:on up:id)) (~once (~seq #:do body:expr))
                      (~optional (~seq #:completion t:id)))
        ...)
     (define v (lookup-upstream #'up))
     (define shp (if (source-static? v) (apply type-shape (source-static-types v)) (stream-static-shape v)))
     (define input (or (and (source-static? v) (source-static-head v)) (static-runtime-id v)))
     (with-syntax ([rt (format-id #'name "~a-action" #'name)]
                   [proc (event-lambda shp #'body)]
                   [input input]
                   [completion (if (attribute t) #''t #'#f)])
       #'(begin
           (define rt (make-node 'name (action-operator proc #:completion completion) (list input) #:sink? #t))
           (define-syntax name (make-stream-static 'name (shape '((ActionCompleted (result . Any)))) (quote-syntax rt)))
           (registry-add-node! occur-registry rt)))]))

;; ---------------------------------------------------------------------------
;; (define-condition name #:kind kind #:parameters ([#:kw param default maybe-override] ...)
;;                   maybe-positional)
;; (define-condition name #:as other #:rename ([#:kw #:other-kw maybe-convert] ...))  RL.template
;;
;; A condition form builds a rule condition. Its parameters are keywords,
;; checked at compile time; those marked #:override are the numeric
;; parameters a binding may override (RL.binding). A library defines its
;; vocabulary with #:as, renaming the keywords of another condition form;
;; a rename may convert the value with a procedure (not for overridable
;; parameters, which bindings override unconverted).

(begin-for-syntax
  (define (condition-expander name kind params positional)
    (λ (stx)
      (syntax-parse stx
        [(_ (~optional (~and pos (~not _:keyword))) (~seq kw:keyword v:expr) ...)
         (define given (map syntax-e (syntax->list #'(kw ...))))
         (for ([k (syntax->list #'(kw ...))])
           (unless (for/or ([p params]) (eq? (vector-ref p 0) (syntax-e k)))
             (raise-syntax-error name (format "unknown parameter ~a; expected ~a" (syntax-e k)
                                              (map (λ (p) (vector-ref p 0)) params))
                                 stx k)))
         (when (and (attribute pos) (not positional))
           (raise-syntax-error name "this condition takes no positional argument" stx #'pos))
         (define values-by-param
           (for/list ([p params])
             (define i (index-of given (vector-ref p 0)))
             (cons (vector-ref p 1)
                   (cond [i (list-ref (syntax->list #'(v ...)) i)]
                         [(and positional (eq? positional (vector-ref p 1)) (attribute pos)) #'pos]
                         [(vector-ref p 2) (vector-ref p 2)]
                         [else (raise-syntax-error name (format "missing parameter ~a" (vector-ref p 0)) stx)]))))
         #`(condition '#,kind (hasheq #,@(append* (for/list ([pv values-by-param]) (list #`'#,(car pv) (cdr pv))))))]
        [_ (raise-syntax-error name "expected keyword parameters" stx)]))))

(define-syntax (define-condition stx)
  (syntax-parse stx
    [(_ name:id #:kind kind:id #:parameters ([kw:keyword param:id (~optional default:expr) (~optional (~and o #:override))] ...)
        (~optional (~seq #:positional pos:id)))
     (define params (for/list ([k (syntax->list #'(kw ...))] [p (syntax->list #'(param ...))]
                               [d (attribute default)] [o (attribute o)])
                      ;; a default is kept wrapped in a list, so that #f is a default too
                      (vector (syntax-e k) (syntax-e p) (and d (list (syntax->datum d))) (and o #t))))
     (with-syntax ([params-datum params] [positional (and (attribute pos) (syntax-e #'pos))])
       #'(define-syntax name
           (let ([ps (map (λ (v) (vector (vector-ref v 0) (vector-ref v 1)
                                         (and (vector-ref v 2) (datum->syntax #'name (car (vector-ref v 2))))
                                         (vector-ref v 3)))
                          'params-datum)])
             (condition-static 'name 'kind ps 'positional
                               (condition-expander 'name 'kind ps 'positional)))))]
    [(_ name:id #:as other:id #:rename ([kw:keyword okw:keyword (~optional conv:expr)] ...))
     (define target (lookup-static #'other))
     (unless (condition-static? target) (raise-syntax-error 'define-condition "not a condition" stx #'other))
     (for ([o (syntax->list #'(okw ...))])
       (unless (for/or ([p (condition-static-params target)]) (eq? (vector-ref p 0) (syntax-e o)))
         (raise-syntax-error 'define-condition (format "~a has no parameter ~a" (syntax-e #'other) (syntax-e o)) stx o)))
     (for ([k (syntax->list #'(kw ...))] [o (syntax->list #'(okw ...))] [c (attribute conv)] #:when c)
       (when (for/or ([p (condition-static-params target)]) (and (eq? (vector-ref p 0) (syntax-e o)) (vector-ref p 3)))
         (raise-syntax-error 'define-condition "an overridable parameter cannot be converted: bindings override it unconverted"
                             stx k)))
     (with-syntax ([renames (map cons (map syntax-e (syntax->list #'(kw ...))) (map syntax-e (syntax->list #'(okw ...))))]
                   [(cv ...) (for/list ([c (attribute conv)]) (if c #`(quote-syntax #,c) #'#f))])
       #'(define-syntax name
           (let* ([t (syntax-local-value #'other)]
                  [rn 'renames]
                  [convs (list cv ...)]
                  [ps (for/list ([r rn])
                        (define tp (for/first ([p (condition-static-params t)] #:when (eq? (vector-ref p 0) (cdr r))) p))
                        (vector (car r) (vector-ref tp 1) (vector-ref tp 2) (vector-ref tp 3)))])
             (condition-static 'name (condition-static-kind t) ps (condition-static-positional t)
                               (λ (use)
                                 ;; rename the keywords, convert the values that have a
                                 ;; conversion, and expand as the target condition
                                 (syntax-case use ()
                                   [(_ . args)
                                    (let loop ([xs (syntax->list #'args)] [acc '()])
                                      (cond
                                        [(null? xs) (datum->syntax use (cons (quote-syntax other) (reverse acc)) use)]
                                        [(keyword? (syntax-e (car xs)))
                                         (define m (assq (syntax-e (car xs)) rn))
                                         (unless m (raise-syntax-error 'name (format "unknown parameter ~a; expected ~a"
                                                                                    (syntax-e (car xs)) (map car rn))
                                                                       use (car xs)))
                                         (define c (list-ref convs (index-of rn m)))
                                         (define v (if c (datum->syntax use (list c (cadr xs)) (cadr xs)) (cadr xs)))
                                         (loop (cddr xs) (list* v (datum->syntax (car xs) (cdr m)) acc))]
                                        [else (loop (cdr xs) (cons (car xs) acc))]))]))))))]))

;; The core conditions, named after their spec IDs.
(define-condition pt:accumulate #:kind accumulate
  #:parameters ([#:count count 1 #:override] [#:span span 0 #:override]))
(define-condition pt:window-close #:kind window-close
  #:parameters ([#:size size] [#:offset offset 0] [#:mode mode 'all]))
(define-condition pt:silence #:kind silence
  #:parameters ([#:after after #:override] [#:recovery recovery #f]))
(define-condition pt:schedule #:kind schedule
  #:parameters ([#:cron cron]) #:positional cron)

;; ---------------------------------------------------------------------------
;; (rule-template id #:condition condition option ...)                    RL.template
;; options: #:watch (kind ...), #:rate-limit (d drop|defer), #:flush d,
;; #:retain (max_delay), #:target e, #:params ([k v] ...),
;; #:applies-to all | none | (key ...), #:on-update continue | restart,
;; #:id e (the rule id, by default the name), #:in-source.
;; Templates are in force from the start (the module's `templates` source),
;; except with #:in-source: such a template is in force only while a rule
;; source that lists it is. Two templates with one #:id are two versions of
;; one rule, which a script switches between by pushing a source.

(define-syntax (rule-template stx)
  (syntax-parse stx
    [(_ name:id (~alt (~once (~seq #:condition cnd:expr))
                      (~optional (~seq #:watch (w:expr ...)))
                      (~optional (~seq #:rate-limit (rl:expr (~optional (~and mode (~or* (~datum drop) (~datum defer)))))))
                      (~optional (~seq #:flush fl:expr))
                      (~optional (~seq #:retain (rt:id ...)))
                      (~optional (~seq #:target tg:expr))
                      (~optional (~seq #:params ([pk:id pv:expr] ...)))
                      (~optional (~seq #:applies-to (~or* (~and all-kw (~datum all)) (~and none-kw (~datum none)) (ak:expr ...))))
                      (~optional (~seq #:on-update (~and ou (~or* (~datum continue) (~datum restart)))))
                      (~optional (~seq #:id rid:expr))
                      (~optional (~and #:in-source in-source)))
        ...)
     (define cstatic (syntax-parse #'cnd [(h:id . _) (let ([v (lookup-static #'h)]) (and (condition-static? v) v))] [_ #f]))
     (for ([r (or (attribute rt) '())])
       (unless (memq (syntax-e r) '(max_delay)) (raise-syntax-error 'rule-template "only max_delay outputs can be retained" stx r)))
     (with-syntax ([rt-id (format-id #'name "~a-rule" #'name)]
                   [mods #`(hasheq #,@(if (attribute rl) (list #''rate-limit #`(cons #,(attribute rl) '#,(if (attribute mode) (syntax-e #'mode) 'defer))) '())
                                   #,@(if (attribute fl) (list #''flush (attribute fl)) '())
                                   #,@(if (attribute rt) (list #''flush-retain? #'#t) '()))]
                   [watch (if (attribute w) #'(list w ...) #'#f)]
                   [params (if (attribute pk) #'(list (cons 'pk pv) ...) #''())]
                   [applies (cond [(attribute none-kw) #''none] [(attribute ak) #'(list ak ...)] [else #''all])]
                   [on-update (if (attribute ou) #`'#,(syntax-e #'ou) #''restart)]
                   [target (or (attribute tg) #'#f)]
                   [cstatic-id (and cstatic (car (syntax->list (syntax-parse #'cnd [(h . _) #'(h)]))))])
       #`(begin
           (define rt-id (make-rule #,(or (attribute rid) #'(symbol->string 'name))
                                    #:condition cnd #:watch watch #:modifiers mods
                                    #:target target #:params params #:applies-to applies #:on-update on-update))
           (define-syntax name (template-static 'name (symbol->string 'name)
                                                #,(if cstatic #'(syntax-local-value (quote-syntax cstatic-id)) #'#f)
                                                (quote-syntax rt-id)))
           #,@(if (attribute in-source)
                  '()
                  (list #'(registry-add-setup!
                           occur-registry
                           (list (control (rule-source-msg (list 'templates (variable-reference->module-source (#%variable-reference)) 'name)
                                                           (list rt-id) 'replace)
                                          #:config? #t)))))))]))

;; ---------------------------------------------------------------------------
;; (rule-source name maybe-at-start item ...)                      RL.source
;; items: template ids, (bind template #:on key option ...), or (jsonl path)
;; The source is pushed at the start, unless #:at-start? is #f; a script
;; pushes it with (rule-source-push 'source-name name), replacing whatever
;; source of that name is in force.
;; (bind template #:on key maybe-also maybe-key maybe-watch override ... maybe-params)
;; Overrides are the condition's overridable keywords, checked at compile time.

(define-syntax (bind stx) (raise-syntax-error #f "only allowed in a rule-source" stx))

(begin-for-syntax
  (define (compile-bind b)
    (syntax-parse b
      [(_ t:id (~seq kw:keyword v:expr) ...)
       (define ts (lookup-static #'t))
       (unless (template-static? ts) (raise-syntax-error 'bind "not a rule template" b #'t))
       (define cs (template-static-condition ts))
       (define overridable (if cs (condition-overridable cs) '()))
       (define-values (on also bkey watch params overrides)
         (for/fold ([on #f] [also #f] [bkey #f] [watch #f] [params #f] [overrides '()])
                   ([k (syntax->list #'(kw ...))] [x (syntax->list #'(v ...))])
           (case (syntax-e k)
             [(#:on) (values x also bkey watch params overrides)]
             [(#:also) (values on x bkey watch params overrides)]
             [(#:key) (values on also x watch params overrides)]
             [(#:watch) (values on also bkey x params overrides)]
             [(#:params) (values on also bkey watch x overrides)]
             [else
              (unless (memq (syntax-e k) overridable)
                (raise-syntax-error 'bind
                                    (format "~a does not fit ~a's condition~a; overridable: ~a"
                                            (syntax-e k) (syntax-e #'t)
                                            (if cs (format " (~a)" (condition-static-name cs)) "")
                                            overridable)
                                    b k))
              (define param (for/first ([p (condition-static-params cs)] #:when (eq? (vector-ref p 0) (syntax-e k)))
                              (vector-ref p 1)))
              (values on also bkey watch params (cons (cons param x) overrides))])))
       (unless on (raise-syntax-error 'bind "a binding needs #:on key" b))
       #`(make-binding #,(template-static-rule-id ts)
                       #:on (cons #,on #,(if also (syntax-parse also [(k ...) #'(list k ...)] [e #'e]) #''()))
                       #:key #,(or bkey #'"")
                       #:watch #,(if watch (syntax-parse watch [(k ...) #'(list k ...)]) #'#f)
                       #:overrides (list #,@(for/list ([o (reverse overrides)]) #`(cons '#,(car o) #,(cdr o))))
                       #:params #,(if params (syntax-parse params [([pk:id pv:expr] ...) #'(list (cons 'pk pv) ...)]) #''()))])))

(define-syntax (rule-source stx)
  (syntax-parse stx
    [(_ name:id (~optional (~seq #:at-start? at-start:expr)) item ...)
     (define items
       (for/list ([it (syntax->list #'(item ...))])
         (syntax-parse it
           #:literals (bind jsonl)
           [(bind . _) (compile-bind it)]
           [(jsonl path:str)
            #`(read-jsonl-rule-items
               (module-relative-path (variable-reference->module-source (#%variable-reference)) path))]
           [t:id #:when (template-static? (lookup-static #'t)) (template-static-runtime-id (lookup-static #'t))]
           [_ (raise-syntax-error 'rule-source "expected a template, (bind …), or (jsonl path)" stx it)])))
     ;; the source's name is bound to its items, so a script can push it, or
     ;; push it again; with #:at-start? #f only a script pushes it
     #`(begin
         (define name (flatten-items (list #,@items)))
         (when #,(if (attribute at-start) #'at-start #'#t)
           (registry-add-setup! occur-registry
                                (list (control (rule-source-msg 'name name 'replace) #:config? #t)))))]))

(define (flatten-items xs) (append* (map (λ (x) (if (list? x) x (list x))) xs)))
