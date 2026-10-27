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
         (only-in "../private/scheduler.rkt" [parse-cron parse-cron-at-phase-1])
         (for-template racket/base
                       "../private/event.rkt"
                       "../private/ops/structure.rkt"
                       "../private/progress.rkt"
                       "../private/window.rkt"
                       "../private/context.rkt"
                       "../private/aggregate.rkt"
                       "../private/join.rkt"
                       "../private/ops/output.rkt"
                       "../private/derive.rkt"
                       "../private/rules-engine.rkt"
                       "../private/dataflow.rkt"
                       "../private/operator.rkt"
                       "../private/ops/stateless.rkt"
                       "../private/registry.rkt"
                       "registry-param.rkt"))

(provide stage-out*
         current-recursive-bound
         window-spec->operator
         default-window-fields
         (struct-out stage-ctx)
         (struct-out stage-out)
         register-stage!
         compile-stages
         event-lambda
         check-attributes!
         shape-with-fields)

;; prev: identifier of the previous node (#f before the first stage)
;; shape: a shape, key: syntax of a key function expression or #f
;; keying: #f or a hasheq of attribute names ('entity 'substream 'kind)
(struct stage-ctx (name prev shape key keying) #:transparent)
;; op: operator expression; inputs: list of input expressions (#f = prev)
;; extras: #f or (node-id -> list of definitions) spliced after the node
;; no-sink?: #t when the stream's own node must not be a sink (route)
(struct stage-out (op inputs shape key [extras #:auto] [no-sink? #:auto] [keying #:auto]
                      [last-only? #:auto])
  #:auto-value #f #:transparent #:mutable)

;; keying: 'same keeps the context's keying
;; last-only?: the stage must end its stream (route)
(define (stage-out* op inputs shape key #:extras [extras #f] #:sink? [sink? #t] #:keying [keying 'same]
                    #:last-only? [last-only? #f])
  (define o (stage-out op inputs shape key))
  (set-stage-out-extras! o extras)
  (set-stage-out-no-sink?! o (not sink?))
  (set-stage-out-keying! o keying)
  (set-stage-out-last-only?! o last-only?)
  o)

;; The #:recursive bound of the stream being compiled, if any.
(define current-recursive-bound (make-parameter #f))

(define stage-table (make-hasheq))
(define (register-stage! name proc) (hash-set! stage-table name proc))

;; compile-stages : identifier (listof syntax)
;;                  -> (values definitions final-id shape key keying)
(define (stage-head stx) (syntax-parse stx [(h:id . _) (syntax-e #'h)] [_ #f]))

;; A window stage directly followed by an aggregate stage fuses into one
;; windowed aggregation (§8).
(define (fuse-stages stages)
  (let loop ([ss stages] [acc '()])
    (cond
      [(null? ss) (reverse acc)]
      [(and (pair? (cdr ss)) (eq? (stage-head (car ss)) 'window) (eq? (stage-head (cadr ss)) 'aggregate))
       (loop (cddr ss)
             (cons (datum->syntax (car ss) (list (datum->syntax (car ss) 'windowed-aggregate) (car ss) (cadr ss))
                                  (car ss))
                   acc))]
      [else (loop (cdr ss) (cons (car ss) acc))])))

(define (compile-stages name-id stages0)
  (define stages (fuse-stages stages0))
  (define name (syntax-e name-id))
  (define n (length stages))
  (let loop ([stages stages] [i 1] [ctx (stage-ctx name #f #f #f #f)] [defs '()])
    (define stg (car stages))
    (define head (stage-head stg))
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
          (make-node '#,node-name #,(stage-out-op out) (list #,@inputs) #:sink? #,sink?
                     #:recursive #,(or (current-recursive-bound) #'#f))))
    (define extras (if (stage-out-extras out) ((stage-out-extras out) node-id) '()))
    (when (and (stage-out-last-only? out) (not last?))
      (raise-syntax-error 'define-stream "this stage must be the last stage of a stream" stg))
    (define keying (let ([k (stage-out-keying out)]) (if (or (eq? k 'same) (not k)) (if (eq? k 'same) (stage-ctx-keying ctx) #f) k)))
    (define ctx2 (stage-ctx name node-id (stage-out-shape out) (stage-out-key out) keying))
    (if last?
        (values (append (reverse (cons def defs)) extras) node-id (stage-out-shape out)
                (stage-ctx-key ctx2) (stage-ctx-keying ctx2))
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
      (define keying (and (source-static? v) (source-static-keying v)))
      (define head (or (and (source-static? v) (source-static-head v)) (static-runtime-id v)))
      (define entity (and keying (hash-ref keying 'entity #f)))
      (stage-out* (if (null? (syntax->list #'(t ...)))
                      #'(make-operator 'from)
                      #'(select-types-operator '(t ...)))
                  (list head)
                  selected
                  (if entity
                      #`(attribute-key-fn '#,entity)
                      (and (stream-static? v)
                           (let ([k (stream-static-key v)]) (and k (syntax-e k) k))))
                  #:keying (or keying (and (stream-static? v) (stream-static-keying v))))])))

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
;; (split T #:for (clause ...) [a expr] ... #:ts e maybe-te maybe-n
;;        maybe-reference)                                          OP.split
;; maybe-reference: #:reference substream-expr maybe-summary, where
;; maybe-summary is #:summary ([k expr] ...). Each child is then a reference
;; event (ES.reference): its `ref` attribute points at the sub-stream over
;; [ts, te] and carries the summary, instead of the samples themselves.

(register-stage!
 'split
 (λ (stx ctx)
   (define shp (stage-ctx-shape ctx))
   (syntax-parse stx
     [(_ t:id (~alt (~once (~seq #:for (clause ...)))
                    (~optional (~seq #:ts ts0:expr)) (~optional (~seq #:te te0:expr))
                    (~optional (~seq #:n n0:expr))
                    (~optional (~seq #:reference ref:expr))
                    (~optional (~seq #:summary ([sk:id sv:expr] ...)))
                    [a:id v:expr])
         ...)
      (define ty (lookup-type #'t stx))
      (check-fields! 'split ty #'(a ...) stx)
      (unless (attribute ts0) (raise-syntax-error 'split "each child needs #:ts" stx))
      (when (and (attribute sk) (not (attribute ref)))
        (raise-syntax-error 'split "#:summary needs #:reference: a summary belongs to a reference" stx))
      (with-syntax ([ts (attribute ts0)]
                    [te (or (attribute te0) (attribute ts0))]
                    [n (or (attribute n0) #'1)])
        (define child
          (if (attribute ref)
              #`(make-reference-event 't #:substream #,(attribute ref) #:ts ts #:te te #:n n
                                      #:summary (list #,@(for/list ([k (or (attribute sk) '())] [x (or (attribute sv) '())])
                                                           #`(cons '#,k #,x)))
                                      #:payload (list (cons 'a v) ...))
              #'(make-event 't (list (cons 'a v) ...) #:ts ts #:te te #:n n)))
        (stage-out* #`(split-operator
                       #,(event-lambda shp #`(for/list (clause ...) #,child) #:lexical stx))
                    #f
                    (if (attribute ref) (shape-with-fields (type-shape ty) '((ref . Any))) (type-shape ty))
                    (stage-ctx-key ctx)))])))

;; A stream of its own reading output `port` of `node-id`.
(define (side-stream-definition br node-id port shp ctx)
  (define br-node (car (generate-temporaries (list br))))
  #`(begin
      (define #,br-node
        (make-node '#,br (make-operator '#,br) (list (cons #,node-id #,port)) #:sink? #t))
      (define-syntax #,br (make-stream-static '#,br (shape '#,(shape-types shp))
                                              (quote-syntax #,br-node)
                                              #:key (quote-syntax #,(stage-ctx-key ctx))
                                              #:keying '#,(stage-ctx-keying ctx)))
      (registry-add-node! occur-registry #,br-node)))

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
                  #:last-only? #t
                  #:extras
                  (λ (node-id)
                    (for/list ([br branches] [port (in-naturals)])
                      (side-stream-definition br node-id port shp ctx))))])))

;; ---------------------------------------------------------------------------
;; (merge stream ...+): as the first stage, the union of the streams; later,
;; the union of the pipeline so far and the streams.                ST.merge

(register-stage!
 'merge
 (λ (stx ctx)
   (syntax-parse stx
     [(_ up:id ...+)
      (define vs (for/list ([u (syntax->list #'(up ...))]) (lookup-upstream u)))
      (define shapes (for/list ([v vs])
                       (if (source-static? v) (apply type-shape (source-static-types v)) (stream-static-shape v))))
      (define heads (for/list ([v vs])
                      (or (and (source-static? v) (source-static-head v)) (static-runtime-id v))))
      (define prev (stage-ctx-prev ctx))
      (define all-shapes (if prev (cons (stage-ctx-shape ctx) shapes) shapes))
      (define inputs (if prev (cons prev heads) heads))
      (stage-out* #`(merge-operator #,(length inputs))
                  inputs
                  (shape (remove-duplicates (append-map shape-types all-shapes)))
                  (stage-ctx-key ctx))])))

;; ---------------------------------------------------------------------------
;; (key-by attr maybe-substream maybe-kind) or (key-by #:fn expr)     ST.key-by
;; With #:substream s and #:kind m, the entity attr owns sub-streams named
;; by s, each of kind m (ES.substream), as a source's keying declares them.

(register-stage!
 'key-by
 (λ (stx ctx)
   (define shp (stage-ctx-shape ctx))
   (syntax-parse stx
     [(_ a:id (~alt (~optional (~seq #:substream sub:id)) (~optional (~seq #:kind kind:id))) ...)
      (check-attributes! 'key-by shp #`(a #,@(filter values (list (attribute sub) (attribute kind)))) stx)
      (when (and (attribute kind) (not (attribute sub)))
        (raise-syntax-error 'key-by "#:kind needs #:substream: kinds belong to sub-streams" stx (attribute kind)))
      (define keying
        (for/fold ([h (hash-set (or (stage-ctx-keying ctx) (hasheq)) 'entity (syntax-e #'a))])
                  ([k '(substream kind)] [v (list (attribute sub) (attribute kind))] #:when v)
          (hash-set h k (syntax-e v))))
      (stage-out* #'(key-by-operator (attribute-key-fn 'a)) #f shp #'(attribute-key-fn 'a)
                  #:keying keying)]
     [(_ #:fn f:expr)
      (define fn (event-lambda shp #'f))
      (stage-out* #`(key-by-operator #,fn) #f shp fn)])))

;; ---------------------------------------------------------------------------
;; (dedupe maybe-key #:horizon d maybe-clock)                       ST.dedupe

(define (stage-clock stx id allowed)
  (if id (check-clock! id allowed) 'event-time))

(register-stage!
 'dedupe
 (λ (stx ctx)
   (define shp (stage-ctx-shape ctx))
   (syntax-parse stx
     [(_ (~alt (~optional (~seq #:key k:expr)) (~once (~seq #:horizon h:expr))
               (~optional (~seq #:clock clk:id)))
         ...)
      (define clock (stage-clock stx (attribute clk) '(event-time processing-time)))
      (stage-out* #`(dedupe-operator #,(if (attribute k) (event-lambda shp #'k) #'event-id)
                                     #:horizon h #:clock '#,clock)
                  #f shp (stage-ctx-key ctx))])))

;; ---------------------------------------------------------------------------
;; (monotonic-merge maybe-substream)                       ST.monotonic-merge
;; Without #:substream, the sub-stream is the (entity, substream) pair of the
;; source's keying. A stream with an entity merges keyed by that entity.

(define (substream-fn-of ctx stx)
  (define ky (stage-ctx-keying ctx))
  (define sub (and ky (hash-ref ky 'substream #f)))
  (define ent (and ky (hash-ref ky 'entity #f)))
  (cond [(and sub ent) #`(λ (e) (cons (event-attr e '#,ent) (event-attr e '#,sub)))]
        [sub #`(attribute-key-fn '#,sub)]
        [else (raise-syntax-error 'monotonic-merge
                                  "no sub-stream: give #:substream or declare the source's #:substream"
                                  stx)]))

(register-stage!
 'monotonic-merge
 (λ (stx ctx)
   (define shp (stage-ctx-shape ctx))
   (syntax-parse stx
     [(_ (~optional (~seq #:substream s:expr)))
      ;; keyed by the stream's entity when it has one, so that dropping an
      ;; entity forgets its sub-streams' marks
      (define ent (let ([ky (stage-ctx-keying ctx)]) (and ky (hash-ref ky 'entity #f))))
      (stage-out* #`(monotonic-merge-operator
                     #,(if (attribute s) (event-lambda shp #'s) (substream-fn-of ctx stx))
                     #:entity #,(if ent #`(attribute-key-fn '#,ent) #'#f))
                  #f shp (stage-ctx-key ctx))])))

;; ---------------------------------------------------------------------------
;; (reorder maybe-late)                                             ST.reorder

(register-stage!
 'reorder
 (λ (stx ctx)
   (syntax-parse stx
     [(_ (~optional (~seq #:late (~and l (~or* (~datum drop) (~datum emit))))))
      (stage-out* #`(reorder-operator #:key #,(or (stage-ctx-key ctx) #'#f)
                                      #:late '#,(if (attribute l) (syntax-e #'l) 'drop))
                  #f (stage-ctx-shape ctx) (stage-ctx-key ctx))])))

;; ---------------------------------------------------------------------------
;; (throttle #:rate r #:per d maybe-clock) | (throttle #:every n)
;; | (throttle #:probability p #:seed s)                            ST.throttle

(register-stage!
 'throttle
 (λ (stx ctx)
   (define key (or (stage-ctx-key ctx) #'#f))
   (define (out mode) (stage-out* #`(throttle-operator #,mode #:key #,key)
                                  #f (stage-ctx-shape ctx) (stage-ctx-key ctx)))
   (syntax-parse stx
     [(_ #:rate r:expr #:per d:expr (~optional (~seq #:clock clk:id)))
      (define clock (stage-clock stx (attribute clk) '(event-time processing-time)))
      (out #`(list 'rate r d '#,clock))]
     [(_ #:every n:expr) (out #'(list 'every n))]
     [(_ #:probability p:expr #:seed s:expr) (out #'(list 'probability p s))])))

;; ===========================================================================
;; Time and progress (spec §6)

;; (skew-guard bound)                                           TM.skew-guard
(register-stage!
 'skew-guard
 (λ (stx ctx)
   (syntax-parse stx
     [(_ b:expr)
      (stage-out* #'(skew-guard-operator b) #f (stage-ctx-shape ctx) (stage-ctx-key ctx))])))

;; (lateness #:policy drop | side | (allow L) maybe-side-output)    TM.lateness
(register-stage!
 'lateness
 (λ (stx ctx)
   (define shp (stage-ctx-shape ctx))
   (syntax-parse stx
     [(_ #:policy (~or* (~and p (~datum drop)) (~and p (~datum side)) ((~and p (~datum allow)) l:expr))
         (~optional (~seq #:side-output side-name:id)))
      (define policy (case (syntax-e #'p) [(drop) #''drop] [(side) #''side] [else #'(list 'allow l)]))
      (when (and (attribute side-name) (not (eq? (syntax-e #'p) 'side)))
        (raise-syntax-error 'lateness "#:side-output needs #:policy side" stx #'side-name))
      (stage-out* #`(lateness-operator #,policy) #f shp (stage-ctx-key ctx)
                  #:extras (and (attribute side-name)
                                (λ (node-id) (list (side-stream-definition #'side-name node-id 1 shp ctx)))))])))

;; (delay d maybe-clock)                                    TM.timer
(register-stage!
 'delay
 (λ (stx ctx)
   (syntax-parse stx
     [(_ d:expr (~optional (~seq #:clock clk:id)))
      (define clock (stage-clock stx (attribute clk) '(event-time processing-time)))
      (stage-out* #`(delay-operator d #:clock '#,clock #:key #,(or (stage-ctx-key ctx) #'#f))
                  #f (stage-ctx-shape ctx) (stage-ctx-key ctx))])))

;; (keyed-watermark maybe-kinds maybe-mode): a stream of Progress events,
;; one whenever a key's data-driven watermark advances.     TM.keyed-watermark
(define progress-shape
  (shape '((Progress (key . Any) (watermark . Time) (mode . Any)))))

(register-stage!
 'keyed-watermark
 (λ (stx ctx)
   (define ky (stage-ctx-keying ctx))
   (syntax-parse stx
     [(_ (~alt (~optional (~seq #:kinds (k ...)))
               (~optional (~seq #:mode (~and m (~or* (~datum any) (~datum all))))))
         ...)
      (unless (stage-ctx-key ctx)
        (raise-syntax-error 'keyed-watermark "needs a keyed stream: add (key-by attr) or a source #:key" stx))
      (define kind (and ky (hash-ref ky 'kind #f)))
      (unless kind
        (raise-syntax-error 'keyed-watermark "needs the source's #:kind to tell kinds apart" stx))
      (stage-out* #`(keyed-watermark-operator #:key #,(stage-ctx-key ctx) #:kind (attribute-key-fn '#,kind)
                                              #:kinds #,(if (attribute k) #'(list 'k ...) #'#f)
                                              #:mode '#,(if (attribute m) (syntax-e #'m) 'all)
                                              #:pass-through? #f)
                  #f progress-shape (stage-ctx-key ctx))])))

;; (idle d maybe-clock): one Idle event per key after d without data.  TM.idle
(define idle-shape (shape '((Idle (key . Any) (since . Time) (after . Duration)))))

(register-stage!
 'idle
 (λ (stx ctx)
   (syntax-parse stx
     [(_ d:expr (~optional (~seq #:clock clk:id)))
      (define clock (if (attribute clk) (check-clock! #'clk '(processing-time hybrid)) 'processing-time))
      (stage-out* #`(idle-operator d #:clock '#,clock #:key #,(or (stage-ctx-key ctx) #'#f))
                  #f idle-shape (stage-ctx-key ctx))])))

;; ===========================================================================
;; Windows (spec §7)
;;
;; (window spec option ...) emits one Window event per (key, window) with the
;; window's contents as constituents. Followed directly by (aggregate ...),
;; the two stages fuse into one windowed aggregation (§8).

(define default-window-fields '((count . Integer) (events . Integer) (key . Any)))

(define (window-shape type fields) (shape (list (cons type fields))))

;; Bind the fields of an accumulator result as variables around `body`.
(define (result-lambda fields body #:lexical lexical)
  (define r (car (generate-temporaries '(r))))
  #`(λ (#,r) (let (#,@(for/list ([f fields])
                         #`[#,(datum->syntax lexical (car f)) (hash-ref #,r '#,(car f) #f)]))
               #,body)))

;; window-spec->operator : spec-stx option-stx ctx acc-expr fields
;;                         -> (values operator-expr shape)
(define (window-spec->operator stx spec opts ctx acc fields)
  (define shp (stage-ctx-shape ctx))
  (define key (or (stage-ctx-key ctx) #'#f))
  (define-values (lateness fire-empty assign clock wm type)
    (syntax-parse opts
      [((~alt (~optional (~seq #:lateness l:expr))
              (~optional (~and fe #:fire-empty))
              (~optional (~seq #:assign (~and a (~or* (~datum start) (~datum end) (~datum overlap)))))
              (~optional (~seq #:clock clk:id))
              (~optional (~seq #:watermark ((~datum keyed) (~and m (~or* (~datum any) (~datum all)))
                                            (~optional (~seq #:kinds (k ...))))))
              (~optional (~seq #:type t:id)))
        ...)
       (values (or (attribute l) #'0)
               (and (attribute fe) #t)
               (if (attribute a) (syntax-e #'a) 'start)
               (if (attribute clk) (check-clock! #'clk '(event-time processing-time)) 'event-time)
               (and (attribute m) (list (syntax-e #'m) (if (attribute k) #'(list k ...) #'#f)))
               (if (attribute t) (syntax-e #'t) 'Window))]))
  (define closer
    (cond
      [wm
       (define ky (stage-ctx-keying ctx))
       (define kind (and ky (hash-ref ky 'kind #f)))
       (unless (and kind (stage-ctx-key ctx))
         (raise-syntax-error 'window "a keyed watermark needs a keyed source with #:kind" stx))
       #`(list 'keyed '#,(car wm) #,(cadr wm) (attribute-key-fn '#,kind))]
      [(eq? clock 'processing-time) #''clock]
      [else #''watermark]))
  (define extra-fields '())
  (define op
    (syntax-parse spec
      [((~datum tumbling) #:size d:expr (~optional (~seq #:offset o:expr)))
       #`(grid-window-operator (tumbling d #:offset #,(or (attribute o) #'0)) #,acc
                               #:key #,key #:assign '#,assign #:lateness #,lateness
                               #:fire-empty? #,fire-empty #:closer #,closer #:type '#,type)]
      [((~datum sliding) (~alt (~once (~seq #:size d:expr)) (~once (~seq #:slide sl:expr))
                               (~optional (~seq #:offset o:expr))) ...)
       #`(grid-window-operator (sliding d sl #:offset #,(or (attribute o) #'0)) #,acc
                               #:key #,key #:assign '#,assign #:lateness #,lateness
                               #:fire-empty? #,fire-empty #:closer #,closer #:type '#,type)]
      [((~datum session) #:gap g:expr)
       #`(session-window-operator g #,acc #:key #,key #:lateness #,lateness #:type '#,type)]
      [((~datum count) #:size n:expr (~optional (~seq #:step m:expr)))
       #`(count-window-operator n #,(or (attribute m) #'n) #,acc #:key #,key #:type '#,type)]
      [((~datum landmark) #:from t0:expr)
       #`(landmark-window-operator t0 #,acc #:key #,key #:type '#,type)]
      [((~datum punctuated) (~alt (~once (~seq #:open o:expr)) (~once (~seq #:close c:expr))
                                  (~optional (~seq #:timeout d:expr)) (~optional (~seq #:clock tc:id)))
                            ...)
       (set! extra-fields '((reason . Any)))
       (define tclock (if (attribute tc) (check-clock! #'tc '(event-time processing-time)) 'event-time))
       #`(punctuated-window-operator #,(event-lambda shp #'o) #,(event-lambda shp #'c) #,acc
                                     #:key #,key #:timeout #,(or (attribute d) #'#f)
                                     #:timeout-clock '#,tclock #:type '#,type)]
      [((~datum since-last) #:when w:expr)
       #`(since-last-window-operator #,(result-lambda fields #'w #:lexical stx) #,acc #:key #,key #:type '#,type)]
      [((~datum calendar) e:str (~optional (~seq #:clock cc:id)))
       (with-handlers ([exn:fail? (λ (x) (raise-syntax-error 'window (exn-message x) spec #'e))])
         (parse-cron-at-phase-1 (syntax-e #'e)))
       (set! extra-fields '((windows . Integer)))
       (define cclock (if (attribute cc) (check-clock! #'cc '(event-time processing-time)) 'processing-time))
       #`(calendar-window-operator e #,acc #:clock '#,cclock #:key #,key #:type '#,type)]
      [_ (raise-syntax-error 'window
                             "expected (tumbling ...), (sliding ...), (session ...), (count ...), (landmark ...), (punctuated ...), (since-last ...), or (calendar ...)"
                             stx spec)]))
  (values op (window-shape type (append fields extra-fields))))

(register-stage!
 'window
 (λ (stx ctx)
   (syntax-parse stx
     [(_ spec opt ...)
      (define-values (op shp)
        (window-spec->operator stx #'spec #'(opt ...) ctx #'contents-accumulator default-window-fields))
      (stage-out* op #f shp (stage-ctx-key ctx))])))

;; ===========================================================================
;; Contexts (spec §7)

(define (context-assigner stx spec ctx)
  (define shp (stage-ctx-shape ctx))
  (syntax-parse spec
    [((~datum fixed-interval) #:from a:expr #:to b:expr) #'(fixed-interval-context a b)]
    [((~datum event-interval) (~alt (~once (~seq #:open o:expr)) (~once (~seq #:close c:expr))
                                    (~optional (~seq #:timeout d:expr))) ...)
     #`(event-interval-context #,(event-lambda shp #'o) #,(event-lambda shp #'c) #:timeout #,(or (attribute d) #'#f))]
    [((~datum sliding-interval) #:size d:expr #:slide s:expr) #'(sliding-interval-context d s)]
    [((~datum sliding-event-interval) #:open o:expr #:duration d:expr)
     #`(sliding-event-interval-context #,(event-lambda shp #'o) d)]
    [((~datum segment) a:id ...+)
     (check-attributes! 'context shp #'(a ...) stx)
     #'(segmentation-context '(a ...))]
    [((~datum near) (~alt (~once (~seq #:location l:expr)) (~once (~seq #:radius r:expr))
                          (~optional (~seq #:attr at:id))) ...)
     #`(fixed-location-context l r #:location '#,(or (attribute at) #'location))]
    [((~datum near-entity) (~alt (~once (~seq #:locate f:expr)) (~once (~seq #:entities es:expr))
                                 (~once (~seq #:radius r:expr)) (~optional (~seq #:attr at:id))) ...)
     #`(entity-distance-context f es r #:location '#,(or (attribute at) #'location))]
    [((~datum near-event) (~alt (~once (~seq #:anchor a:expr)) (~once (~seq #:radius r:expr))
                                (~once (~seq #:duration d:expr)) (~optional (~seq #:attr at:id))) ...)
     #`(event-distance-context #,(event-lambda shp #'a) r d #:location '#,(or (attribute at) #'location))]
    [((~datum and) part ...+)
     #`(composite-context #,@(for/list ([p (syntax->list #'(part ...))]) (context-assigner stx p ctx)))]
    [_ (raise-syntax-error 'context "unknown context" stx spec)]))

(register-stage!
 'context
 (λ (stx ctx)
   (define shp (stage-ctx-shape ctx))
   (define key (stage-ctx-key ctx))
   (define tagged (shape-with-fields shp '((context . Any))))
   (syntax-parse stx
     [(_ ((~datum state) #:from up:id #:active a:expr))
      (define v (lookup-upstream #'up))
      (define up-shape (if (source-static? v) (apply type-shape (source-static-types v)) (stream-static-shape v)))
      (stage-out* #`(state-context-operator #,(event-lambda up-shape #'a))
                  (list (stage-ctx-prev ctx) (or (and (source-static? v) (source-static-head v)) (static-runtime-id v)))
                  tagged key)]
     [(_ ((~datum entity) dir:id))
      (unless key (raise-syntax-error 'context "the entity context needs a keyed stream" stx))
      (define d (lookup-static #'dir))
      (unless (directory-static? d) (raise-syntax-error 'context "not a declared directory" stx #'dir))
      (stage-out* #`(entity-context-operator '#,(directory-static-name d) #:key #,key) #f shp key)]
     [(_ spec)
      (stage-out* #`(context-operator #,(context-assigner stx #'spec ctx)) #f tagged key)])))

;; (activations): the activation instants of the stream's entities.  CX.activation
(register-stage!
 'activations
 (λ (stx ctx)
   (syntax-parse stx
     [(_) (stage-out* #'activations-operator #f (shape '((Activated (key . Any)))) (stage-ctx-key ctx))])))

;; ===========================================================================
;; Aggregation (spec §8)
;;
;; (aggregate [name (agg arg ...)] ...): after a window, one event per
;; (key, window) with the named results; on its own, a running aggregate per
;; key emitted after every event.

(define (aggregate-form stx clause shp ctx)
  (define (f e) (event-lambda shp e #:lexical stx))
  (define ky (stage-ctx-keying ctx))
  (syntax-parse clause
    [((~datum count)) #'(count-agg)]
    [((~datum multiplicity)) #'(multiplicity-agg)]
    [((~datum events)) #'(events-agg)]
    [((~datum sum) e) #`(sum-agg #,(f #'e))]
    [((~datum min) e) #`(min-agg #,(f #'e))]
    [((~datum max) e) #`(max-agg #,(f #'e))]
    [((~datum avg) e) #`(avg-agg #,(f #'e))]
    [((~datum variance) e (~optional (~and s #:sample))) #`(variance-agg #,(f #'e) #:sample? #,(and (attribute s) #t))]
    [((~datum stddev) e (~optional (~and s #:sample))) #`(stddev-agg #,(f #'e) #:sample? #,(and (attribute s) #t))]
    [((~datum first) e) #`(first-agg #,(f #'e))]
    [((~datum last) e) #`(last-agg #,(f #'e))]
    [((~datum span)) #'(span-agg)]
    [((~datum sources) sub kind) #`(sources-agg #,(f #'sub) #,(f #'kind))]
    [((~datum sources))
     (define sub (and ky (hash-ref ky 'substream #f)))
     (define kind (and ky (hash-ref ky 'kind #f)))
     (unless (and sub kind)
       (raise-syntax-error 'aggregate "(sources) needs a source with #:substream and #:kind, or (sources sub kind)" stx clause))
     #`(sources-agg (attribute-key-fn '#,sub) (attribute-key-fn '#,kind))]
    [((~datum collect) e) #`(collect-agg #,(f #'e))]
    [((~datum argmax) e) #`(argmax-agg #,(f #'e))]
    [((~datum argmin) e) #`(argmin-agg #,(f #'e))]
    [((~datum median) e) #`(median-agg #,(f #'e))]
    [((~datum percentile) e p:expr) #`(percentile-agg #,(f #'e) p)]
    [((~datum count-distinct) e) #`(count-distinct-agg #,(f #'e))]
    [((~datum mode) e) #`(mode-agg #,(f #'e))]
    [((~datum top-k) e k:expr (~optional (~seq #:by (~and by (~or* (~datum value) (~datum frequency))))))
     #`(top-k-agg #,(f #'e) k #:by '#,(if (attribute by) (syntax-e #'by) 'value))]
    [((~datum approx-distinct) e) #`(approx-distinct-agg #,(f #'e))]
    [((~datum approx-percentile) e p:expr) #`(approx-percentile-agg #,(f #'e) p)]
    [((~datum approx-frequency) e x:expr) #`(approx-frequency-agg #,(f #'e) x)]
    [((~datum count-min) e) #`(count-min-agg #,(f #'e))]
    [_ (raise-syntax-error 'aggregate
                           "unknown aggregate; expected count, multiplicity, events, sum, min, max, avg, variance, stddev, first, last, span, sources, collect, argmax, argmin, median, percentile, count-distinct, mode, top-k, approx-distinct, approx-percentile, approx-frequency, or count-min"
                           stx clause)]))

;; (values accumulator-expr fields)
(define (aggregate-clauses stx agg-stx ctx)
  (define shp (stage-ctx-shape ctx))
  (syntax-parse agg-stx
    [(_ [n:id clause] ...+)
     (define dup (check-duplicates (syntax->list #'(n ...)) #:key syntax-e))
     (when dup (raise-syntax-error 'aggregate "result name given twice" agg-stx dup))
     (values #`(aggregate-accumulator
                (list #,@(for/list ([n (syntax->list #'(n ...))] [c (syntax->list #'(clause ...))])
                           #`(cons '#,n #,(aggregate-form agg-stx c shp ctx)))))
             (append (for/list ([n (syntax->list #'(n ...))]) (cons (syntax-e n) 'Any)) '((key . Any))))]))

(register-stage!
 'windowed-aggregate
 (λ (stx ctx)
   (syntax-parse stx
     [(_ (~and w (_ spec opt ...)) agg)
      (define-values (acc fields) (aggregate-clauses #'agg #'agg ctx))
      (define-values (op shp) (window-spec->operator #'w #'spec #'(opt ...) ctx acc fields))
      (stage-out* op #f shp (stage-ctx-key ctx))])))

(register-stage!
 'aggregate
 (λ (stx ctx)
   (define-values (acc fields) (aggregate-clauses stx stx ctx))
   (stage-out* #`(landmark-window-operator -inf.0 #,acc #:key #,(or (stage-ctx-key ctx) #'#f) #:type 'Aggregate)
               #f (shape (list (cons 'Aggregate fields))) (stage-ctx-key ctx))))

;; ===========================================================================
;; Joins (spec §9)
;;
;; (join other #:kind kind key-option option ...) joins the pipeline (left)
;; with another stream (right). θ (#:on) and #:select see the attributes as
;; left.attr and right.attr; an unmatched side reads as #f.

(define (pair-lambda lshape rshape body #:lexical lexical)
  (define l (car (generate-temporaries '(l))))
  (define r (car (generate-temporaries '(r))))
  (define (binds side ev shp)
    (for/list ([a (shape-attributes shp)])
      #`[#,(datum->syntax lexical (string->symbol (format "~a.~a" side a)))
         (and #,ev (event-attr/default #,ev '#,a #f))]))
  #`(λ (#,l #,r) (let (#,@(binds 'left l lshape) #,@(binds 'right r rshape)) #,body)))

(register-stage!
 'join
 (λ (stx ctx)
   (define lshape (stage-ctx-shape ctx))
   (syntax-parse stx
     [(_ other:id
         (~alt (~once (~seq #:kind (~and kind (~or* (~datum window) (~datum interval) (~datum table) (~datum as-of)))))
               (~optional (~seq #:key k:id))
               (~optional (~seq #:keys (lk:id rk:id)))
               (~optional (~seq #:on θ:expr))
               (~optional (~seq #:window w))
               (~optional (~seq #:lower lo:expr))
               (~optional (~seq #:upper hi:expr))
               (~optional (~seq #:valid-from vf:expr))
               (~optional (~seq #:outer (~and outer (~or* (~datum left) (~datum right) (~datum full)))))
               (~optional (~seq #:type t:id))
               (~optional (~seq #:select ([sa:id se:expr] ...))))
         ...)
      (define v (lookup-upstream #'other))
      (define rshape (if (source-static? v) (apply type-shape (source-static-types v)) (stream-static-shape v)))
      (define right-input (or (and (source-static? v) (source-static-head v)) (static-runtime-id v)))
      (define kind* (syntax-e #'kind))
      ;; the shared key
      (define-values (la ra)
        (cond [(attribute k) (values #'k #'k)]
              [(attribute lk) (values #'lk #'rk)]
              [else (raise-syntax-error 'join "give #:key attr, or #:keys (left-attr right-attr)" stx)]))
      (check-attributes! 'join lshape #`(#,la) stx)
      (check-attributes! 'join rshape #`(#,ra) stx)
      (define left-types (shape-type-names lshape))
      (define key-fn
        (if (eq? (syntax-e la) (syntax-e ra))
            #`(attribute-key-fn '#,la)
            #`(λ (e) (if (memq (event-type e) '#,left-types) (event-attr e '#,la) (event-attr e '#,ra)))))
      (define θ-fn (if (attribute θ) (pair-lambda lshape rshape #'θ #:lexical stx) #'(λ (l r) #t)))
      (define type (if (attribute t) (syntax-e #'t) 'Joined))
      (define combine
        (if (attribute sa)
            (pair-lambda lshape rshape #'(make-immutable-hasheq (list (cons 'sa se) ...)) #:lexical stx)
            #'default-combine))
      (define out-fields
        (if (attribute sa)
            (for/list ([a (syntax->list #'(sa ...))]) (cons (syntax-e a) 'Any))
            (remove-duplicates (append (for*/list ([ty (shape-types lshape)] [f (cdr ty)]) f)
                                       (for*/list ([ty (shape-types rshape)] [f (cdr ty)]) f))
                               #:key car)))
      (define outer* (if (attribute outer) #`'#,(syntax-e #'outer) #'#f))
      (define common #`(#:key #,key-fn #:type '#,type #:combine #,combine))
      (define lattrs #`'#,(shape-attributes lshape))
      (define rattrs #`'#,(shape-attributes rshape))
      (when (and (memq kind* '(table as-of)) (attribute outer) (not (eq? (syntax-e #'outer) 'left)))
        (raise-syntax-error 'join "table and as-of joins support only #:outer left" stx #'outer))
      (define op
        (case kind*
          [(window)
           (unless (attribute w) (raise-syntax-error 'join "a window join needs #:window" stx))
           (define g (syntax-parse #'w
                       [((~datum tumbling) #:size d:expr) #'(tumbling d)]
                       [((~datum sliding) #:size d:expr #:slide sl:expr) #'(sliding d sl)]
                       [_ (raise-syntax-error 'join "expected (tumbling #:size d) or (sliding #:size d #:slide s)" stx #'w)]))
           #`(window-join-operator #,g #,θ-fn #,@common #:outer #,outer*
                                   #:left-attributes #,lattrs #:right-attributes #,rattrs)]
          [(interval)
           (unless (and (attribute lo) (attribute hi))
             (raise-syntax-error 'join "an interval join needs #:lower and #:upper" stx))
           #`(interval-join-operator #,(attribute lo) #,(attribute hi) #,θ-fn #,@common #:outer #,outer*
                                     #:left-attributes #,lattrs #:right-attributes #,rattrs)]
          [(table)
           #`(table-join-operator #,@common #:outer #,outer* #:right-attributes #,rattrs)]
          [(as-of)
           #`(as-of-join-operator #,@common #:outer #,outer* #:right-attributes #,rattrs
                                  #:valid-from #,(if (attribute vf) (event-lambda rshape #'vf) #'event-ts))]))
      (stage-out* op (list (stage-ctx-prev ctx) right-input)
                  (shape (list (cons type out-fields))) key-fn)])))

;; ===========================================================================
;; Output policies on streams (spec §12.3)

;; (coalesce maybe-sum): outputs of one key due at one instant become one.  OUT.coalesce
(register-stage!
 'coalesce
 (λ (stx ctx)
   (syntax-parse stx
     [(_ (~optional (~seq #:sum (a:id ...))))
      (stage-out* #`(coalesce-operator #:key #,(or (stage-ctx-key ctx) #'#f)
                                       #:sum '#,(if (attribute a) #'(a ...) #'()))
                  #f (shape-with-fields (stage-ctx-shape ctx) '((merged . Integer))) (stage-ctx-key ctx))])))

;; (rate-limit d #:mode drop|defer): one output per d of processing time.   OUT.rate-limit
(register-stage!
 'rate-limit
 (λ (stx ctx)
   (syntax-parse stx
     [(_ d:expr (~optional (~seq #:mode (~and m (~or* (~datum drop) (~datum defer))))))
      (stage-out* #`(rate-limit-operator d #:mode '#,(if (attribute m) (syntax-e #'m) 'defer)
                                         #:key #,(or (stage-ctx-key ctx) #'#f))
                  #f (stage-ctx-shape ctx) (stage-ctx-key ctx))])))

;; (edge #:enter expr maybe-leave maybe-report): only transitions.          OUT.edge
(register-stage!
 'edge
 (λ (stx ctx)
   (define shp (stage-ctx-shape ctx))
   (syntax-parse stx
     [(_ (~alt (~once (~seq #:enter en:expr)) (~optional (~seq #:leave lv:expr))
               (~optional (~seq #:report-leave rl:boolean)))
         ...)
      (stage-out* #`(edge-operator #:enter? #,(event-lambda shp #'en)
                                   #:leave? #,(if (attribute lv) (event-lambda shp #'lv) #'(λ (e) #f))
                                   #:report-leave? #,(if (attribute rl) #'rl #'#t)
                                   #:key #,(or (stage-ctx-key ctx) #'#f))
                  #f shp (stage-ctx-key ctx))])))

;; ===========================================================================
;; Derivation (spec §13)

;; (summarize): replace each event's constituents with a summary.   DR.summary
(register-stage!
 'summarize
 (λ (stx ctx)
   (syntax-parse stx
     [(_) (stage-out* #'(map-like-summarize) #f (shape-with-fields (stage-ctx-shape ctx) '((summary . Any)))
                      (stage-ctx-key ctx))])))

;; (dedupe-outputs): drop events whose content identity was seen.   DR.output-identity
(register-stage!
 'dedupe-outputs
 (λ (stx ctx)
   (syntax-parse stx
     [(_) (stage-out* #'(identity-dedupe-operator) #f (stage-ctx-shape ctx) (stage-ctx-key ctx))])))

;; (feedback name): also read the stream `name`, by name, so a stream can
;; consume a later stream or itself. Cycles must be declared #:recursive
;; with a bound (DR.cycle-guard); a direct self-loop without one is a
;; compile-time error, and other loops are rejected when the program is
;; built.
(register-stage!
 'feedback
 (λ (stx ctx)
   (syntax-parse stx
     [(_ n:id)
      (when (and (eq? (syntax-e #'n) (stage-ctx-name ctx)) (not (current-recursive-bound)))
        (raise-syntax-error 'define-stream
                            (format "cycle guard: stream ~a consumes its own outputs; declare #:recursive with a termination bound"
                                    (syntax-e #'n))
                            stx #'n))
      (stage-out* #'(merge-operator 2) (list (stage-ctx-prev ctx) #''n) (stage-ctx-shape ctx) (stage-ctx-key ctx))])))

;; ===========================================================================
;; Rules (spec §14)
;;
;; (rules): the rules node. It runs the rule instances in force on each key
;; of a keyed stream, reading kinds and sub-streams through the source's
;; keying, and emits RuleOutput events.

(define rule-output-shape
  (shape '((RuleOutput (rule . Any) (key . Any) (binding-key . Any) (target . Any) (reason . Any)
                       (window . Any) (samples . Integer) (windows . Integer) (sources . Any)
                       (params . Any) (emitted-at . Time) (identity . String) (summary . Any)))))

(register-stage!
 'rules
 (λ (stx ctx)
   (syntax-parse stx
     [(_)
      (define key (stage-ctx-key ctx))
      (unless key (raise-syntax-error 'rules "the rules node needs a keyed stream: a source #:key or (key-by attr)" stx))
      (define ky (stage-ctx-keying ctx))
      (define kind (and ky (hash-ref ky 'kind #f)))
      (define sub (and ky (hash-ref ky 'substream #f)))
      (stage-out* #`(rules-operator #:key #,key
                                    #:kind #,(if kind #`(attribute-key-fn '#,kind) #'(λ (e) #f))
                                    #:substream #,(if sub #`(attribute-key-fn '#,sub) #'(λ (e) #f)))
                  #f rule-output-shape key)])))
