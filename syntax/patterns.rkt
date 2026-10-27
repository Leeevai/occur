#lang racket/base
;; The pattern compiler behind define-pattern (phase 1).
;;
;; It parses the pattern sub-language into an expression building the AST of
;; private/pattern/ast.rkt, records the bindings (name, type, single or
;; list), and compiles the expressions that see them: primitive predicates
;; see the event's attributes; #:where and #:emit see each binding x, its
;; attributes as x.attr, and list helpers. Compile-time checks cover unknown
;; types and attributes, bindings used but never bound, and Kleene bindings
;; used where a single event is expected.
(require racket/list
         racket/string
         syntax/parse
         "static.rkt"
         "stages.rkt"
         (for-template racket/base
                       "../private/event.rkt"
                       "../private/pattern/ast.rkt"
                       "../private/pattern/families.rkt"
                       "../private/pattern/operators.rkt"
                       "../private/pattern/where-helpers.rkt"))

(provide parse-pattern
         bindings-lambda
         check-binding-uses!
         (struct-out pbinding))

;; name: symbol; type: type-static or #f; kind: 'single or 'list
(struct pbinding (name type kind) #:transparent)

;; parse-pattern : syntax shape boolean -> (values ast-expr (listof pbinding) has-bound?)
;; in-list? is true under a repetition (bindings become lists).
(define (parse-pattern stx shp [in-list? #f])
  (define bindings '())
  (define (bind! name type in-list?)
    (define existing (findf (λ (b) (eq? (pbinding-name b) (syntax-e name))) bindings))
    (when (and existing (not (eq? (pbinding-kind existing) (if in-list? 'list 'single))))
      (raise-syntax-error 'define-pattern "a binding is used both inside and outside a repetition" stx name))
    (unless existing
      (set! bindings (append bindings (list (pbinding (syntax-e name) type (if in-list? 'list 'single)))))))
  (define (type-in-shape t form)
    (define ty (lookup-type t form))
    (unless (memq (type-static-name ty) (shape-type-names shp))
      (raise-syntax-error 'define-pattern
                          (format "the stream does not carry events of type ~a; it carries ~a"
                                  (type-static-name ty) (shape-type-names shp))
                          form t))
    ty)
  (define (pred-of ty pred)
    (event-lambda (type-shape ty) pred))
  (define (go p in-list?)
    (syntax-parse p
      #:datum-literals (: act seq and or any unless kleene+ kleene* repeat within where absent
                          aperiodic aperiodic* periodic periodic*)
      [act #'act]
      [t:id
       #:when (type-static? (lookup-static #'t))
       (type-in-shape #'t p)
       #'(prim 't)]
      [(x:id : t:id)
       (define ty (type-in-shape #'t p))
       (bind! #'x ty in-list?)
       #'(prim 't #:bind 'x)]
      [(x:id : t:id pred:expr)
       (define ty (type-in-shape #'t p))
       (bind! #'x ty in-list?)
       #`(prim 't #:bind 'x #:when #,(pred-of ty #'pred))]
      [(t:id pred:expr)
       #:when (type-static? (lookup-static #'t))
       (define ty (type-in-shape #'t p))
       #`(prim 't #:when #,(pred-of ty #'pred))]
      [(seq part ...+) (go-seq p (syntax->list #'(part ...)) in-list?)]
      [(and a b ...+) #`(conj #,(go #'a in-list?) #,@(for/list ([x (syntax->list #'(b ...))]) (go x in-list?)))]
      [(or a b ...+) #`(disj #,(go #'a in-list?) #,@(for/list ([x (syntax->list #'(b ...))]) (go x in-list?)))]
      [(any k:exact-positive-integer a ...+)
       (unless (<= (syntax-e #'k) (length (syntax->list #'(a ...))))
         (raise-syntax-error 'define-pattern "ANY(k; …) needs k at most the number of patterns" p #'k))
       #`(any-of k #,@(for/list ([x (syntax->list #'(a ...))]) (go x in-list?)))]
      [(unless a q) #`(span-absence #,(go #'a in-list?) #,(go-closed #'q))]
      [(kleene+ a) #`(kleene+ #,(go #'a #t))]
      [(kleene* a) #`(kleene* #,(go #'a #t))]
      [(repeat a n:exact-nonnegative-integer) #`(repeat #,(go #'a #t) n)]
      [(repeat a n:exact-nonnegative-integer m:exact-positive-integer)
       (when (> (syntax-e #'n) (syntax-e #'m))
         (raise-syntax-error 'define-pattern "a repetition's minimum exceeds its maximum" p))
       #`(repeat #,(go #'a #t) n m)]
      [(within a d:expr) #`(within #,(go #'a in-list?) d)]
      [(where a θ:expr)
       (define inner (go #'a in-list?))
       (check-binding-uses! #'θ bindings p)
       #`(where #,inner #,(bindings-lambda bindings #'θ #:lexical p))]
      [(aperiodic a b c) #`(aperiodic #,(go #'a in-list?) #,(go #'b in-list?) #,(go #'c in-list?))]
      [(aperiodic* a b c) #`(aperiodic* #,(go #'a in-list?) #,(go #'b #t) #,(go #'c in-list?))]
      [(periodic a d:expr c) #`(periodic #,(go #'a in-list?) d #,(go-closed #'c))]
      [(periodic* a d:expr c) #`(periodic* #,(go #'a in-list?) d #,(go #'c in-list?))]
      [(absent . _) (raise-syntax-error 'define-pattern "absent is allowed only inside seq" p)]
      [_ (raise-syntax-error 'define-pattern
                             "expected a pattern: (x : Type pred), Type, act, (seq …), (and …), (or …), (any k …), (unless P Q), (kleene+ P), (kleene* P), (repeat P n m), (within P d), (where P θ), (aperiodic …), or (periodic …)"
                             p)]))
  ;; negated patterns are closed: their bindings are not visible outside
  (define (go-closed q)
    (define-values (ast _bs) (parse-pattern q shp in-list?))
    ast)
  ;; (seq P (absent Q) P …) is negation between; a trailing (absent Q #:within d) is absence.
  (define (go-seq form parts in-list?)
    (define (absent? x) (syntax-parse x #:datum-literals (absent) [(absent . _) #t] [_ #f]))
    (let loop ([acc #f] [parts parts])
      (cond
        [(null? parts) (or acc (raise-syntax-error 'define-pattern "seq needs a pattern" form))]
        [(absent? (car parts))
         (syntax-parse (car parts)
           #:datum-literals (absent)
           [(absent q #:within d:expr)
            (unless acc (raise-syntax-error 'define-pattern "absent needs a pattern before it" form (car parts)))
            (loop #`(absence #,acc #,(go-closed #'q) d) (cdr parts))]
           [(absent q)
            (unless acc (raise-syntax-error 'define-pattern "absent needs a pattern before it" form (car parts)))
            (when (null? (cdr parts))
              (raise-syntax-error 'define-pattern
                                  "a trailing absent needs #:within: (absent Q #:within d)" form (car parts)))
            (when (absent? (cadr parts))
              (raise-syntax-error 'define-pattern "two absents in a row" form (cadr parts)))
            (loop #`(neg-between #,acc #,(go-closed #'q) #,(go (cadr parts) in-list?)) (cddr parts))])]
        [else (define next (go (car parts) in-list?))
              (loop (if acc #`(seq #,acc #,next) next) (cdr parts))])))
  (define ast (go stx in-list?))
  (values ast bindings))

;; ---------------------------------------------------------------------------
;; Expressions over bindings: each binding x is a variable (an event, or a
;; list of events for a Kleene binding), and x.attr its attribute (a list of
;; values for a Kleene binding).

(define (dotted id) (string->symbol (format "~a.~a" (car id) (cdr id))))

(define (binding-attrs b) (if (pbinding-type b) (map car (type-static-fields (pbinding-type b))) '()))

(define (bindings-lambda bindings body #:lexical lexical)
  (define bs (car (generate-temporaries '(bs))))
  (define clauses
    (append*
     (for/list ([b bindings])
       (define x (datum->syntax lexical (pbinding-name b)))
       (define list? (eq? (pbinding-kind b) 'list))
       (cons #`[#,x (hash-ref #,bs '#,(pbinding-name b) #,(if list? #''() #'#f))]
             (for/list ([a (binding-attrs b)])
               #`[#,(datum->syntax lexical (dotted (cons (pbinding-name b) a)))
                  #,(if list?
                        #`(map (λ (e) (event-attr/default e '#,a #f)) #,x)
                        #`(and #,x (event-attr/default #,x '#,a #f)))])))))
  #`(λ (#,bs) (with-where-helpers #,lexical (let* (#,@clauses) #,body))))

;; The heads under which a Kleene binding (a list) may appear.
(define list-heads
  '(count sum avg min max span length first last increasing? decreasing? non-increasing? non-decreasing?
    stable? mixed? trend increasing decreasing non-increasing non-decreasing stable mixed
    apply map andmap ormap filter foldl foldr for-each list-ref reverse sort median percentile
    relative-max? relative-min? moving-toward? moving-away? constant-direction? min-distance max-distance
    avg-distance count-distinct remove-duplicates member memq null? pair? empty? append cons list
    events collect values))

;; Check every use of a binding in `expr`: dotted references must name a
;; bound pattern variable and an attribute of its type, and a Kleene
;; binding may appear only as an argument of a list operation.
(define (check-binding-uses! expr bindings form)
  (define (lookup name) (findf (λ (b) (eq? (pbinding-name b) name)) bindings))
  (define (split sym)
    (define s (symbol->string sym))
    (define m (regexp-match #rx"^([^.]+)\\.([^.]+)$" s))
    (and m (cons (string->symbol (cadr m)) (string->symbol (caddr m)))))
  (let walk ([x expr] [head #f])
    (syntax-parse x
      #:literals (quote)
      [(quote . _) (void)]
      [(h . args)
       (define hd (and (identifier? #'h) (syntax-e #'h)))
       (walk #'h #f)
       (for ([a (syntax->list #'args)]) (walk a hd))]
      [id:id
       (define sym (syntax-e #'id))
       (define parts (split sym))
       (define b (lookup (if parts (car parts) sym)))
       (cond
         [(and parts (not b) (not (identifier-binding #'id)))
          (raise-syntax-error 'define-pattern (format "binding ~a is used but never bound" (car parts)) form #'id)]
         [(and parts b (pbinding-type b) (not (memq (cdr parts) (binding-attrs b))))
          (raise-syntax-error 'define-pattern
                              (format "unknown attribute ~a for binding ~a (~a events carry ~a)"
                                      (cdr parts) (car parts) (type-static-name (pbinding-type b)) (binding-attrs b))
                              form #'id)]
         [(and b (eq? (pbinding-kind b) 'list) (not (memq head list-heads)))
          (raise-syntax-error 'define-pattern
                              (format "~a is a Kleene binding (a list of events); use it where a list is expected, as in (sum ~a.attr) or (count ~a)"
                                      (pbinding-name b) (pbinding-name b) (pbinding-name b))
                              form #'id)]
         [else (void)])]
      [_ (void)])))
