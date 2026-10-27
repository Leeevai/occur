#lang racket/base
;; Compile-time descriptions of occur declarations.
;;
;; This module is used at phase 1 by the expander. Each declaration form
;; binds its name to one of these structs, so later forms can check types,
;; attributes, and sources at compile time with source locations. Used as
;; an expression, a declared name refers to its runtime value.
(require racket/list
         syntax/parse
         (for-template racket/base
                       "../private/event.rkt"
                       "../private/io.rkt"))

(provide (struct-out type-static)
         (struct-out source-static)
         (struct-out stream-static)
         (struct-out shape)
         lookup-static
         lookup-type
         lookup-source
         lookup-upstream
         static-runtime-id
         parse-domain
         type-shape
         shape-attributes
         shape-type-names
         shape-single
         shape-has-attribute?
         shape-domain)

;; An identifier macro: `name` alone means the runtime value; `(name . args)`
;; is handled by `apply-proc` (the event constructor for types).
(define (identifier-macro runtime-of apply-proc)
  (λ (self stx)
    (syntax-case stx ()
      [id (identifier? #'id) (runtime-of self)]
      [(id . args) (apply-proc self stx)])))

;; ---------------------------------------------------------------------------
;; Event types

;; fields: list of (symbol . domain-datum)
(struct type-static (name fields kind runtime-id)
  #:property prop:procedure
  (identifier-macro (λ (self) (type-static-runtime-id self))
                    (λ (self stx) (expand-event-constructor self stx))))

;; (Type #:at t [attr v] ...) builds an event, checking attributes at compile time.
(define (expand-event-constructor t stx)
  (syntax-parse stx
    [(_ (~alt (~optional (~seq #:at at:expr))
              (~optional (~seq #:ts ts:expr))
              (~optional (~seq #:te te:expr))
              (~optional (~seq #:n n:expr))
              (~optional (~seq #:id id:expr))
              (~optional (~seq #:src src:expr))
              [attr:id v:expr]) ...)
     (define names (map syntax-e (syntax->list #'(attr ...))))
     (define fields (type-static-fields t))
     (for ([a (syntax->list #'(attr ...))])
       (unless (assq (syntax-e a) fields)
         (raise-syntax-error #f (format "unknown attribute ~a for event type ~a; its attributes are ~a"
                                        (syntax-e a) (type-static-name t) (map car fields))
                             stx a)))
     (define dup (check-duplicates (syntax->list #'(attr ...)) #:key syntax-e))
     (when dup (raise-syntax-error #f "attribute given twice" stx dup))
     (for ([f fields])
       (unless (memq (car f) names)
         (raise-syntax-error #f (format "missing attribute ~a for event type ~a" (car f) (type-static-name t))
                             stx)))
     (unless (or (attribute at) (attribute ts))
       (raise-syntax-error #f "an event needs a time: give #:at or #:ts" stx))
     (with-syntax ([rt (type-static-runtime-id t)]
                   [name (type-static-name t)]
                   [ts* (or (attribute ts) (attribute at))]
                   [te* (or (attribute te) (attribute ts) (attribute at))]
                   [n* (or (attribute n) #'1)]
                   [id* (or (attribute id) #'#f)]
                   [src* (or (attribute src) #'#f)]
                   [srcloc (format "~a:~a" (syntax-source stx) (syntax-line stx))])
       (syntax/loc stx
         (checked-event rt (make-event 'name (list (cons 'attr v) ...)
                                       #:ts ts* #:te te* #:n n* #:id id* #:src src*)
                        srcloc)))]))

;; ---------------------------------------------------------------------------
;; Shapes: what events a stream may carry. A shape lists (type-name . fields)
;; for every type that can occur; fields of anonymous derived streams use the
;; stream's name.

(struct shape (types) #:transparent)   ; types: list of (symbol . fields)

(define (type-shape . ts)
  (shape (for/list ([t ts]) (cons (type-static-name t) (type-static-fields t)))))
(define (shape-type-names s) (map car (shape-types s)))
(define (shape-single s) (and (= 1 (length (shape-types s))) (car (shape-types s))))
(define (shape-attributes s)
  (remove-duplicates (append-map (λ (t) (map car (cdr t))) (shape-types s))))
(define (shape-has-attribute? s a) (and (memq a (shape-attributes s)) #t))
(define (shape-domain s a)
  (for*/first ([t (shape-types s)] [f (cdr t)] #:when (eq? (car f) a)) (cdr f)))

;; ---------------------------------------------------------------------------
;; Sources and streams

;; types: list of type-static
(struct source-static (name types runtime-id)
  #:property prop:procedure
  (identifier-macro (λ (self) (source-static-runtime-id self))
                    (λ (self stx) (raise-syntax-error #f "a source is not a procedure" stx))))

;; shape: a shape
(struct stream-static (name shape runtime-id)
  #:property prop:procedure
  (identifier-macro (λ (self) (stream-static-runtime-id self))
                    (λ (self stx) (raise-syntax-error #f "a stream is not a procedure" stx))))

(define (static-runtime-id v)
  (cond [(type-static? v) (type-static-runtime-id v)]
        [(source-static? v) (source-static-runtime-id v)]
        [(stream-static? v) (stream-static-runtime-id v)]))

(define (lookup-static id)
  (and (identifier? id) (syntax-local-value id (λ () #f))))

(define (lookup-type id [ctx id])
  (define v (lookup-static id))
  (unless (type-static? v)
    (raise-syntax-error #f (format "unknown event type ~a" (if (identifier? id) (syntax-e id) id))
                        ctx id))
  v)

(define (lookup-source id)
  (define v (lookup-static id))
  (unless (source-static? v)
    (raise-syntax-error #f "not a declared source" id))
  v)

;; A stream input: a source or a stream.
(define (lookup-upstream id)
  (define v (lookup-static id))
  (unless (or (source-static? v) (stream-static? v))
    (raise-syntax-error #f "not a declared source or stream" id))
  v)

;; ---------------------------------------------------------------------------
;; Value domains: (values datum runtime-expression)

(define base-domain-names '(Integer Real String Boolean Time Duration Location Any))

(define (parse-domain stx)
  (syntax-parse stx
    [d:id
     (unless (memq (syntax-e #'d) base-domain-names)
       (raise-syntax-error #f (format "unknown value domain; expected one of ~a, (Listof D), or (Record [a : D] ...)"
                                      base-domain-names)
                           stx))
     (values (syntax-e #'d) #`(quote d))]
    [((~datum Listof) d)
     (define-values (datum expr) (parse-domain #'d))
     (values (list 'Listof datum) #`(list-domain #,expr))]
    [((~datum Record) [a:id (~datum :) d] ...)
     (define dup (check-duplicates (syntax->list #'(a ...)) #:key syntax-e))
     (when dup (raise-syntax-error #f "duplicate attribute name" stx dup))
     (define parsed (for/list ([x (syntax->list #'(d ...))])
                      (call-with-values (λ () (parse-domain x)) cons)))
     (values (cons 'Record (for/list ([a (syntax->list #'(a ...))] [p parsed]) (cons (syntax-e a) (car p))))
             #`(record-domain (list #,@(for/list ([a (syntax->list #'(a ...))] [p parsed])
                                         #`(cons '#,a #,(cdr p))))))]
    [_ (raise-syntax-error #f "unknown value domain" stx)]))
