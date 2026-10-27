#lang racket/base
;; Shared test helpers.
;;
;; `spec-test` tags a test case with one or more spec IDs from docs/spec.md.
;; tests/coverage.rkt scans the test sources for these tags, so every ID in
;; docs/coverage.md must appear in at least one `(spec-test 'ID ...)` or
;; `(spec-test '(ID ...) ...)` form.
(require rackunit
         racket/port
         syntax/modread
         (for-syntax racket/base))
(provide spec-test
         trials
         spec-ids->string
         compile-occur
         eval-occur
         compile-error
         compile-warnings)

;; spec-ids->string : (or/c symbol? (listof symbol?)) -> string?
(define (spec-ids->string ids)
  (cond [(symbol? ids) (symbol->string ids)]
        [else (apply string-append
                     (for/list ([id (in-list ids)] [i (in-naturals)])
                       (string-append (if (zero? i) "" " ") (symbol->string id))))]))

;; (spec-test 'PT.sequence "description" body ...)
(define-syntax (spec-test stx)
  (syntax-case stx ()
    [(_ ids desc body ...)
     #'(test-case (string-append "[" (spec-ids->string ids) "] " desc)
         body ...)]))

;; The number of trials for each property test. CI and the definition of
;; done require at least 1000; OCCUR_TRIALS overrides it locally.
(define (trials)
  (define env (getenv "OCCUR_TRIALS"))
  (or (and env (string->number env)) 1000))

;; Expand a #lang occur module given as a string. Returns the expanded
;; syntax; raises the expansion error, if any.
(define (compile-occur src)
  (parameterize ([current-namespace (make-base-namespace)]
                 [read-accept-reader #t]
                 [read-accept-lang #t])
    (define in (open-input-string src))
    (port-count-lines! in)
    (define stx (with-module-reading-parameterization
                  (λ () (read-syntax 'occur-test in))))
    (expand stx)))

;; Compile and instantiate a #lang occur module given as a string; returns
;; the namespace it was instantiated in.
(define (eval-occur src)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns]
                 [read-accept-reader #t]
                 [read-accept-lang #t])
    (define in (open-input-string src))
    (port-count-lines! in)
    (define stx (with-module-reading-parameterization (λ () (read-syntax 'occur-test in))))
    (define name (syntax-e (cadr (syntax-e stx))))
    (eval stx)
    (namespace-require (list 'quote name))
    ns))

;; The message and the source line of the compile-time error raised while
;; expanding `src`, or #f when it compiles.
(define (compile-error src)
  (with-handlers ([exn:fail:syntax?
                   (λ (e)
                     (define exprs (exn:fail:syntax-exprs e))
                     (list (exn-message e)
                           (and (pair? exprs) (syntax-line (car exprs)))))])
    (compile-occur src)
    #f))

;; The compile-time warnings printed on stderr while expanding `src`.
(define (compile-warnings src)
  (define err (open-output-string))
  (parameterize ([current-error-port err]) (compile-occur src))
  (get-output-string err))
