#lang racket/base
;; Shared test helpers.
;;
;; `spec-test` tags a test case with one or more spec IDs from docs/spec.md.
;; tests/coverage.rkt scans the test sources for these tags, so every ID in
;; docs/coverage.md must appear in at least one `(spec-test 'ID ...)` or
;; `(spec-test '(ID ...) ...)` form.
(require rackunit
         (for-syntax racket/base))
(provide spec-test
         trials
         spec-ids->string)

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
