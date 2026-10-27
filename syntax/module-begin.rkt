#lang racket/base
;; #%module-begin for #lang occur.
;;
;; Every module gets its own registry. Declarations register into it, the
;; module provides `occur-program` and `occur-run`, and a `main` submodule
;; runs the program against the declared sources and prints the outputs.
(require racket/stxparam
         racket/splicing
         "registry-param.rkt"
         "../private/registry.rkt"
         (for-syntax racket/base))

(provide occur-module-begin)

(define-syntax (occur-module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     #'(#%module-begin
        (define reg (make-registry))
        (splicing-syntax-parameterize ([occur-registry (make-rename-transformer #'reg)])
          form ...)
        (define (occur-program) (registry-program reg))
        (define (occur-steps) (registry-steps reg))
        (define (occur-run) (run-registry reg))
        (provide occur-program occur-steps occur-run)
        (module+ main
          (print-run-result (run-registry reg))))]))
