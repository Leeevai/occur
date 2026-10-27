#lang racket/base
;; #%module-begin for #lang occur.
;;
;; Every module gets its own registry. Declarations register into it, and a
;; module that declares something to run (a source, a stream, a rule
;; source, a script, ...) provides `occur-program`, `occur-steps`, and
;; `occur-run`, and has a `main` submodule that runs the program and prints
;; its outputs. A module that declares nothing to run is a library, such as
;; lib/reflex.rkt: it exports only what it provides, so several libraries
;; can be required together.
;;
;; Every declaration refers to the registry through the syntax parameter
;; `occur-registry`, so its expansion marks the module as a program. The
;; exports are a provide form, which Racket expands after the module body.
(require racket/stxparam
         racket/splicing
         racket/provide-syntax
         "registry-param.rkt"
         "../private/registry.rkt"
         (for-syntax racket/base racket/provide-transform))

(provide occur-module-begin)

;; (program-exports declared-id id ...): the ids, if the module declared
;; something to run.
(define-syntax program-exports
  (make-provide-transformer
   (λ (stx modes)
     (syntax-case stx ()
       [(_ declared id ...)
        (if (unbox (syntax-local-value #'declared))
            (expand-export #'(combine-out id ...) modes)
            '())]))))

(define-syntax (occur-module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     #'(#%module-begin
        (define reg (make-registry))
        (define-syntax declared (box #f))
        (splicing-syntax-parameterize ([occur-registry (λ (id)
                                                         (set-box! (syntax-local-value #'declared) #t)
                                                         #'reg)])
          form ...)
        (define (occur-program) (registry-program reg))
        (define (occur-steps) (registry-steps reg))
        (define (occur-run) (run-registry reg))
        (provide (program-exports declared occur-program occur-steps occur-run))
        (module+ main
          (print-run-result (run-registry reg))))]))
