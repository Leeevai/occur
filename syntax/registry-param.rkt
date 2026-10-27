#lang racket/base
;; The registry of the #lang occur module being expanded. #%module-begin
;; binds it with splicing-syntax-parameterize, so declarations produced by
;; library macros (lib/reflex.rkt) register in the module that uses them.
(require racket/stxparam
         (for-syntax racket/base))
(provide occur-registry)

(define-syntax-parameter occur-registry
  (λ (stx)
    (raise-syntax-error #f "occur declarations are only allowed in a #lang occur module" stx)))
