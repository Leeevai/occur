#lang racket/base
;; lib/reflex.rkt and everything under examples/reflex/ use occur's public
;; forms only: no module path they require names a private module.
(require rackunit
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         syntax/modread)

(define-runtime-path root "..")

(define (reflex-files)
  (define lib (build-path root "lib" "reflex.rkt"))
  (define ex (build-path root "examples" "reflex"))
  (cons lib (if (directory-exists? ex)
                (for/list ([f (in-directory ex)] #:when (equal? (path-get-extension f) #".rkt")) f)
                '())))

;; every module path in every require-like form of the module
(define (required-paths stx)
  (define found '())
  (let walk ([d (syntax->datum stx)] [in-require? #f])
    (cond
      [(pair? d)
       (define head (car d))
       (define req? (or in-require? (memq head '(require for-syntax for-label for-template only-in except-in
                                                 prefix-in rename-in dynamic-require local-require))))
       (for ([x (in-list (if (list? d) d (list (car d) (cdr d))))]) (walk x req?))]
      [(and in-require? (or (string? d) (symbol? d))) (set! found (cons d found))]
      [else (void)]))
  found)

(define (read-module path)
  (parameterize ([read-accept-reader #t] [read-accept-lang #t])
    (call-with-input-file path
      (λ (in) (port-count-lines! in) (with-module-reading-parameterization (λ () (read-syntax path in)))))))

(define (private? p) (regexp-match? #rx"(^|/)private(/|$)" (format "~a" p)))

(test-case "lib/reflex.rkt and examples/reflex/ require no private module"
  (for ([f (reflex-files)])
    (define bad (filter private? (required-paths (read-module f))))
    (check-equal? bad '() (format "~a requires ~a" f bad))
    ;; nor names one anywhere in its text
    (check-false (regexp-match? #rx"private/" (file->string f)) (format "~a mentions private/" f))))

(test-case "the purity check notices a private require"
  (check-equal? (filter private? (required-paths #'(module m racket/base (require "../private/event.rkt" racket/list))))
                '("../private/event.rkt")))
