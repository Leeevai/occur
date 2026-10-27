#lang racket/base
;; The vocabulary of lib/reflex.rkt expands into the core forms that spec
;; §16.2 names.
(require rackunit
         racket/list
         "util.rkt")

;; Evaluate a #lang occur module that requires the library; `found` is what
;; it provides.
(define (found-in body)
  (define ns (eval-occur (string-append "#lang occur\n(require occur/lib/reflex)\n" body "\n(provide found)\n")))
  (eval 'found ns))

(define (condition-of expr)
  (found-in (format "(define c ~a)\n(define found (list (condition-kind c) (condition-params c)))" expr)))

(spec-test '(PT.accumulate WN.since-last AG.multiplicity AG.span)
           "accumulate(N, S) is PT.accumulate with N samples and an S-second span"
  (check-equal? (condition-of "(accumulate #:min-samples 3 #:min-span-s 30)")
                (list 'accumulate (hasheq 'count 3 'span 30)))
  (check-equal? (condition-of "(accumulate #:min-samples 5)") (list 'accumulate (hasheq 'count 5 'span 0)))
  (check-compile-error "#lang occur\n(require occur/lib/reflex)\n(define c (accumulate #:count 3))\n"
                       #rx"unknown parameter #:count; expected \\(#:min-samples #:min-span-s\\)" 3))
