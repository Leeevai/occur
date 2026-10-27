#lang racket/base
(require rackunit "util.rkt")

(spec-test 'ES.type "#:derived may appear before or after the attributes"
  (check-false (compile-error "#lang occur\n(event-type A #:derived [x : Real])\n(event-type B [x : Real] #:derived)\n")))
