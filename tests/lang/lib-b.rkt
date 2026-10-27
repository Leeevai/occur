#lang occur
;; Another library: a condition name, and nothing to run.
(define-condition quiet-for #:as pt:silence #:rename ([#:seconds #:after]))
(provide quiet-for)
