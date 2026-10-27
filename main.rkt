#lang racket/base
;; occur: the public runtime API.
;;
;; Every module under private/ is internal. This module re-exports the
;; bindings that programs, libraries (such as lib/reflex.rkt), and tests may
;; use. The surface language (#lang occur) expands into calls to these
;; bindings.
