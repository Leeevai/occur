#lang info

;; occur is a single-collection package: every module lives under the
;; `occur` collection, so `(require occur)` loads main.rkt and
;; `#lang occur` loads lang/reader.rkt.
(define collection "occur")
(define pkg-desc
  "A Racket DSL for event processing: the event space, stream operators, windows, joins, a complete CEP pattern algebra, and rules as data")
(define version "0.0")
(define pkg-authors '("Leeevai"))
(define license 'MIT)

(define deps '("base"))
(define build-deps '("rackunit-lib"
                     "rackcheck-lib"
                     "scribble-lib"
                     "racket-doc"))

(define scribblings '(("scribblings/occur.scrbl" (multi-page) (language))))
