#lang racket/base
;; The module language of #lang occur: racket/base, the public runtime API,
;; and the declaration forms. The runtime accessor `event-type` is renamed
;; `event-type-of` because `event-type` is the declaration form here.
(require (except-in "../main.rkt" event-type source)
         (only-in "../main.rkt" [event-type event-type-of])
         "../syntax/forms.rkt"
         "../syntax/module-begin.rkt")

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [occur-module-begin #%module-begin])
         (all-from-out "../main.rkt")
         event-type-of
         (all-from-out "../syntax/forms.rkt"))
