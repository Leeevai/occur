#lang racket/base
;; The module language of #lang occur: racket/base, the public runtime API,
;; and the declaration forms. The runtime accessor `event-type` is renamed
;; `event-type-of` and the ACT pattern value `pattern-act`, because
;; `event-type` and `act` are declaration forms here.
(require (except-in "../main.rkt" event-type source act)
         (only-in "../main.rkt" [event-type event-type-of] [act pattern-act])
         "../syntax/forms.rkt"
         "../syntax/module-begin.rkt")

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [occur-module-begin #%module-begin])
         (all-from-out "../main.rkt")
         event-type-of
         pattern-act
         (all-from-out "../syntax/forms.rkt"))
