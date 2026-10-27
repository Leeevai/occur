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

(spec-test '(PT.window-close WN.tumbling TM.keyed-watermark OUT.coalesce)
           "window(w, o, all?) is PT.window-close on a tumbling grid, closed under W_all or W_any"
  (check-equal? (condition-of "(window #:window-s 60 #:offset-s 0 #:require-all #t)")
                (list 'window-close (hasheq 'size 60 'offset 0 'mode 'all)))
  (check-equal? (condition-of "(window #:window-s 900 #:offset-s 30 #:require-all #f)")
                (list 'window-close (hasheq 'size 900 'offset 30 'mode 'any))))

(spec-test '(PT.silence PT.recovery PT.activation OUT.edge)
           "silence(Δ, recovery?) is PT.silence, with PT.recovery when emit_recovery"
  (check-equal? (condition-of "(silence #:after-s 600 #:emit-recovery #t)")
                (list 'silence (hasheq 'after 600 'recovery #t)))
  (check-equal? (condition-of "(silence #:after-s 300)") (list 'silence (hasheq 'after 300 'recovery #f))))

(spec-test '(PT.schedule WN.calendar TM.schedule)
           "cron(expr) is PT.schedule over the calendar windows of a five-field UTC expression"
  (check-equal? (condition-of "(cron \"0 * * * *\")") (list 'schedule (hasheq 'cron "0 * * * *"))))

(spec-test '(RL.template PT.flush CON.retain OUT.rate-limit RL.carry)
           "reflex-rule maps max_delay, reset_on_max_delay, min_interval, and on_update onto the template"
  (define found
    (found-in (string-append "(reflex-rule r #:condition (accumulate #:min-samples 3) #:watch (\"load\")\n"
                             "  #:max-delay-s 900 #:reset-on-max-delay #f #:min-interval-s 60 #:on-update continue\n"
                             "  #:target \"job\" #:params ([threshold 50]) #:applies-to (\"d1\"))\n"
                             "(define found (list (rule-id r-rule) (rule-watch r-rule) (rule-modifiers r-rule)\n"
                             "  (rule-on-update r-rule) (rule-target r-rule) (rule-params r-rule) (rule-applies-to r-rule)))")))
  (check-equal? found (list "r" '("load") (hasheq 'flush 900 'flush-retain? #t 'rate-limit '(60 . defer))
                            'continue "job" (hasheq 'threshold 50) '("d1")))
  ;; reset_on_max_delay true (the default) consumes: no retain
  (check-equal? (found-in (string-append "(reflex-rule r #:condition (accumulate #:min-samples 3) #:max-delay-s 900\n"
                                         "  #:reset-on-max-delay #t)\n(define found (rule-modifiers r-rule))"))
                (hasheq 'flush 900))
  (check-compile-error "#lang occur\n(require occur/lib/reflex)\n(reflex-rule r #:condition (accumulate #:min-samples 3)\n #:reset-on-max-delay #f)\n"
                       #rx"#:reset-on-max-delay needs #:max-delay-s" 4))

(spec-test '(CX.entity CX.activation) "the device directory is a CX.entity directory: watched, deprecated, deleted"
  (check-equal? (found-in (string-append "(define-devices devices [\"d1\" watched] [\"d2\" deprecated] [\"d3\" deleted])\n"
                                         "(define found (directory-entries devices-directory))"))
                '(("d1" . active) ("d2" . inactive) ("d3" . removed)))
  (check-equal? (found-in "(define found (directory-msg-entries (step:control-msg (push-devices 'devices '((\"d1\" . deleted)) #:mode 'patch))))")
                (hash "d1" 'removed)))
