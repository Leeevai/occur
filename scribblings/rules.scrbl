#lang scribble/manual
@(require (for-label racket/base occur))

@title[#:tag "rules"]{Rules as Data}

Spec §14. Rules say which operators run, for which keys, and with which
parameters; they arrive and change while the engine runs.

@codeblock|{
(rule-template threshold-check
  #:condition (pt:accumulate #:count 3 #:span 0)
  #:rate-limit (60 defer)
  #:target "threshold-evaluator"
  #:params ([threshold 50]))

(rule-source alarms
  (bind threshold-check #:on "d7" #:key "cfg-41" #:count 5 #:params ([threshold 40])))

(define-stream triggers (from arrivals) (rules))
}|

@defform[(define-condition name #:kind kind #:parameters ([keyword param maybe-default maybe-override] ...)
                           maybe-positional)]{Declares condition vocabulary
(@tt{RL.template}). Parameters marked @racket[#:override] may be overridden
by bindings (@tt{RL.binding}). @racket[(define-condition name #:as other #:rename ([kw other-kw maybe-convert] ...))]
renames another condition's keywords, and may convert a value with a
procedure, as @racket[[#:require-all #:mode (λ (all?) (if all? 'all 'any))]]
does; an overridable parameter cannot be converted. The core conditions are
@racket[pt:accumulate] (@tt{PT.accumulate}), @racket[pt:window-close]
(@tt{PT.window-close}), @racket[pt:silence] (@tt{PT.silence},
@tt{PT.recovery}), and @racket[pt:schedule] (@tt{PT.schedule}).}

@defform[(rule-template id #:condition condition option ...)]{Options:
@racket[#:watch], @racket[#:rate-limit (d mode)] (@tt{OUT.rate-limit}),
@racket[#:flush d] (@tt{PT.flush}), @racket[#:retain (max_delay)]
(@tt{CON.retain}), @racket[#:target], @racket[#:params],
@racket[#:applies-to], @racket[#:on-update] (@tt{RL.carry}), @racket[#:id]
(the rule id, by default the template's name), and @racket[#:in-source]. A
template is in force from the start, unless it has @racket[#:in-source]:
then only while a rule source that lists it is. Two templates with one
@racket[#:id] are two versions of a rule.}

@defform[(rule-source name maybe-at-start item ...)
         #:grammar ([maybe-at-start (code:line) (code:line #:at-start? bool)])]{Items are templates,
@racket[(bind template #:on key option ...)] bindings (@tt{RL.binding},
@tt{RL.fan-out} with @racket[#:also]), and @racket[(jsonl path)] data
(@tt{RL.source}). Sources are pushed at start, unless
@racket[#:at-start? #f]; @racket[name] is bound to the items, and scripts
push replacements and patches with @racket[rule-source-push], so an edit is
@racket[(rule-source-push 'alarms alarms/edited)]. Ownership (@tt{RL.ownership}),
dangling and invalid items (@tt{RL.dangling}), instance identity
(@tt{RL.identity}), fingerprints (@tt{RL.semantics}), re-judging
(@tt{RL.rejudge}), and convergence (@tt{RL.converge}) follow §14.}

@specform[(rules)]{The rules node: runs the instances in force on each key.}

@defproc[(rules-operator [#:key key (-> event? any/c)]) operator?]{}
@defproc[(rule-source-push [name any/c] [items list?] [#:mode mode (or/c 'replace 'patch) 'replace]
                           [#:at t (or/c #f time?) #f])
         step:control?]{}
