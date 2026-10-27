#lang scribble/manual
@(require (for-label racket/base occur))

@title[#:tag "runner"]{Operators, Programs, and the Runner}

@section{Operators (@tt{ES.operator})}

@defproc[(make-operator [name symbol?] [#:arity arity exact-positive-integer? 1]
                        [#:monotone? monotone? boolean? #t]
                        [#:key key (or/c #f (-> event? any/c)) #f]
                        [#:init-key init-key procedure? (λ (g k ctx) #f)]
                        [#:on-event on-event procedure? pass-through]
                        [#:on-timer on-timer procedure? (λ (g s ctx) s)]
                        [#:deadline deadline procedure? (λ (s) #f)]
                        [#:event-deadline event-deadline procedure? (λ (s) #f)])
         operator?]{An operator: callbacks over immutable per-key state. The
runner partitions it by @racket[key]. Callbacks emit with @racket[emit!].
Further keyword arguments install watermark, control, signal, and
retraction hooks.}

@defproc[(emit! [ctx ctx?] [item item?] [port exact-nonnegative-integer? 0]) void?]{
Emits an event, a @racket[retraction], or a @racket[signal].}

@section{Programs and scripts}

@defproc[(make-node [name symbol?] [op operator?] [inputs list?]
                    [#:sink? sink? boolean? #f]
                    [#:recursive bound (or/c #f exact-positive-integer?) #f])
         node?]{A node of the dataflow graph.}

@defproc[(make-program [roots list?]) program?]{Collects every source and
node reachable from @racket[roots]. A cycle is rejected unless it passes
through a node declared recursive (@tt{DR.cycle-guard}).}

@deftogether[(@defproc[(arrive [t time?] [source symbol?] [e event?]) step:arrive?]
              @defproc[(advance-to [t time?]) step:advance?]
              @defproc[(watermark-to [source symbol?] [t time?]) step:watermark?]
              @defproc[(suspend [t1 time?] [t2 time?]) step:suspend?])]{Script steps.}

@defproc[(run [prog program?] [steps list?]
              [#:clock clock simulated-clock? (simulated-clock 0)]
              [#:final-watermark? final? boolean? #t])
         run-result?]{Runs a script deterministically. Processing time is
simulated from the steps. Deadlines due before a step fire first, at their
own times; outputs of a step are delivered grouped by key; after a
suspension every overdue instance fires once.}
