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

@section[#:tag "checkpoint"]{Checkpoint and replay (@tt{TM.checkpoint}, @tt{TM.replay})}

The runner keeps every data, watermark, control, and clock step it has run:
this log is the retained input. Three steps act on it. In a
@racket[script] they are written @racket[(checkpoint)], @racket[(restore)],
and @racket[(cold-start)].

@deftogether[(@defproc[(checkpoint) step:checkpoint?]
              @defproc[(restore) step:restore?]
              @defproc[(cold-start) step:cold-start?])]{
@racket[checkpoint] delivers pending outputs, then snapshots every node's
state, the deadlines, watermarks, counters, provenance, the id counter, and
the read position. @racket[restore] returns to the last snapshot and
re-reads the input logged after it, as after a crash: the outputs of that
interval repeat with the same identities, and @racket[dedupe-outputs]
removes the repeats. It is an error without an earlier checkpoint.

@racket[cold-start] rebuilds every node from its initial state by
replaying the whole log with processing time frozen. Processing-time
deadlines wait until the replay has caught up, so replayed arrivals are
not taken for silence or flush deadlines. Outputs produced meanwhile are
held. They are released at the next configuration push when the program
has a configuration operator, otherwise as soon as the replay has caught
up. Released outputs are coalesced to one per operator instance
(@tt{OUT.coalesce}) and judged against the instances then in force; an
output whose instance is gone is dropped and counted as
@racket['dropped-after-replay].}

@codeblock|{
(script
 (at 1 arrivals (reading 1 1))
 (at 2 arrivals (reading 2 1))        ; trigger 1
 (checkpoint)
 (at 3 arrivals (reading 3 1))
 (at 4 arrivals (reading 4 1))        ; trigger 2
 (restore)                            ; back to the checkpoint; 3 and 4 are re-read
 (cold-start)                         ; no checkpoint: replay everything
 (rule-source-push 'alarms alarms))   ; held triggers are released here
}|
