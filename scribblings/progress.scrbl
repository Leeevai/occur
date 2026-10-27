#lang scribble/manual
@(require (for-label racket/base occur))

@title[#:tag "progress"]{Time and Progress}

Spec §6. All semantics default to event time (@tt{TM.event-time}). Every
timed stage declares its clock (@tt{TM.clock}) with
@racket[#:clock event-time], @racket[processing-time], or @racket[hybrid];
unknown clock names are compile-time errors. Processing time
(@tt{TM.processing-time}) is simulated by the runner.

@section{Watermarks}

A source declares a watermark strategy (@tt{TM.watermark}):
@racket[none] (only explicit @racket[(watermark source t)] steps),
@racket[ascending], @racket[(bounded-delay d)], or @racket[(heartbeat T)],
which advances the watermark to each @racket[T] event's time so event time
moves while data is idle (@tt{TM.idle}).

@specform[(keyed-watermark maybe-kinds maybe-mode)
          #:grammar ([maybe-kinds (code:line) (code:line #:kinds (kind ...))]
                     [maybe-mode (code:line) (code:line #:mode any) (code:line #:mode all)])]{
@tt{TM.keyed-watermark}: a stream of @tt{Progress} events, one whenever a
key's data-driven watermark advances. W^any is the newest kind, W^all the
slowest; a watched kind never seen is −∞.}

@section{Guards, lateness, timers, and idleness}

@specform[(skew-guard bound)]{@tt{TM.skew-guard}: drops and counts events
stamped more than @racket[bound] ahead of the clock. Sources accept
@racket[#:skew-guard bound] too.}

@specform[(lateness #:policy policy maybe-side-output)
          #:grammar ([policy drop side (allow L)]
                     [maybe-side-output (code:line) (code:line #:side-output name)])]{
@tt{TM.lateness}: an event with te < W is dropped and counted, sent to the
side-output stream @racket[name], or accepted within allowed lateness
@racket[L].}

@specform[(delay d maybe-clock)]{@tt{TM.timer}: releases each event from a
timer, at te + d once the watermark reaches it, or at arrival + d on the
processing clock.}

@specform[(idle d maybe-clock)]{@tt{TM.idle}: one @tt{Idle} event per key
once nothing has arrived for @racket[d] (processing clock) or once the
newest event time is @racket[d] behind the clock (hybrid).}

@section{Schedules}

@defform[(define-schedule name cron-expression)]{@tt{TM.schedule}: a stream
of @tt{Tick} events at every tick of a five-field cron expression read in
UTC. Ticks that came due while the engine was suspended fire once, with
their count in @tt{ticks}. Malformed expressions are compile-time errors.}

@defproc[(parse-cron [s string?]) cron?]{}
@defproc[(cron-next [c cron?] [t real?]) (or/c #f real?)]{The first tick after @racket[t].}
@defproc[(cron-prev [c cron?] [t real?]) (or/c #f real?)]{The last tick at or before @racket[t].}

@section{Retraction}

@tt{TM.retraction}: emitting ⊖e cancels a previously emitted e; a correction
is ⊖e_old followed by e_new. A source's data may contain
@racket[(retraction-of id)] (in JSON Lines, @tt{{"retract": id}}), and the
runner propagates the retraction to every event derived from it.

@defproc[(retraction-of [id any/c]) retraction?]{}
@defproc[(emit-correction! [ctx ctx?] [old event?] [new event?]) void?]{}
