#lang scribble/manual
@(require (for-label racket/base (except-in occur cron) occur/lib/reflex))

@title[#:tag "reflex"]{Reflex}

@defmodule[occur/lib/reflex]

Spec §16 describes Reflex, a stateful trigger engine: it turns data
arrivals into triggers, ``run job X for device k over this data, because of
reason R''. @filepath{lib/reflex.rkt} is Reflex as a library written in
@hash-lang[] @racketmodname[occur] with public forms only.
@filepath{docs/reflex.md} maps every row of §16.6 to the form below that
realizes it, and @filepath{examples/reflex/} holds the acceptance traces
R1 to R6 as programs.

@codeblock|{
#lang occur
(require occur/lib/reflex)

(define-devices devices ["d1" watched])
(define-reflex #:arrivals arrivals #:devices devices
               #:contributions contributions #:triggers triggers)

(reflex-rule threshold-check
  #:condition (accumulate #:min-samples 3 #:min-span-s 0)
  #:min-interval-s 60
  #:target "threshold-evaluator"
  #:applies-to ("d1"))

(script #:final-watermark? #f
 (at 0 arrivals (data-arrival 0 (sensor-write #:owner "d1" #:sensor "s1" #:type "load"
                                              #:n 1 #:first 0)))
 (advance-to 100))
}|

@section{Inputs and outputs (§16.1, §16.3)}

@defidform[DataArrival]{The event of one storage write: a reference payload
listing, per sensor written, the owning device, the sensor id, its
measurement type, the sample count, the interval @tt{[first_ts, last_ts]},
and the newest value.}

@defproc[(data-arrival [t time?] [write hash?] ...) event?]{The
@racket[DataArrival] of a write committed at @racket[t].}

@defproc[(sensor-write [#:owner owner string?] [#:sensor sensor string?] [#:type type string?]
                       [#:n n exact-nonnegative-integer?] [#:first first time?]
                       [#:last last time? first] [#:newest newest any/c #f])
         hash?]{One entry of a @racket[DataArrival]'s listing.}

@defidform[SensorData]{The batch event of one sensor written: n samples over
its interval, a reference to the sensor's samples (@tt{ES.reference}).}

@defidform[Trigger]{A trigger: target, device, binding key, reason,
interval, samples, windows, sources, params, emission time, and identity
(@tt{DR.derive}, @tt{DR.summary}, @tt{DR.output-identity}).}

@section{Devices}

@defform[(define-devices name [device status] ...)
         #:grammar ([status watched deprecated deleted])]{The device directory
(@tt{CX.entity}). A watched device is activated before it sends anything; a
deprecated device's state is dropped and its arrivals are ignored and
counted; a deleted device is forgotten, and activated afresh if it reports
again.}

@defproc[(push-devices [name symbol?] [entries (listof pair?)]
                       [#:mode mode (or/c 'replace 'patch) 'replace]
                       [#:at t (or/c #f time?) #f])
         step:control?]{A script step that replaces or patches the directory.}

@section{Conditions (§16.2)}

@defform[(accumulate #:min-samples n maybe-span)
         #:grammar ([maybe-span (code:line) (code:line #:min-span-s s)])]{
@tt{PT.accumulate} over @tt{WN.since-last}: ready once the contributions
since the last trigger reach @racket[n] samples and span @racket[s] seconds.
A binding may override both.}

@defform[(window #:window-s w #:offset-s o #:require-all all?)]{
@tt{PT.window-close}: a tumbling grid of @racket[w] seconds offset by
@racket[o], anchored at the first contribution, closed by the device's
keyed watermark over the watched kinds (all of them with
@racket[#:require-all #t], any otherwise), windows that closed together
coalesced.}

@defform[(silence #:after-s d maybe-recovery)
         #:grammar ([maybe-recovery (code:line) (code:line #:emit-recovery bool)])]{
@tt{PT.silence} on the hybrid clock, from the device's activation
(@tt{ACT}); with recovery, @tt{PT.recovery}; edge-triggered. A binding may
override @racket[d].}

@defform[(cron expr)]{@tt{PT.schedule}: calendar windows of a five-field
UTC cron expression in processing time, firing even when empty, missed
ticks coalesced.}

@section{Rules}

@defform[(reflex-rule id #:condition condition option ...)
         #:grammar ([option (code:line #:watch (type ...))
                            (code:line #:max-delay-s d)
                            (code:line #:reset-on-max-delay bool)
                            (code:line #:min-interval-s d)
                            (code:line #:on-update continue)
                            (code:line #:on-update restart)
                            (code:line #:target expr)
                            (code:line #:params ([key expr] ...))
                            (code:line #:applies-to targets)
                            (code:line #:id expr)
                            (code:line #:in-source)])]{
A rule template (@racket[rule-template]) with Reflex's modifiers:
@racket[#:max-delay-s] is the flush on the processing clock
(@tt{PT.flush}), @racket[#:reset-on-max-delay #f] retains the flushed
contributions (@tt{CON.retain}), @racket[#:min-interval-s] is a deferring
rate limit (@tt{OUT.rate-limit}), and @racket[#:on-update] the carry
policy (@tt{RL.carry}). Rule sources and bindings are @racket[rule-source]
and @racket[bind].}

@section{The program}

@defform[(define-reflex #:arrivals arrivals maybe-from #:devices devices
                        #:contributions contributions #:triggers triggers)
         #:grammar ([maybe-from (code:line) (code:line #:from data)])]{
Declares the Reflex program in the enclosing module: the
@racket[DataArrival] source @racket[arrivals]; the stream
@racket[contributions], which splits each arrival per sensor, keys the
sensors by the device that owns them, drops data stamped over an hour
ahead, gates by the directory @racket[devices], and merges each sensor by
its high-water mark; and the stream @racket[triggers] of the rule instances
in force, delivered by device.}

@defform[(define-fleet-health name #:from contributions #:per per
                              #:live d #:silent d #:stale d)
         #:grammar ([per device sensor])]{A view of each device's or each
sensor's health by arrival age and data age (live, late, quiet, or silent),
exported every minute (@tt{DR.view}, @tt{TM.schedule}).}
