#lang scribble/manual
@(require (for-label racket/base racket/contract occur))

@title[#:tag "event-space"]{The Event Space}

The event space 𝓔 (spec §2) contains every event an engine could observe or
produce. A run observes a tiny subset of it.

@section{Time (@tt{ES.time})}

The time domain is the extended reals in seconds: any non-NaN real, plus
@racket[-inf.0] and @racket[+inf.0]. Exact numbers are preserved.

@defproc[(time? [v any/c]) boolean?]{Recognizes time points.}
@defproc[(duration? [v any/c]) boolean?]{Recognizes non-negative times.}
@deftogether[(@defproc[(milliseconds [n real?]) duration?]
              @defproc[(seconds [n real?]) duration?]
              @defproc[(minutes [n real?]) duration?]
              @defproc[(hours [n real?]) duration?]
              @defproc[(days [n real?]) duration?]
              @defproc[(weeks [n real?]) duration?])]{Durations in seconds:
@racket[(minutes 10)] is @racket[600].}

@defstruct*[interval ([start time?] [end time?])]{A closed occurrence
interval with @racket[start] at most @racket[end]. A point is the degenerate
interval; see @racket[make-point].}

@defproc[(utc [year exact-integer?] [month (integer-in 1 12)] [day (integer-in 1 31)]
              [hour (integer-in 0 23) 0] [minute (integer-in 0 59) 0] [second real? 0])
         time?]{Seconds since the Unix epoch for a UTC civil time.}

@defproc[(iso8601->seconds [s string?]) time?]{Parses a UTC ISO-8601 timestamp.}

@section{Types and schemas (@tt{ES.type})}

@defproc[(make-event-type [name symbol?] [fields (listof (cons/c symbol? domain?))]
                          [#:derived? derived? boolean? #f])
         event-type?]{A type with a schema of distinct attributes. Domains are
@racket['Integer], @racket['Real], @racket['String], @racket['Boolean],
@racket['Time], @racket['Duration], @racket['Location], @racket['Any], record
domains, and list domains.}

@defproc[(payload-problem [t event-type?] [v any/c]) (or/c #f string?)]{
Returns @racket[#f] when @racket[v] is in the payload domain D_τ, or a
description of why it is not.}

@section{Events (@tt{ES.event}, @tt{ES.batch}, @tt{ES.reference}, @tt{ES.composite})}

An event is the tuple ⟨id, τ, v, ts, te, n, src, C⟩.

@defproc[(make-event [type symbol?] [payload (or/c hash? (listof pair?))]
                     [#:id id any/c #f] [#:at at (or/c #f time?) #f]
                     [#:ts ts (or/c #f time?) at] [#:te te (or/c #f time?) ts]
                     [#:n n exact-nonnegative-integer? 1] [#:src src any/c #f]
                     [#:constituents cs (listof event?) '()])
         event?]{Builds an event. Ids are fresh and deterministic within a run.}

@defproc[(batch-share [e event?] [b time?]) exact-nonnegative-integer?]{The
number of the samples of batch event @racket[e] lying strictly past
boundary @racket[b]:
0 if n = 0 or te ≤ b; n if ts > b; otherwise
min(n, max(1, round(n · (te − b) / (te − ts)))) with round-half-to-even.}

@defproc[(make-reference-event [type symbol?] [#:substream s any/c] [#:ts ts time?]
                               [#:te te time? ts] [#:n n exact-nonnegative-integer? 1]
                               [#:summary summary (or/c hash? list?) (hasheq)]
                               [#:payload extra (or/c hash? list?) '()]
                               [#:id id any/c #f] [#:src src any/c #f])
         event?]{An event that points at stored data instead of carrying it.}

@defproc[(make-composite [type symbol?] [payload (or/c hash? (listof pair?))]
                         [constituents (listof event?)]
                         [#:ts ts (or/c #f time?) #f] [#:te te (or/c #f time?) #f]
                         [#:n n exact-nonnegative-integer? 1] [#:id id any/c #f]
                         [#:src src any/c #f])
         event?]{A derived event; its interval defaults to the hull of the
constituents and its multiplicity to 1.}

@section{The inductive space (@tt{ES.space})}

@defproc[(event-level [e event?]) exact-nonnegative-integer?]{The abstraction
level: 0 for primitive events, one more than the highest constituent for
derived ones.}

@defproc[(event-space-member? [e event?] [registry type-registry?]) boolean?]{
Membership in 𝓔 by the inductive definition.}

@section{Relations (@tt{ES.precedes}, @tt{ES.membership}, @tt{ES.causality})}

@defproc[(precedes? [a event?] [b event?]) boolean?]{a ≺ b iff te(a) < ts(b).}
@defproc[(member*? [a event?] [c event?]) boolean?]{a ∈* c: a is in the
provenance of c.}
@defproc[(causes? [g causality?] [a event?] [b event?]) boolean?]{a →⁺ b in
the causality @racket[g].}

@section{Streams (@tt{ES.stream}, @tt{ES.history}, @tt{ES.keyed}, @tt{ES.substream})}

@defproc[(history [s event-stream?] [t time?]) (listof event?)]{H_S(t), the
events with te ≤ t.}
@defproc[(partition-stream [s event-stream?] [κ (-> event? any/c)])
         (listof (cons/c any/c event-stream?))]{The substreams S_k.}
@defproc[(attribute-keying [#:entity entity symbol?] [#:substream sub (or/c #f symbol?) #f]
                           [#:kind kind (or/c #f symbol?) #f])
         keying?]{Hierarchical keys: an entity owns sub-streams, each with a kind.}
