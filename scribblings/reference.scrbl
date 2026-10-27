#lang scribble/manual
@(require (for-label racket/base racket/contract occur))

@title[#:tag "reference"]{Runtime Reference}

The definitions the other chapters refer to: the runtime's data types,
predicates, and helpers, grouped by the part of the specification they
belong to. Everything here is provided by @racketmodname[occur] and
available in @hash-lang[] @racketmodname[occur] expressions.

@section[#:tag "ref-events"]{Events and types (§2)}

@defproc[(event? [v any/c]) boolean?]{An event ⟨id, τ, v, ts, te, n, src, C⟩
(@tt{ES.event}). Build one with @racket[make-event]. Its fields are read with
@racketidfont{event-id}, @racketidfont{event-type} (in @hash-lang[]
@racketmodname[occur], @racketidfont{event-type-of}), @racketidfont{event-payload},
@racket[event-ts], @racketidfont{event-te}, @racketidfont{event-n},
@racketidfont{event-src}, and @racketidfont{event-constituents}.}

@defproc[(event [id any/c] [type symbol?] [payload (and/c hash? immutable?)] [ts time?] [te time?]
                 [n exact-nonnegative-integer?] [src any/c] [constituents (listof event?)])
         event?]{The raw constructor, without the checks and defaults of
@racket[make-event], which programs use instead.}
@defproc[(event-ts [e event?]) time?]{The start of an event's occurrence interval.}
@defproc[(event-attr [e event?] [a symbol?]) any/c]{The value of attribute @racket[a];
an error if the event does not carry it. @racketidfont{event-attr/default} takes a default.}
@defproc[(location? [v any/c]) boolean?]{A point in the plane, built with
@racketidfont{location} and compared by Euclidean distance (@tt{CX.spatial}).}

@defproc[(event-type? [v any/c]) boolean?]{An event type (@tt{ES.type}).}
@defproc[(domain? [v any/c]) boolean?]{A value domain of an attribute.}
@defproc[(type-registry? [v any/c]) boolean?]{A set of declared event types.}

@defstruct*[data-ref ([substream any/c] [interval interval?] [summary hash?])]{
A reference payload (@tt{ES.reference}): a pointer at a sub-stream's
samples over an interval, with a small summary.}

@defproc[(make-point [t time?]) interval?]{The point interval [t, t].}
@defproc[(event-stream? [v any/c]) boolean?]{A stream of events of declared types (@tt{ES.stream}).}
@defproc[(keying? [v any/c]) boolean?]{How a stream's events name their entity, sub-stream, and kind (@tt{ES.substream}).}

@defstruct*[causality ([edges (listof pair?)] [closure hash?])]{A causality
relation (@tt{ES.causality}): cause and effect ids with their transitive
closure.}

@section[#:tag "ref-runner"]{Operators, programs, and runs (§2, §6)}

@defproc[(operator? [v any/c]) boolean?]{An operator (@tt{ES.operator}).}
@defproc[(ctx? [v any/c]) boolean?]{The context an operator callback runs
in: its node, key, port, clock, processing time, watermark, and arrival
sequence number.}
@defproc[(item? [v any/c]) boolean?]{What flows along an edge: an event, a
@racket[retraction], or a @racket[signal].}
@defstruct*[retraction ([event event?])]{⊖e: cancels a previously emitted e (@tt{TM.retraction}).}
@defstruct*[signal ([kind symbol?] [key any/c] [time (or/c #f time?)] [data any/c])]{An
in-band lifecycle signal, such as an entity's activation (@tt{CX.activation}).}
@defproc[(node? [v any/c]) boolean?]{A node of a dataflow program.}
@defproc[(program? [v any/c]) boolean?]{A dataflow program.}
@defstruct*[simulated-clock ([start time?])]{The simulated processing clock of a run.}
@defstruct*[output ([at time?] [node symbol?] [key any/c] [item item?])]{One
output of a run: when it was delivered, by which node, under which key.}
@defstruct*[run-result ([outputs (listof output?)] [counters hash?] [reports list?] [now time?]
                        [watermarks hash?] [states hash?])]{The result of
@racket[run].}
@defproc[(run-result-history [r run-result?] [node symbol?] [t time?]) (listof event?)]{
@tt{ES.history}: the events @racket[node] emitted and did not retract whose
end time is at most @racket[t].}

@deftogether[(@defproc[(step:arrive? [v any/c]) boolean?]
              @defproc[(step:advance? [v any/c]) boolean?]
              @defproc[(step:watermark? [v any/c]) boolean?]
              @defproc[(step:suspend? [v any/c]) boolean?]
              @defproc[(step:control? [v any/c]) boolean?]
              @defproc[(step:checkpoint? [v any/c]) boolean?]
              @defproc[(step:restore? [v any/c]) boolean?]
              @defproc[(step:cold-start? [v any/c]) boolean?])]{The kinds of script step.}

@defstruct*[arrival ([at time?] [event (or/c event? retraction?)])]{An event, or a
retraction, with the processing time it arrives at; in a source's
@racket[(events ...)] data, @racket[(arrival at e)].}

@section[#:tag "ref-windows"]{Windows, contexts, and aggregation (§7, §8)}

@defstruct*[grid ([size (and/c real? positive?)] [step (and/c real? positive?)] [offset real?])
            #:omit-constructor]{A grid of windows [kσ + o, kσ + o + Δ).}
@defproc[(tumbling [size (and/c real? positive?)] [#:offset offset real? 0]) grid?]{@tt{WN.tumbling}.}
@defproc[(sliding [size (and/c real? positive?)] [slide (and/c real? positive?)] [#:offset offset real? 0])
         grid?]{@tt{WN.sliding}.}
@defstruct*[accumulator ([init procedure?] [add procedure?] [result procedure?]
                         [remove (or/c #f procedure?)] [combine (or/c #f procedure?)])
            #:omit-constructor]{What a window accumulates; see @racket[make-accumulator].}
@defstruct*[assigner ([name symbol?] [init procedure?] [assign procedure?]) #:omit-constructor]{A
context assigner: which context instances an event belongs to.}
@defproc[(directory-push [name any/c] [entries (or/c hash? list?)]
                         [#:mode mode (or/c 'replace 'patch) 'replace] [#:at t (or/c #f time?) #f])
         step:control?]{A script step replacing or patching an entity directory (@tt{CX.entity}).}
@defproc[(cron? [v any/c]) boolean?]{A parsed five-field cron expression (@tt{TM.schedule}).}
@defstruct*[aggregator ([name symbol?] [class (or/c 'distributive 'algebraic 'holistic 'approximate)]
                        [init procedure?] [add procedure?] [remove (or/c #f procedure?)]
                        [combine (or/c #f procedure?)] [result procedure?])
            #:omit-constructor]{An aggregate; see @racket[make-aggregator].}

@section[#:tag "ref-structure"]{Stream structure and joins (§5, §9)}

@defstruct*[hwm-state ([hw time?] [late exact-nonnegative-integer?] [contributions exact-nonnegative-integer?])]{
A sub-stream's high-water mark, late count, and number of contributions
(@tt{ST.monotonic-merge}).}
@defthing[throttle-mode/c contract?]{How @racket[throttle-operator] samples:
@racket[(list 'rate n per)], @racket[(list 'rate n per clock)],
@racket[(list 'every n)], or @racket[(list 'probability p seed)].}
@defthing[outer/c contract?]{An outer join's side: @racket[#f], @racket['left], @racket['right], or @racket['full].}

@section[#:tag "ref-allen"]{Allen relations (§10)}

@deftogether[(@defproc[(before? [a (or/c event? interval?)] [b (or/c event? interval?)]) boolean?]
              @defproc[(after? [a (or/c event? interval?)] [b (or/c event? interval?)]) boolean?]
              @defproc[(meets? [a (or/c event? interval?)] [b (or/c event? interval?)]) boolean?]
              @defproc[(met-by? [a (or/c event? interval?)] [b (or/c event? interval?)]) boolean?]
              @defproc[(overlaps? [a (or/c event? interval?)] [b (or/c event? interval?)]) boolean?]
              @defproc[(overlapped-by? [a (or/c event? interval?)] [b (or/c event? interval?)]) boolean?]
              @defproc[(starts? [a (or/c event? interval?)] [b (or/c event? interval?)]) boolean?]
              @defproc[(started-by? [a (or/c event? interval?)] [b (or/c event? interval?)]) boolean?]
              @defproc[(during? [a (or/c event? interval?)] [b (or/c event? interval?)]) boolean?]
              @defproc[(contains? [a (or/c event? interval?)] [b (or/c event? interval?)]) boolean?]
              @defproc[(finishes? [a (or/c event? interval?)] [b (or/c event? interval?)]) boolean?]
              @defproc[(finished-by? [a (or/c event? interval?)] [b (or/c event? interval?)]) boolean?]
              @defproc[(equals? [a (or/c event? interval?)] [b (or/c event? interval?)]) boolean?])]{
The thirteen relations of Allen's interval algebra (@tt{AL.*}), over
events (their [ts, te]) and intervals. Exactly one holds for any two.}

@section[#:tag "ref-patterns"]{Patterns (§11)}

@defproc[(pat? [v any/c]) boolean?]{A pattern of the algebra of §11.2.}
@defproc[(seq [p pat?] [q pat?] ...) pat?]{@tt{PT.sequence}: P ; Q ; ….}
@defproc[(kleene+ [p pat?]) pat?]{@tt{PT.kleene-plus}: P⁺.}
@defstruct*[pmatch ([events (listof event?)] [bindings hash?] [ts time?] [te time?] [leaves list?])
            #:omit-constructor]{A match of a pattern: its events, bindings, and interval.}
@defproc[(engine? [v any/c]) boolean?]{The state of the incremental engine for one pattern instance.}

@defproc[(crosses? [v (or/c #f real?)] [op (or/c '> '>= '< '<= '=)] [k real?]) boolean?]{@tt{PT.threshold}: whether op(v, k) holds.}
@defproc[(threshold-pattern [p pat?] [agg (-> hash? (or/c #f real?))] [op (or/c '> '>= '< '<= '=)] [k real?]) pat?]{
P where op(agg(bindings), k).}
@deftogether[(@defproc[(increasing? [xs (listof real?)]) boolean?]
              @defproc[(decreasing? [xs (listof real?)]) boolean?]
              @defproc[(non-increasing? [xs (listof real?)]) boolean?]
              @defproc[(non-decreasing? [xs (listof real?)]) boolean?]
              @defproc[(stable? [xs (listof real?)] [eps real? 0]) boolean?]
              @defproc[(mixed? [xs (listof real?)] [eps real? 0]) boolean?]
              @defproc[(trend [xs (listof real?)] [eps real? 0]) symbol?])]{
@tt{PT.trend.*}: trends over the successive values of a Kleene binding. In
@racket[#:where] clauses they are written without the question mark, as
@racket[(increasing b.amount)].}
@deftogether[(@defproc[(relative-max? [x any/c] [xs list?] [key (-> any/c real?) values]) boolean?]
              @defproc[(relative-min? [x any/c] [xs list?] [key (-> any/c real?) values]) boolean?])]{
@tt{PT.relative}: whether @racket[x] is at least as large (small) as every element of @racket[xs].}
@deftogether[(@defproc[(within-distance? [a location?] [b location?] [r real?]) boolean?]
              @defproc[(beyond-distance? [a location?] [b location?] [r real?]) boolean?]
              @defproc[(min-distance [locs (non-empty-listof location?)] [anchor location?]) real?]
              @defproc[(max-distance [locs (non-empty-listof location?)] [anchor location?]) real?]
              @defproc[(avg-distance [locs (non-empty-listof location?)] [anchor location?]) real?])]{
@tt{PT.spatial.distance}.}
@deftogether[(@defproc[(moving-toward? [locs (listof location?)] [anchor location?]) boolean?]
              @defproc[(moving-away? [locs (listof location?)] [anchor location?]) boolean?]
              @defproc[(constant-direction? [locs (listof location?)] [#:tolerance tolerance real? 0.1]) boolean?])]{
@tt{PT.spatial.moving}.}
@defproc[(accumulate-pattern [type symbol?] [n real?] [s real?] [#:bind x symbol? 'b]) pat?]{
@tt{PT.accumulate} as a pattern: X⁺ where count ≥ n and span ≥ s.}
@defproc[(silence-pattern [type symbol?] [d duration?] [#:bind x symbol? 'x]) pat?]{
@tt{PT.silence} as a pattern: (ACT ∨ x:X) ; ¬X within d on the hybrid clock.}
@defproc[(recovery-pattern [silence-type symbol?] [type symbol?]) pat?]{@tt{PT.recovery}: a silence, then the first X.}
@defproc[(window-close-operator [size (and/c real? positive?)] [#:kind kind (-> event? any/c)]
                                [#:key key (-> event? any/c)])
         operator?]{@tt{PT.window-close} as an operator.}

@section[#:tag "ref-derivation"]{Derivation (§13)}

@defproc[(summarize [e event?] [#:sources-of sources-of (or/c #f (-> event? any/c)) #f]) event?]{
@tt{DR.summary}: e with its constituents replaced by a summary.}
@defproc[(summarized-output [type symbol?] [payload (or/c hash? list?)] [#:ts ts time?] [#:te te time?]
                            [#:samples samples exact-nonnegative-integer?]
                            [#:merged merged exact-nonnegative-integer? 1] [#:sources sources list? '()]
                            [#:src src any/c #f])
         event?]{A derived event carrying a summary instead of constituents.}
@defproc[(event-identity [e event?]) string?]{@tt{DR.output-identity}: an event's content identity.}
@defproc[(dedupe-outputs [outputs (listof output?)] [#:identity identity (-> event? any/c) event-identity])
         (listof output?)]{Downstream deduplication: drop outputs whose identity was seen.}
@defproc[(delivery-groups [outputs (listof output?)]) (listof (cons/c any/c (listof output?)))]{
@tt{DR.ordered}: outputs grouped by key, in order within each key.}
@defproc[(classify-health [#:now now time?] [#:last-arrival last-arrival (or/c #f time?)] [#:newest newest time?]
                          [#:live live duration?] [#:silent silent duration?] [#:stale stale duration?])
         (or/c 'live 'late 'quiet 'silent)]{A health classification by arrival
age and data age, for @tt{DR.view}.}
