#lang scribble/manual
@(require (for-label racket/base occur))

@title[#:tag "patterns"]{Patterns}

Spec §11. A pattern recognizes combinations of events, and the meaningful
absence of events, and derives new events from its matches. The semantics
map a pattern P and a history H to a set of matches ⟦P⟧(H); a match is a set
of events with bindings and an interval.

@codeblock|{
(define-pattern card-testing
  #:from feed  #:within (minutes 10)
  (seq (a : Swipe (< amount 5))
       (absent Verified)
       (kleene+ (b : Swipe (> amount 500))))
  #:where (increasing b.amount)
  #:emit (SuspectedFraud [card a.card] [n (count b)] [total (sum b.amount)]))
}|

@defform[(define-pattern name option ... pattern option ...)
         #:grammar ([option (code:line #:from stream) (code:line #:key attr) (code:line #:within duration)
                            (code:line #:clock clock) (code:line #:selection strategy)
                            (code:line #:consumption policy) (code:line #:output policy)
                            (code:line #:skip strategy) (code:line #:eval timing) (code:line #:engine engine)
                            (code:line #:where θ) (code:line #:emit (type [attr expr] ...))]
                    [pattern (x : type) (x : type pred) type (type pred) act
                             (seq pattern-or-absent ...+) (and pattern ...+) (or pattern ...+)
                             (any k pattern ...+) (unless pattern pattern)
                             (kleene+ pattern) (kleene* pattern) (repeat pattern n) (repeat pattern n m)
                             (within pattern duration) (where pattern θ)
                             (aperiodic pattern pattern pattern) (aperiodic* pattern pattern pattern)
                             (periodic pattern duration pattern) (periodic* pattern duration pattern)]
                    [pattern-or-absent pattern (absent pattern) (absent pattern #:within duration)])]{

@tabular[#:style 'boxed
 (list (list @bold{Form} @bold{Spec} @bold{Meaning})
       (list @racket[(x : T pred)] "PT.primitive" "x:τ[p]: an event of type T satisfying pred, bound to x")
       (list @racket[act] "PT.activation" "the activation instant; matches no event")
       (list @racket[(seq P Q)] "PT.sequence" "te(m₁) < ts(m₂)")
       (list @racket[(and P Q)] "PT.conjunction" "disjoint, any order")
       (list @racket[(or P Q)] "PT.disjunction" "either")
       (list @racket[(seq P (absent Q) R)] "PT.negation" "no Q in the gap between P and R")
       (list @racket[(seq P (absent Q #:within d))] "PT.absence" "no Q within d after P; ends at the deadline")
       (list @racket[(unless P Q)] "PT.span-absence" "no Q inside the whole match")
       (list @racket[(kleene+ P)] "PT.kleene-plus, PT.kleene-star, PT.repeat" "one or more; kleene* and repeat")
       (list @racket[(any k P ...)] "PT.any" "k of the n patterns")
       (list @racket[(within P d)] "PT.within" "span at most d")
       (list @racket[(where P θ)] "PT.where" "θ over the bindings")
       (list @racket[(aperiodic S X E)] "PT.aperiodic, PT.aperiodic-cumulative" "X inside an S…E interval")
       (list @racket[(periodic S d E)] "PT.periodic, PT.periodic-cumulative" "ticks every d after S until E"))]

Primitive predicates see the event's attributes as variables. @racket[#:where]
and @racket[#:emit] see each binding @racket[x] and its attributes as
@racket[x.attr]; a Kleene binding is a list, and its attributes are lists of
values, usable with @racket[count] (the multiplicity sum), @racket[sum],
@racket[avg], @racket[min], @racket[max], @racket[span], @racket[median], and
the trend predicates. Using a Kleene binding where a single event is expected,
or a binding that is never bound, is a compile-time error. A pattern with
negation or Kleene closure and no time bound draws a compile-time warning,
because its state can grow without bound.}

@section{Derived families}

Spec §11.4. These predicates and constructors are available in
@hash-lang[] @racketmodname[occur]:
@racket[crosses?] and @racket[threshold-pattern] (@tt{PT.threshold});
@racket[increasing?], @racket[decreasing?], @racket[non-increasing?],
@racket[non-decreasing?], @racket[stable?], @racket[mixed?], and
@racket[trend] (@tt{PT.trend.*}); @racket[relative-max?] and
@racket[relative-min?] (@tt{PT.relative}); @racket[within-distance?],
@racket[beyond-distance?], @racket[min-distance], @racket[max-distance],
@racket[avg-distance] (@tt{PT.spatial.distance}); @racket[moving-toward?],
@racket[moving-away?], @racket[constant-direction?]
(@tt{PT.spatial.moving}); @racket[accumulate-pattern]
(@tt{PT.accumulate}), @racket[silence-pattern] (@tt{PT.silence}),
@racket[recovery-pattern] (@tt{PT.recovery}),
@racket[window-close-operator] (@tt{PT.window-close}), and calendar windows
(@tt{PT.schedule}).

@section{The incremental engine}

Patterns run on an incremental engine (@filepath{private/pattern/nfa.rkt}):
a tree of match automata, one per pattern node, each keeping buffers of the
sub-matches it has seen and returning only new matches as events arrive.
Negation, absence, span absence, and the aperiodic and periodic forms are
not monotone (@tt{ES.operator}): they hold candidates until the watermark
guarantees the answer (@tt{EVAL.deferred}). A hybrid-clock absence decides
on the processing clock, which is how a total silence is seen
(@tt{TM.idle}). Property tests check the engine against the oracle on
random patterns and histories. @racket[#:engine reference] runs the oracle
instead.

@section{Runtime}

@defproc[(denote [p pat?] [history (listof event?)] [#:activation t0 (or/c #f time?) #f]
                 [#:horizon h (or/c #f time?) #f])
         (listof pmatch?)]{The reference oracle: ⟦P⟧(H) by enumeration.}
@defproc[(derive-match [m pmatch?] [#:type type symbol? 'Match]) event?]{@tt{DR.derive}.}
@defproc[(make-engine [p pat?] [#:activation t0 (or/c #f time?) #f] [#:selection s symbol? 'any]) engine?]{}
@defproc[(engine-event [e engine?] [ev event?] [index exact-nonnegative-integer?])
         (values engine? (listof pmatch?))]{}
@defproc[(engine-advance [e engine?] [W time?] [now time?]) (values engine? (listof pmatch?))]{}
