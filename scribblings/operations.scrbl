#lang scribble/manual
@(require (for-label racket/base occur))

@title[#:tag "operations"]{Single-Event Operations}

Spec §4. A filter returns the events it receives; every other operation
derives events whose constituent set is C = {e}, inheriting e's interval and
multiplicity unless it sets them.

Inside a stage, the attributes of the current event are variables, and the
event itself is @racket[it]. With a stream of a single type, reading an
attribute is strict; with several types, an attribute a type lacks reads as
@racket[#f]. Attributes shadow Racket bindings of the same name within the
stage.

@section{Stages}

@specform[(filter expr)]{@tt{OP.filter}: σ_p keeps e iff @racket[expr]
holds. @racket[(filter (> amount 1000))]}

@specform[(project attr ...+ maybe-as)
          #:grammar ([maybe-as (code:line) (code:line #:as type)])]{@tt{OP.project}:
π_A restricts the payload. Every attribute must be carried by every type of
the stream (checked at compile time).}

@specform[(map type [attr expr] ... time-option ...)
          #:grammar ([time-option (code:line #:ts expr) (code:line #:te expr) (code:line #:n expr)])]{
@tt{OP.map}: μ_f. The typed form builds an event of @racket[type] and must
give exactly its attributes.}

@specform[(map [attr expr] ...+ time-option ...)]{The untyped form updates
or adds attributes and keeps the event's type.}

@specform[(enrich #:key expr #:with lookup maybe-adds maybe-missing)
          #:grammar ([maybe-adds (code:line) (code:line #:adds ([attr : domain] ...))]
                     [maybe-missing (code:line) (code:line #:missing keep) (code:line #:missing drop)])]{
@tt{OP.enrich}: e ⊕ R(k(e)), where @racket[lookup] is a hash or a procedure
from the key to the attributes to add.}

@specform[(split type #:for (for-clause ...) [attr expr] ... #:ts expr time-option ... maybe-reference)
          #:grammar ([maybe-reference (code:line)
                                      (code:line #:reference substream-expr)
                                      (code:line #:reference substream-expr #:summary ([key expr] ...))])]{
@tt{OP.split}: e ↦ {e₁, …, eₙ}, one child of @racket[type] per iteration of
the @racket[for] clauses, each with a fresh id. With @racket[#:reference],
each child is a reference event (@tt{ES.reference}): its @racket[ref]
attribute names the sub-stream and the child's interval, with the summary,
in place of the samples; later stages read it through @racket[ref], for
example @racket[(hash-ref (data-ref-summary ref) 'newest)].}

@specform[(route maybe-mode [branch expr] ...+ maybe-otherwise)
          #:grammar ([maybe-mode (code:line) (code:line #:mode exclusive) (code:line #:mode inclusive)]
                     [maybe-otherwise (code:line) (code:line #:otherwise branch)])]{
@tt{OP.route}: Sᵢ = σ_{pᵢ}(S). The last stage of a stream; each branch is
declared as a stream of its own.}

@section{Runtime}

@defproc[(filter-operator [pred (-> event? any/c)]) operator?]{}
@defproc[(project-operator [attrs (listof symbol?)] [#:type type (or/c #f symbol?) #f]) operator?]{}
@defproc[(map-operator [f (-> event? (or/c hash? (listof pair?)))]
                       [#:type type (or/c #f symbol?) #f]
                       [#:ts ts (or/c #f (-> event? time?)) #f]
                       [#:te te (or/c #f (-> event? time?)) #f]
                       [#:n n (or/c #f (-> event? exact-nonnegative-integer?)) #f])
         operator?]{}
@defproc[(enrich-operator [lookup (-> any/c any/c)] [#:key key (-> event? any/c)]
                          [#:missing missing (or/c 'keep 'drop) 'keep]
                          [#:type type (or/c #f symbol?) #f])
         operator?]{}
@defproc[(split-operator [f (-> event? (listof event?))]) operator?]{}
@defproc[(route-operator [preds (listof (-> event? any/c))]
                         [#:mode mode (or/c 'exclusive 'inclusive) 'exclusive]
                         [#:otherwise? otherwise? boolean? #f])
         operator?]{}
