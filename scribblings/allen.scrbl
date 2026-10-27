#lang scribble/manual
@(require (for-label racket/base occur))

@title[#:tag "allen"]{Allen's Interval Algebra}

Spec §10. For proper intervals a = [a⁻, a⁺] and b = [b⁻, b⁺], exactly one of
13 relations holds. Every function accepts intervals, events (their
occurrence intervals), or pairs, so the relations can be used in stage
expressions and pattern predicates: @racket[(filter (during? it shift))].

@tabular[#:style 'boxed
 (list (list @bold{Relation} @bold{Condition} @bold{Inverse})
       (list @racket[before?] "a⁺ < b⁻" @racket[after?])
       (list @racket[meets?] "a⁺ = b⁻" @racket[met-by?])
       (list @racket[overlaps?] "a⁻ < b⁻ < a⁺ < b⁺" @racket[overlapped-by?])
       (list @racket[starts?] "a⁻ = b⁻ and a⁺ < b⁺" @racket[started-by?])
       (list @racket[during?] "b⁻ < a⁻ and a⁺ < b⁺" @racket[contains?])
       (list @racket[finishes?] "a⁺ = b⁺ and b⁻ < a⁻" @racket[finished-by?])
       (list @racket[equals?] "a⁻ = b⁻ and a⁺ = b⁺" @racket[equals?]))]

Between two points only before, equals and after are distinguishable.
Between a point p and an interval b there are five relations: before,
starts, during, finishes, after.

@defproc[(allen-relation [a any/c] [b any/c]) symbol?]{The relation that holds.}
@defproc[(allen-inverse [r symbol?]) symbol?]{The inverse relation.}
