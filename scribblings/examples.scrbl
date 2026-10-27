#lang scribble/manual
@(require (for-label racket/base occur))

@title[#:tag "examples"]{Examples}

The @filepath{examples} directory holds complete @hash-lang[]
@racketmodname[occur] programs. Run one with @exec{racket
examples/card-testing.rkt}; each output line shows the processing time, the
node that produced it, and the event.

@section[#:tag "card-testing"]{Card testing (spec §15)}

@filepath{examples/card-testing.rkt} detects a tiny charge followed by
escalating large charges with no customer verification in between, per
card, within ten minutes. The pattern is written once, as a macro over the
selection strategy and the output policy:

@codeblock|{
(define-syntax-rule (card-testing name source selection output)
  (define-pattern name #:from source #:within (minutes 10)
    #:selection selection #:consumption unrestricted #:output output
    (seq (a : Swipe (< amount 5))
         (absent Verified)
         (kleene+ (b : Swipe (> amount 500))))
    #:where (increasing b.amount)
    #:emit (SuspectedFraud [card a.card] [n (count b)] [total (sum b.amount)])))
}|

The example instantiates it for every cell of trace A's table, and runs the
two negation scopes of trace A′ and the four strategies of trace B:

@tabular[#:style 'boxed
 (list (list @bold{Selection} @bold{every} @bold{maximal})
       (list "strict" "{e1, e2}" "{e1, e2}")
       (list "partition" "{e1, e2}" "{e1, e2}")
       (list "next" "{e1, e2}, {e1, e2, e4}" "{e1, e2, e4}")
       (list "any" "{e1, e2}, {e1, e4}, {e1, e2, e4}" "{e1, e2, e4}"))]

The maximal match derives @tt{SuspectedFraud(card = c1, n = 2, total =
1500)} over minutes 1 to 4, with constituents e1, e2, and e4, once the
watermark passes 11 minutes. With @tt{Verified(c1)} at 3.5 minutes,
@racket[(absent Verified)] between @racket[a] and @racket[b] blocks
@tt{{e1, e4}} only, while @racket[(unless P Verified)] over the whole pattern
keeps only @tt{{e1, e2}}. In trace B the verification between the swipes
prevents every match. @filepath{tests/card-testing-test.rkt} checks all of
this, on the incremental engine and on the reference oracle.
