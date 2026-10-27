#lang scribble/manual
@(require (for-label racket/base occur))

@title[#:tag "joins"]{Joins}

Spec §9. A join correlates the pipeline (the left stream s) with another
stream (the right stream r) on a shared key. The output is a derived event
with C = {s, r}, or C = {s} for an unmatched outer result.

@specform[(join other #:kind kind key-option option ...)
          #:grammar ([kind window interval table as-of]
                     [key-option (code:line #:key attr) (code:line #:keys (left-attr right-attr))]
                     [option (code:line #:on θ) (code:line #:window (tumbling #:size d))
                             (code:line #:lower d1) (code:line #:upper d2) (code:line #:valid-from expr)
                             (code:line #:outer left) (code:line #:outer right) (code:line #:outer full)
                             (code:line #:type type) (code:line #:select ([attr expr] ...))])]{
@itemlist[
 @item{@tt{JN.window}: s ⋈ r if θ(s, r) holds and both are in the same window.}
 @item{@tt{JN.interval}: s ⋈ r if θ(s, r) and t(r) ∈ [t(s) − Δ₁, t(s) + Δ₂].}
 @item{@tt{JN.table}: each s is matched against the table's current row;
       the right stream upserts rows.}
 @item{@tt{JN.as-of}: each s meets the version of r valid at t(s).}
 @item{@tt{JN.outer}: left, right, or full; unmatched events are emitted
       with nulls once their window closes.}]
In @racket[#:on] and @racket[#:select], the attributes of the two sides are
@racket[left.attr] and @racket[right.attr].}

@defproc[(window-join-operator [g grid?] [θ (-> event? event? any/c)] [#:key key (-> event? any/c)]
                               [#:outer outer outer/c #f])
         operator?]{}
@defproc[(interval-join-operator [lower duration?] [upper duration?] [θ (-> event? event? any/c)]
                                 [#:key key (-> event? any/c)] [#:outer outer outer/c #f])
         operator?]{}
@defproc[(table-join-operator [#:key key (-> event? any/c)] [#:outer outer (or/c #f 'left) #f]) operator?]{}
@defproc[(as-of-join-operator [#:key key (-> event? any/c)] [#:valid-from vf (-> event? time?) event-ts])
         operator?]{}
