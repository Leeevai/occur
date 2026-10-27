#lang scribble/manual
@(require (for-label racket/base occur))

@title[#:tag "aggregation"]{Aggregation}

Spec §8. Aggregates are distributive (F(X ⊎ Y) = G(F(X), F(Y))), algebraic
(from a fixed-size tuple of distributive aggregates), holistic (no
constant-size partial state), or approximate (constant-size sketches).

@specform[(aggregate [name (agg arg ...)] ...+)]{After a
@racket[(window ...)] stage, one event per (key, window) with the named
results; on its own, a running aggregate per key.

@tabular[#:style 'boxed
 (list (list @bold{Aggregate} @bold{Spec ID} @bold{Class})
       (list @racket[(count)] "AG.count, AG.multiplicity (Σ n(e))" "distributive")
       (list @racket[(events)] "AG.events" "distributive")
       (list @racket[(sum e)] "AG.sum" "distributive")
       (list @racket[(min e)] " / (max e)" "distributive")
       (list @racket[(avg e)] "AG.avg" "algebraic")
       (list @racket[(variance e)] "AG.variance; also (stddev e), #:sample" "algebraic")
       (list @racket[(first e)] "AG.first / AG.last by event time" "distributive")
       (list @racket[(span)] "AG.span: max te − min ts" "algebraic")
       (list @racket[(sources)] "AG.sources" "distributive")
       (list @racket[(collect e)] "AG.collect" "holistic")
       (list @racket[(argmax e)] "AG.argmax / AG.argmin" "distributive")
       (list @racket[(median e)] "AG.median" "holistic")
       (list @racket[(percentile e p)] "AG.percentile (nearest rank)" "holistic")
       (list @racket[(count-distinct e)] "AG.count-distinct" "holistic")
       (list @racket[(mode e)] "AG.mode" "holistic")
       (list @racket[(top-k e k)] "AG.top-k, #:by value or frequency" "holistic")
       (list @racket[(approx-distinct e)] "AG.approx: HyperLogLog" "approximate")
       (list @racket[(approx-percentile e p)] "AG.approx: t-digest" "approximate")
       (list @racket[(approx-frequency e x)] "AG.approx: Count-Min" "approximate"))]}

Incremental maintenance (@tt{AG.incremental}): invertible aggregates
support remove, which makes sliding count windows cheap; min and max use a
two-stack queue (or a monotonic deque); holistic aggregates keep their
values.

@defproc[(make-aggregator [name symbol?] [class symbol?] [#:init init (-> any/c)]
                          [#:add add (-> any/c event? any/c)] [#:result result (-> any/c any/c) values]
                          [#:remove remove (or/c #f procedure?) #f] [#:combine combine (or/c #f procedure?) #f])
         aggregator?]{}
@defproc[(aggregate-events [agg aggregator?] [events (listof event?)]) any/c]{The batch definition.}
@defproc[(aggregate-accumulator [named (listof (cons/c symbol? aggregator?))]) accumulator?]{
Named aggregates as a window accumulator.}
