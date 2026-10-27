#lang scribble/manual
@(require (for-label racket/base occur))

@title[#:tag "structure"]{Stream Structure}

Spec §5. These stages change how streams are combined, partitioned, and
ordered.

@section{Stages and source options}

@specform[(merge stream ...+)]{@tt{ST.merge}: the union of streams,
interleaved by arrival. As the first stage it merges the named streams;
later it merges the pipeline so far with them.}

@specform[(key-by attr maybe-substream)
          #:grammar ([maybe-substream (code:line) (code:line #:substream sub-attr)
                                      (code:line #:substream sub-attr #:kind kind-attr)])]{
@tt{ST.key-by}: partitions the stream by an attribute. Downstream stateful
stages run per key. @racket[(key-by #:fn expr)] keys by any expression.
Re-keying is the same operation. With @racket[#:substream] and
@racket[#:kind], the key is an entity that owns sub-streams, each of a kind
(@tt{ES.substream}), as a source's @racket[#:key], @racket[#:substream], and
@racket[#:kind] declare them; the merge and the rules node read them.}

@specform[(dedupe maybe-key #:horizon duration maybe-clock)
          #:grammar ([maybe-key (code:line) (code:line #:key expr)]
                     [maybe-clock (code:line) (code:line #:clock event-time)
                                  (code:line #:clock processing-time)])]{
@tt{ST.dedupe}: drops an event when an earlier one with the same key lies
within the horizon. The default key is the event id.}

@specform[(monotonic-merge maybe-substream)
          #:grammar ([maybe-substream (code:line) (code:line #:substream expr)])]{
@tt{ST.monotonic-merge}: high-water-mark merging per sub-stream. Downstream
stages see contributions: the share of each arrival past the sub-stream's
newest time. Data wholly behind the mark is dropped and counted as
@tt{late}. On a stream with an entity, the merge is keyed by the entity, so
dropping the entity (@tt{CX.entity}) forgets its sub-streams' marks.}

@specform[(reorder maybe-late)
          #:grammar ([maybe-late (code:line) (code:line #:late drop) (code:line #:late emit)])]{
@tt{ST.reorder}: releases events in event-time order once the watermark
reaches them.}

@specform[(throttle #:rate r #:per duration maybe-clock)]{@tt{ST.throttle}:
at most @racket[r] events per epoch-aligned period. The other forms are
@racket[(throttle #:every n)] and
@racket[(throttle #:probability p #:seed s)].}

A @racket[source] may declare @racket[#:key], @racket[#:substream], and
@racket[#:kind] attributes (@tt{ES.substream}) and
@racket[#:merge high-water-mark], which makes every stream that reads the
source see the merged contributions.

@section[#:tag "structure-runtime"]{Runtime}

@defproc[(merge-operator [n exact-positive-integer?]) operator?]{}
@defproc[(key-by-operator [κ (-> event? any/c)]) operator?]{}
@defproc[(dedupe-operator [key (-> event? any/c)] [#:horizon h duration?]
                          [#:clock clock (or/c 'event-time 'processing-time) 'event-time])
         operator?]{}
@defproc[(monotonic-merge-operator [substream (-> event? any/c)]
                                   [#:entity entity (or/c #f (-> event? any/c)) #f])
         operator?]{The per-sub-stream state is a @racket[hwm-state] with the
high-water mark, the late count, and the number of contributions. With
@racket[#:entity], the operator is keyed by the entity and keeps a hash from
sub-stream to @racket[hwm-state].}
@defproc[(reorder-operator [#:key key (or/c #f (-> event? any/c)) #f]
                           [#:late late (or/c 'drop 'emit) 'drop])
         operator?]{}
@defproc[(throttle-operator [mode throttle-mode/c] [#:key key (or/c #f (-> event? any/c)) #f])
         operator?]{}
