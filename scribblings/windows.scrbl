#lang scribble/manual
@(require (for-label racket/base occur))

@title[#:tag "windows"]{Windows and Contexts}

Spec §7. A window assignment maps each event to half-open windows [s, e).
Each (key, window) produces one @tt{Window} event with ts = s, te = e, and
the window's contents as constituents.

@section{Windows}

@specform[(window spec option ...)
          #:grammar ([spec (tumbling #:size d maybe-offset)
                           (sliding #:size d #:slide s maybe-offset)
                           (session #:gap g)
                           (count #:size n maybe-step)
                           (landmark #:from t0)
                           (punctuated #:open expr #:close expr maybe-timeout)
                           (since-last #:when expr)
                           (calendar cron-expression maybe-clock)]
                     [option (code:line #:lateness L) #:fire-empty
                             (code:line #:assign start) (code:line #:assign end) (code:line #:assign overlap)
                             (code:line #:clock processing-time)
                             (code:line #:watermark (keyed all #:kinds (kind ...)))
                             (code:line #:type type)])]{
@itemlist[
 @item{@tt{WN.tumbling}: the grid aligned to the epoch plus the offset; the
       first window is the one containing the first event.}
 @item{@tt{WN.sliding}: each event is in at most ⌈Δ/σ⌉ windows.}
 @item{@tt{WN.session}: per key, maximal runs with gaps below g; the window
       is [first, last + g); an arrival may merge two sessions.}
 @item{@tt{WN.count}: the last n events, advancing every m.}
 @item{@tt{WN.landmark}: [t0, now), growing forever.}
 @item{@tt{WN.punctuated}: opened by one event, closed by another or a
       timeout; @tt{reason} says which.}
 @item{@tt{WN.since-last}: closes when @racket[#:when] holds for the
       accumulated fields, then reopens.}
 @item{@tt{WN.calendar}: between ticks of a schedule; missed ticks
       coalesce, with their count in @tt{windows}.}
 @item{@tt{WN.empty}: @racket[#:fire-empty] makes empty grid windows fire;
       calendar windows fire empty by default.}]

An event-time window fires when the watermark reaches its end; with
@racket[#:lateness L] a late event updates it as a correction until
W ≥ e + L. Under @racket[#:watermark (keyed all ...)] windows close per key
under the key's data-driven watermark (@tt{TM.keyed-watermark}).}

@section{Contexts}

@specform[(context spec)
          #:grammar ([spec (fixed-interval #:from s #:to e)
                           (event-interval #:open expr #:close expr maybe-timeout)
                           (sliding-interval #:size d #:slide s)
                           (sliding-event-interval #:open expr #:duration d)
                           (segment attr ...+)
                           (near #:location loc #:radius r)
                           (near-entity #:locate f #:entities thunk #:radius r)
                           (near-event #:anchor expr #:radius r #:duration d)
                           (and spec ...+)
                           (state #:from stream #:active expr)
                           (entity directory)])]{
@tt{CX.temporal}, @tt{CX.segmentation}, @tt{CX.spatial},
@tt{CX.composite}, @tt{CX.state}, and @tt{CX.entity}. Assigning contexts
tag each event with a @tt{context} attribute per instance it belongs to;
key by it to evaluate per instance. The state and entity contexts gate the
stream.}

@defform[(directory name #:from data)
         #:grammar ([data (jsonl path) (entries [id status] ...) (data expr)])]{
An entity directory (@tt{CX.entity}). Entities listed as active are
activated at once with their activation instant (@tt{CX.activation});
deprecated entities have their state dropped and their events ignored;
removed entities are activated afresh if they send data. A source's
@racket[#:directory] option gates its arrivals by the directory; scripts
push changes with @racket[(directory-push name entries #:mode 'patch)].}

@specform[(activations)]{A stream of @tt{Activated} events, one per
activation signal.}

@section[#:tag "windows-runtime"]{Runtime}

@defproc[(grid-window-operator [g grid?] [acc accumulator?] [#:key key any/c #f]
                               [#:assign assign (or/c 'start 'end 'overlap) 'start]
                               [#:lateness L duration? 0] [#:fire-empty? fire-empty? boolean? #f]
                               [#:closer closer any/c 'watermark] [#:type type symbol? 'Window])
         operator?]{Tumbling and sliding windows; see @racket[tumbling] and
@racket[sliding].}
@defproc[(make-accumulator [#:init init (-> any/c)] [#:add add (-> any/c event? any/c)]
                           [#:result result (-> any/c hash?)]
                           [#:remove remove (or/c #f (-> any/c event? any/c)) #f])
         accumulator?]{The accumulator protocol shared with aggregates.}
@defproc[(context-operator [a assigner?] [#:key key any/c #f]) operator?]{}
@defproc[(entity-context-operator [directory any/c] [#:key key (-> event? any/c)]) operator?]{}
