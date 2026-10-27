#lang scribble/manual
@(require (for-label racket/base occur))

@title[#:tag "derivation"]{Derivation, Closure, and Action}

Spec §13.

@itemlist[
 @item{@tt{DR.derive}: a pattern's @racket[#:emit] builds ⟨fresh id, τ′,
       f(bindings), ts(m), te(m), 1, op, m⟩; see @racket[derive-match].}
 @item{@tt{DR.summary}: @racket[(summarize)] replaces constituents with a
       summary (interval, multiplicity sum, merged count, sources);
       @racket[summarized-output] builds such outputs directly.}
 @item{@tt{DR.output-identity}: @racket[output-identity] and
       @racket[event-identity] derive identities from content;
       @racket[(dedupe-outputs)] drops repeats downstream.}
 @item{@tt{DR.ordered}: outputs are delivered grouped by key within each
       instant; @racket[delivery-groups].}
 @item{@tt{DR.closure}: derived streams feed any stage or pattern.}
 @item{@tt{DR.cycle-guard}: @racket[(feedback name)] creates loops, which
       need @racket[#:recursive] bounds.}
 @item{@tt{DR.act}: @racket[act] runs a side effect per event.}
 @item{@tt{DR.view}: @racket[define-view]; @racket[view-ref];
       @racket[classify-health].}
 @item{@tt{DR.retract}: retracting a constituent retracts what was derived
       from it.}]

@defform[(define-view name #:from stream maybe-key #:init expr #:update expr
                      maybe-project maybe-export maybe-lifecycle)
         #:grammar ([maybe-key (code:line) (code:line #:key attr)]
                    [maybe-project (code:line) (code:line #:project expr)]
                    [maybe-export (code:line) (code:line #:export-every duration)]
                    [maybe-lifecycle (code:line) (code:line #:lifecycle? bool)])]{
@racket[#:update] sees @racket[state], @racket[now], @racket[it], and the
event's attributes. With @racket[#:project], the view exports the
projection, which sees @racket[state], @racket[now], @racket[last-arrival],
and @racket[newest], as a health classification needs:
@racket[(classify-health #:now now #:last-arrival last-arrival #:newest newest #:live 30 #:silent 120 #:stale 60)].
A view follows its entities' lifecycle unless @racket[#:lifecycle? #f], as
for a view keyed by something other than the entity.}

@defform[(act name #:on stream #:do expr maybe-completion)
         #:grammar ([maybe-completion (code:line) (code:line #:completion type)])]{}

@defproc[(view-ref [result run-result?] [view symbol?] [key any/c]) any/c]{}
@defproc[(output-identity [#:instance instance any/c] [#:key key any/c] [#:reason reason any/c]
                          [#:interval iv interval?] [#:samples n exact-nonnegative-integer?]
                          [#:emitted-at at (or/c #f time?) #f] [#:no-new-data? none? boolean? #f])
         string?]{}
