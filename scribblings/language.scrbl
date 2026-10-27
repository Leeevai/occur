#lang scribble/manual
@(require (for-label racket/base occur))

@title[#:tag "language"]{The Surface Language}

A @hash-lang[] @racketmodname[occur] module declares event types, sources,
and streams. Running the module (@exec{racket file.rkt}) runs its program
against the declared sources and prints every output. The module also
provides @racket[occur-program], @racket[occur-steps], and
@racket[occur-run] for tests and other modules.

A module that declares nothing to run (no source, stream, pattern,
schedule, view, action, rule source, directory, script, or run options) is
a @emph{library}: it exports only what it provides, such as event types,
condition names, and macros that expand into declarations in the module
that uses them. Several libraries can be required together;
@filepath{lib/reflex.rkt} is one (see @secref["reflex"]).

@codeblock|{
#lang occur
(event-type Swipe [card : String] [amount : Real])
(event-type Verified [card : String])

(source feed #:types (Swipe Verified) #:from (jsonl "data/cards.jsonl"))

(define-stream swipes (from feed Swipe))
}|

@section{Declarations}

@defform[(event-type name [attr : domain] ... maybe-derived)
         #:grammar ([domain Integer Real String Boolean Time Duration Location Any
                            (Listof domain) (Record [attr : domain] ...)]
                    [maybe-derived (code:line) #:derived])]{
Declares an event type (@tt{ES.type}). Duplicate attributes and unknown
domains are compile-time errors. Used in operator position, the type builds
a checked event: @racket[(Swipe #:at 60 [card "c1"] [amount 2])]. Unknown
and missing attributes are compile-time errors.}

@defform[(source name #:types (type ...) maybe-from maybe-watermark)
         #:grammar ([maybe-from (code:line) (code:line #:from (jsonl path))
                                (code:line #:from (events item ...))
                                (code:line #:from (data expr))]
                    [maybe-watermark (code:line) (code:line #:watermark none)
                                     (code:line #:watermark ascending)
                                     (code:line #:watermark (bounded-delay d))])]{
Declares a source stream (@tt{ES.stream}) with its types and data. A
@racket[jsonl] path is relative to the module. Inline items are events or
@racket[(arrival at event)] records carrying a processing time.}

@defform[(define-stream name stage ...+)]{Declares a stream as a pipeline of
stages. The first stage is @racket[(from upstream type ...)], which reads a
source or another stream, optionally only some of its types.}

@defform[(script option ... step ...)
         #:grammar ([option (code:line #:start t) (code:line #:final-watermark? bool)]
                    [step (at t source event-expr) (advance-to t) (watermark source t)
                          (suspend t1 t2) expr])]{Replaces the sources' data
with an explicit script for the simulated processing clock. With
@racket[#:start], the clock starts at @racket[t] and the declarations'
setup (directories, rule sources) takes effect then; with
@racket[#:final-watermark? #f] the run ends without moving every watermark
to +∞. An @racket[expr] step is any runtime step, such as
@racket[(checkpoint)] or @racket[(rule-source-push 'name items)].}

@defform[(run-options option ...)]{The options of @racket[script], for a
module that runs on its sources' own data.}
