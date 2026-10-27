#lang scribble/manual
@(require (for-label racket/base occur))

@title[#:tag "language"]{The Surface Language}

A @hash-lang[] @racketmodname[occur] module declares event types, sources,
and streams. Running the module (@exec{racket file.rkt}) runs its program
against the declared sources and prints every output. The module also
provides @racket[occur-program], @racket[occur-steps], and
@racket[occur-run] for tests and other modules.

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

@defform[(script step ...)
         #:grammar ([step (at t source event) (advance-to t) (watermark source t)
                          (suspend t1 t2) expr])]{Replaces the sources' data
with an explicit script for the simulated processing clock.}
