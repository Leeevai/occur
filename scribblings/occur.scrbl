#lang scribble/manual
@(require (for-label racket/base occur))

@title{occur: Event Processing in Racket}
@author{Leeevai}

@defmodule[occur]

@emph{occur} is a domain-specific language for event processing, implemented
in Racket as @hash-lang[] @racketmodname[occur]. It realizes the
specification in @filepath{docs/spec.md}: the event space, single-event and
stream-structure operations, time and progress, windows and contexts,
aggregation, joins, Allen's interval relations, a complete pattern algebra
with its selection, consumption and output policies, derivation, and rules as
data.

Every concept of the specification has a stable ID, such as
@tt{PT.sequence}. This manual cites those IDs; @filepath{docs/coverage.md}
maps each one to its surface form, runtime function, tests, and example.

The library has two layers. The @emph{runtime} is plain Racket with
contracts on every export, usable through @racket[(require occur)]. The
@emph{surface language}, @hash-lang[] @racketmodname[occur], expands into
calls to the runtime and checks schemas, attributes, and policies at compile
time.

@table-of-contents[]

@include-section["event-space.scrbl"]
@include-section["language.scrbl"]
@include-section["runner.scrbl"]
@include-section["operations.scrbl"]
@include-section["structure.scrbl"]
@include-section["progress.scrbl"]
@include-section["windows.scrbl"]
@include-section["aggregation.scrbl"]
@include-section["joins.scrbl"]
@include-section["allen.scrbl"]
@include-section["patterns.scrbl"]
@include-section["derivation.scrbl"]
