# 0004. How a #lang occur module is structured

- Status: accepted
- Date: 2026-10-26
- Spec: all surface forms

## Context

The language must run a module's program when the module is executed,
expose the program to tests and other modules, let libraries (such as
`lib/reflex.rkt`) define new forms in occur itself, and check schemas at
compile time with source locations.

## Decision

- `#lang occur` reads s-expressions with `syntax/module-reader` over the
  module language `occur/lang/language`: `racket/base`, the public runtime
  API (`occur`), and the declaration forms. The runtime accessor
  `event-type` is available as `event-type-of`, because `event-type` is the
  declaration form.
- Every module has its own registry, bound with a splicing syntax
  parameter. Declarations produced by a library macro therefore register in
  the module that uses the macro, not in the library.
- Each module provides `occur-program`, `occur-steps`, and `occur-run`, and
  has a `main` submodule that runs the program and prints its outputs.
  `racket file.rkt` runs a program; `raco test` only instantiates it.
- Declaration forms bind their names to compile-time descriptions (types,
  sources, streams, templates). As expressions, the names refer to runtime
  values; a type name in operator position builds a checked event.
- Sub-form keywords inside `define-stream` and `define-pattern` (`from`,
  `filter`, `seq`, `kleene+`, and so on) are matched by name, not by
  binding, so they never clash with ordinary Racket bindings such as
  `filter`. Extensible names, such as rule conditions defined by libraries,
  are bindings.
- Dotted references such as `a.card` are read as one symbol and split by
  the expander when the prefix is a pattern binding.

## Consequences

- A module that only defines library forms has an empty program; running
  it prints nothing.
- Users who need `racket/base`'s `filter` inside expressions still have it.
