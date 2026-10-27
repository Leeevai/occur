# 0017. Reflex as a library, and libraries in #lang occur

- Status: accepted
- Date: 2026-10-27
- Spec: §16; §2 ES.reference, ES.substream; §13 DR.view; §14

## Context

§16 claims that Reflex is one program in the specification's vocabulary,
and the task asks for it as a small library written in occur's public forms,
with no special case in the runtime. Writing it showed what a library needs
from the language, and where the surface was missing pieces that §16 uses.

## Decision

- **Libraries.** A `#lang occur` module that declares nothing to run (no
  source, stream, pattern, schedule, view, action, rule source, directory,
  script, or run options) is a library: it exports only what it provides.
  Every declaration refers to the module's registry through a syntax
  parameter, whose expansion marks the module as a program; only a program
  provides `occur-program`, `occur-steps`, and `occur-run`. Event types no
  longer register in the registry, which never read them.
- **The library's vocabulary.** `lib/reflex.rkt` provides Reflex's types
  (`DataArrival`, `SensorData`, `Trigger`), its device directory
  (`define-devices`, `push-devices`, with watched, deprecated, and deleted
  mapped onto active, inactive, and removed), its four conditions as
  renamed core conditions, `reflex-rule` (a template with Reflex's
  modifiers), `define-reflex` (the program of §16.1 and §16.3), and
  `define-fleet-health`.
- **Order of normalization.** Split, key by owner, skew guard, directory,
  merge, as §16.1 lists them: the guard and the directory drop data before
  it can move a high-water mark.
- **General features, not Reflex cases.** The library needed, and the
  surface gained: `split … #:reference #:summary` (reference events),
  `key-by … #:substream #:kind`, `define-condition #:as` renames that convert
  a value, `define-view #:project` and `#:lifecycle?`, `script #:start
  #:final-watermark?` and `run-options`, rule versions (`rule-template #:id
  #:in-source`, `rule-source #:at-start? #f`), and a merge keyed by entity.
  Writing the examples found two faults that are fixed for every program:
  the flush clock now restarts after a consuming output, and a replay
  holds a rules node's outputs at the node (decision 0015).
- **Values §16 does not give.** The health thresholds are parameters of
  `define-fleet-health`, with no defaults. `#:reset-on-max-delay` defaults
  to true (the flush consumes), and `#:require-all` to the core window
  condition's default, mode all. The skew bound is the hour of §16.1.
- **Trace R6's variant** runs both of its cases in one program on two
  devices, one per carry policy; devices do not share state, so each
  behaves as the trace's single device does.

## Consequences

- `lib/reflex.rkt` is about two hundred lines of `#lang occur`, and
  `tests/reflex-purity.rkt` keeps it free of private modules.
- Several `#lang occur` libraries can be required together.
- `docs/reflex.md` maps every row of §16.6 to the form that realizes it.
