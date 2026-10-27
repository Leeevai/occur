# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.0] - 2026-10-27

The first release: every concept of the specification, from the event
space to rules as data, in the runtime and in `#lang occur`, tested and
documented; the card-testing acceptance of §15; and Reflex (§16) as a
library written in occur, reproducing traces R1–R6 exactly.

### Added

- Repository skeleton: license, package metadata, the verbatim specification,
  project conventions, the progress checklist, and the coverage table.
- Event space (M1): the time domain in seconds with closed intervals and UTC
  calendar arithmetic; event types and schemas; event instances with
  deterministic ids; batch events and `batch-share`; reference payloads and a
  sample store; composite events; the inductive event space with abstraction
  levels; precedence, membership, and causality; streams, histories, keyed
  partitions, and sub-streams.
- Language skeleton and runner (M2): the deadline min-heap; operators as
  callbacks over immutable state; programs with a static cycle guard; the
  deterministic scripted runner with a simulated processing clock,
  event-time timers, watermarks, suspension, retraction propagation, and
  ordered delivery; JSON Lines input; module registries; `#lang occur` with
  `event-type`, `source`, `define-stream` (`from`), and `script`, and
  compile-time schema checks with source locations; the first manual
  sections.
- Single-event operations (M3): `filter`, `project`, `map`, `enrich`,
  `split`, and `route` as runtime operators and `define-stream` stages, with
  attributes bound as variables in stage expressions and compile-time checks
  against the stream's shape.
- Stream structure (M4): `merge`, `key-by`, `dedupe`, high-water-mark
  `monotonic-merge` (trace R2), `reorder`, and `throttle`, with source keying
  options (`#:key`, `#:substream`, `#:kind`, `#:merge high-water-mark`) and a
  property test that partitioned runs emit what one run emits.
- Time and progress (M5): keyed data-driven watermarks (W^any, W^all), the
  future-skew guard, lateness policies with side outputs and allowed
  lateness, event- and processing-time timers, idleness detection and
  heartbeat watermarks, five-field UTC cron schedules with coalesced missed
  ticks (`define-schedule`), generator nodes, and retractions in source
  data.
- Windows and contexts (M6): tumbling, sliding, session, count, landmark,
  punctuated, since-last, and calendar windows; empty windows; allowed
  lateness with corrections; windows closing on the processing clock or
  under keyed watermarks; temporal, segmentation, spatial, state, composite,
  and entity-lifecycle contexts with activation signals; directories.
- Aggregation (M7): distributive, algebraic, holistic, and approximate
  aggregates (HyperLogLog, t-digest, Count-Min on a deterministic hash),
  incremental maintenance with remove, two-stack queues, and monotonic
  deques, and the `(aggregate ...)` stage fused with windows.
- Joins (M8): windowed, interval, stream-table, and as-of joins with left,
  right, and full outer variants, and the `(join ...)` stage with `left.`
  and `right.` attribute references.
- Allen relations (M9): the 13 interval relations, point/point and
  point/interval relations, `allen-relation` and a predicate per relation,
  over intervals and events.
- Pattern algebra (M10): the pattern AST of §11.2 with ACT; the reference
  oracle transcribing §11.3 operator by operator; the derived families of
  §11.4 (threshold, trends, relative extremes, spatial distance and
  movement, accumulate, silence, recovery, window close, schedule);
  `define-pattern` with compile-time checks for bindings, Kleene misuse,
  and policy names, and a warning for unbounded patterns; the enforced
  coverage test.
- Incremental engine (M11): a tree of incremental match automata with
  delta joins, deferred decisions at the watermark, hybrid-clock absence,
  strict event-time deadlines, per-node arrival sequence numbers, and
  differential property tests against the oracle; the default engine of
  `define-pattern`.
- Pattern policies (M12): strict, partition, and skip-till-next-match
  selection as per-step conditions checked by the engine and the oracle,
  with the nesting property; recent, chronicle, continuous, and cumulative
  consumption and retain; first, last, and maximal output; timed-out partial
  matches (PT.flush); the five skip strategies; immediate and deferred
  evaluation; edge, rate-limit, and coalesce stages; trace A's table of
  §15.
- Derivation (M13): summarized outputs, content-derived output identity
  and downstream deduplication, ordered delivery per instant, closure of
  derived streams, bounded feedback loops behind the cycle guard, actions,
  and materialized views with scheduled export and health classification.
- Rules as data (M14): templates, bindings with overrides and params,
  fan-out, instance identity, named sources replaced whole or patched,
  ownership, dangling and invalid items reported, fingerprints and edit
  classes, progress carry, re-judge on edit, and convergence; the rules
  engine with accumulate (and flush), window close, silence (and recovery),
  and schedule conditions under deferring rate limits; `define-condition`,
  `rule-template`, `rule-source`, `bind`, and the `(rules)` stage; JSON
  Lines rule items; traces R1 and R3 to R6 of §16.7 on the runtime.
- Checkpoint and replay (M15): `(checkpoint)` and `(restore)` script steps
  that snapshot all runner state and re-read the input retained since, and
  `(cold-start)`, which rebuilds state from all retained input with
  processing-time timers suspended, holds outputs back until the first
  rules push, coalesces them per operator instance, and judges them against
  the instances then in force; a rule source's name is bound to its items.
- Card-testing acceptance (M16): `examples/card-testing.rkt` with traces A,
  A′, and B of §15 as acceptance tests, on the incremental engine and the
  reference oracle; the reference engine applies the selection strategies
  with every or maximal output; an examples chapter in the manual.

- Reflex (M17): `lib/reflex.rkt`, Reflex as a library of public occur forms
  (its types, device directory, four conditions, `reflex-rule`,
  `define-reflex`, and fleet health), with traces R1 to R6 of §16.7, a
  fleet, health, and recovery as programs in `examples/reflex/`, and
  `docs/reflex.md` mapping §16.6 row by row. The surface gained what the
  library needed: reference events from `split`, sub-streams and kinds on
  `key-by`, condition renames that convert values, view projections and
  lifecycle, script start times and `run-options`, rule versions pushed by
  scripts, and a merge keyed by entity. A module that declares nothing to
  run is a library and exports only what it provides.

- Examples and documentation (M18): `examples/fraud.rkt`,
  `examples/iot-telemetry.rkt`, `examples/logistics.rkt`, and
  `examples/history.rkt`, each with a test, so that every spec ID has an
  example; `run-result-history` for a stream's history at a time; grid
  windows that forget a retracted event before they fire; a Runtime
  Reference chapter and a manual that builds without warnings; the README
  with a quick start, the algebra in LaTeX, and many examples; every one of
  the 175 coverage rows done, and the strict coverage check on.

### Fixed

- A side stream declared by a stage that is not its stream's last is kept.
- An operator still holding a retracted event, such as a delay or an open
  window, never emits it or anything derived from it.
- Retracting an event also retracts what a node already derived from it
  when the node's operator handles retractions itself, such as a
  stream-table join.
- An outer join's unmatched rows go through its `#:select`.
- A trailing absence with its own bound no longer draws the unbounded
  pattern warning.
- A rule's max_delay clock restarts after a consuming output, instead of
  flushing a later contribution at once.
- A cold start holds a rules node's outputs at the node, so stages after it
  see them coalesced per instance and judged.
- JSON rule items keep their watched kinds as strings, the kinds of JSON
  data.
- Last and maximal output of a pattern without a top-level bound, and an
  event-less match under maximal output, are held until the end of the
  stream instead of being reported before an extension could arrive;
  `define-pattern` warns about such patterns.

[Unreleased]: https://github.com/Leeevai/occur/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/Leeevai/occur/releases/tag/v0.1.0
