# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
