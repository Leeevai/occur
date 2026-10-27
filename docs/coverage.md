# Spec coverage

One row per spec ID in `docs/spec.md`. A row is complete when it names the
surface form in `#lang occur`, the runtime function that implements it, the
tests tagged with the ID (`spec-test`), and an example that uses it.

`tests/coverage.rkt` parses the ID column of this table and fails if any ID
has no tagged test.

Status values: `todo`, `partial`, `done`.

| ID | § | Surface form | Runtime function | Tests | Example | Status |
|---|---|---|---|---|---|---|
| `ES.time` | §2 | (M2) durations: `seconds` `minutes` `hours` | `time?` `interval` `utc` (private/time.rkt) | tests/time-test.rkt |  | partial |
| `ES.type` | §2 | (M2) `event-type` | `make-event-type` `payload-problem` (private/event.rkt) | tests/event-test.rkt |  | partial |
| `ES.event` | §2 | (M2) `source` data | `make-event` `event-attr` (private/event.rkt) | tests/event-test.rkt |  | partial |
| `ES.batch` | §2 | (M2) `#:n` in source data | `make-batch-event` `batch-share` (private/event.rkt) | tests/batch-test.rkt |  | partial |
| `ES.reference` | §2 | (M2) | `make-reference-event` `store-read` (private/event.rkt) | tests/reference-test.rkt |  | partial |
| `ES.composite` | §2 | (M10) `#:emit` | `make-composite` (private/event.rkt) | tests/composite-test.rkt |  | partial |
| `ES.space` | §2 | (M2) compile-time schema checks | `event-space-member?` `event-level` (private/event.rkt) | tests/space-test.rkt |  | partial |
| `ES.precedes` | §2 | (M9) `(precedes a b)` in `#:where` | `precedes?` (private/relations.rkt) | tests/relations-test.rkt |  | partial |
| `ES.membership` | §2 | (M13) | `constituent-of?` `provenance` `member*?` (private/relations.rkt) | tests/relations-test.rkt |  | partial |
| `ES.causality` | §2 | (M13) | `causality-from-events` `causes?` (private/relations.rkt) | tests/relations-test.rkt |  | partial |
| `ES.stream` | §2 | (M2) `source` | `make-event-stream` (private/stream.rkt) | tests/stream-test.rkt |  | partial |
| `ES.history` | §2 | (M2) | `history` (private/stream.rkt) | tests/stream-test.rkt |  | partial |
| `ES.keyed` | §2 | (M4) `(key-by attr)` | `partition-stream` (private/stream.rkt) | tests/stream-test.rkt |  | partial |
| `ES.substream` | §2 | (M4) `#:key` `#:substream` `#:kind` | `attribute-keying` `watching` (private/stream.rkt) | tests/stream-test.rkt |  | partial |
| `ES.operator` | §2 | | | | | todo |
| `OP.filter` | §4 | | | | | todo |
| `OP.project` | §4 | | | | | todo |
| `OP.map` | §4 | | | | | todo |
| `OP.enrich` | §4 | | | | | todo |
| `OP.split` | §4 | | | | | todo |
| `OP.route` | §4 | | | | | todo |
| `ST.monotonic-merge` | §5 | | | | | todo |
| `ST.merge` | §5 | | | | | todo |
| `ST.key-by` | §5 | | | | | todo |
| `ST.dedupe` | §5 | | | | | todo |
| `ST.reorder` | §5 | | | | | todo |
| `ST.throttle` | §5 | | | | | todo |
| `TM.clock` | §6 | | | | | todo |
| `TM.lateness` | §6 | | | | | todo |
| `TM.keyed-watermark` | §6 | | | | | todo |
| `TM.event-time` | §6 | | | | | todo |
| `TM.processing-time` | §6 | | | | | todo |
| `TM.watermark` | §6 | | | | | todo |
| `TM.idle` | §6 | | | | | todo |
| `TM.skew-guard` | §6 | | | | | todo |
| `TM.timer` | §6 | | | | | todo |
| `TM.schedule` | §6 | | | | | todo |
| `TM.retraction` | §6 | | | | | todo |
| `TM.checkpoint` | §6 | | | | | todo |
| `TM.replay` | §6 | | | | | todo |
| `WN.tumbling` | §7 | | | | | todo |
| `WN.sliding` | §7 | | | | | todo |
| `WN.session` | §7 | | | | | todo |
| `WN.count` | §7 | | | | | todo |
| `WN.landmark` | §7 | | | | | todo |
| `WN.punctuated` | §7 | | | | | todo |
| `WN.since-last` | §7 | | | | | todo |
| `WN.calendar` | §7 | | | | | todo |
| `WN.empty` | §7 | | | | | todo |
| `CX.temporal` | §7 | | | | | todo |
| `CX.segmentation` | §7 | | | | | todo |
| `CX.spatial` | §7 | | | | | todo |
| `CX.state` | §7 | | | | | todo |
| `CX.entity` | §7 | | | | | todo |
| `CX.activation` | §7 | | | | | todo |
| `CX.composite` | §7 | | | | | todo |
| `AG.count` | §8 | | | | | todo |
| `AG.multiplicity` | §8 | | | | | todo |
| `AG.events` | §8 | | | | | todo |
| `AG.sum` | §8 | | | | | todo |
| `AG.min` | §8 | | | | | todo |
| `AG.max` | §8 | | | | | todo |
| `AG.avg` | §8 | | | | | todo |
| `AG.variance` | §8 | | | | | todo |
| `AG.first` | §8 | | | | | todo |
| `AG.last` | §8 | | | | | todo |
| `AG.span` | §8 | | | | | todo |
| `AG.sources` | §8 | | | | | todo |
| `AG.collect` | §8 | | | | | todo |
| `AG.argmax` | §8 | | | | | todo |
| `AG.argmin` | §8 | | | | | todo |
| `AG.median` | §8 | | | | | todo |
| `AG.percentile` | §8 | | | | | todo |
| `AG.count-distinct` | §8 | | | | | todo |
| `AG.mode` | §8 | | | | | todo |
| `AG.top-k` | §8 | | | | | todo |
| `AG.approx` | §8 | | | | | todo |
| `AG.incremental` | §8 | | | | | todo |
| `JN.window` | §9 | | | | | todo |
| `JN.interval` | §9 | | | | | todo |
| `JN.table` | §9 | | | | | todo |
| `JN.as-of` | §9 | | | | | todo |
| `JN.outer` | §9 | | | | | todo |
| `AL.before` | §10 | | | | | todo |
| `AL.after` | §10 | | | | | todo |
| `AL.meets` | §10 | | | | | todo |
| `AL.met-by` | §10 | | | | | todo |
| `AL.overlaps` | §10 | | | | | todo |
| `AL.overlapped-by` | §10 | | | | | todo |
| `AL.starts` | §10 | | | | | todo |
| `AL.started-by` | §10 | | | | | todo |
| `AL.during` | §10 | | | | | todo |
| `AL.contains` | §10 | | | | | todo |
| `AL.finishes` | §10 | | | | | todo |
| `AL.finished-by` | §10 | | | | | todo |
| `AL.equals` | §10 | | | | | todo |
| `PT.sequence` | §11 | | | | | todo |
| `PT.primitive` | §11 | | | | | todo |
| `PT.activation` | §11 | | | | | todo |
| `PT.conjunction` | §11 | | | | | todo |
| `PT.disjunction` | §11 | | | | | todo |
| `PT.negation` | §11 | | | | | todo |
| `PT.absence` | §11 | | | | | todo |
| `PT.span-absence` | §11 | | | | | todo |
| `PT.kleene-plus` | §11 | | | | | todo |
| `PT.kleene-star` | §11 | | | | | todo |
| `PT.repeat` | §11 | | | | | todo |
| `PT.any` | §11 | | | | | todo |
| `PT.within` | §11 | | | | | todo |
| `PT.where` | §11 | | | | | todo |
| `PT.aperiodic` | §11 | | | | | todo |
| `PT.aperiodic-cumulative` | §11 | | | | | todo |
| `PT.periodic` | §11 | | | | | todo |
| `PT.periodic-cumulative` | §11 | | | | | todo |
| `PT.threshold` | §11 | | | | | todo |
| `PT.trend.increasing` | §11 | | | | | todo |
| `PT.trend.decreasing` | §11 | | | | | todo |
| `PT.trend.non-increasing` | §11 | | | | | todo |
| `PT.trend.non-decreasing` | §11 | | | | | todo |
| `PT.trend.stable` | §11 | | | | | todo |
| `PT.trend.mixed` | §11 | | | | | todo |
| `PT.relative` | §11 | | | | | todo |
| `PT.spatial.distance` | §11 | | | | | todo |
| `PT.spatial.moving` | §11 | | | | | todo |
| `PT.accumulate` | §11 | | | | | todo |
| `PT.flush` | §11 | | | | | todo |
| `PT.silence` | §11 | | | | | todo |
| `PT.recovery` | §11 | | | | | todo |
| `PT.window-close` | §11 | | | | | todo |
| `PT.schedule` | §11 | | | | | todo |
| `SEL.next` | §12 | | | | | todo |
| `SEL.strict` | §12 | | | | | todo |
| `SEL.partition` | §12 | | | | | todo |
| `SEL.any` | §12 | | | | | todo |
| `CON.cumulative` | §12 | | | | | todo |
| `CON.retain` | §12 | | | | | todo |
| `CON.recent` | §12 | | | | | todo |
| `CON.unrestricted` | §12 | | | | | todo |
| `CON.chronicle` | §12 | | | | | todo |
| `CON.continuous` | §12 | | | | | todo |
| `OUT.rate-limit` | §12 | | | | | todo |
| `OUT.coalesce` | §12 | | | | | todo |
| `OUT.first` | §12 | | | | | todo |
| `OUT.timeout-partial` | §12 | | | | | todo |
| `OUT.edge` | §12 | | | | | todo |
| `OUT.every` | §12 | | | | | todo |
| `OUT.last` | §12 | | | | | todo |
| `OUT.maximal` | §12 | | | | | todo |
| `SKIP.no-skip` | §12 | | | | | todo |
| `SKIP.to-next` | §12 | | | | | todo |
| `SKIP.past-last` | §12 | | | | | todo |
| `SKIP.to-first` | §12 | | | | | todo |
| `SKIP.to-last` | §12 | | | | | todo |
| `EVAL.immediate` | §12 | | | | | todo |
| `EVAL.deferred` | §12 | | | | | todo |
| `DR.summary` | §13 | | | | | todo |
| `DR.view` | §13 | | | | | todo |
| `DR.output-identity` | §13 | | | | | todo |
| `DR.derive` | §13 | | | | | todo |
| `DR.ordered` | §13 | | | | | todo |
| `DR.closure` | §13 | | | | | todo |
| `DR.cycle-guard` | §13 | | | | | todo |
| `DR.act` | §13 | | | | | todo |
| `DR.retract` | §13 | | | | | todo |
| `RL.rejudge` | §14 | | | | | todo |
| `RL.template` | §14 | | | | | todo |
| `RL.binding` | §14 | | | | | todo |
| `RL.fan-out` | §14 | | | | | todo |
| `RL.identity` | §14 | | | | | todo |
| `RL.source` | §14 | | | | | todo |
| `RL.ownership` | §14 | | | | | todo |
| `RL.dangling` | §14 | | | | | todo |
| `RL.semantics` | §14 | | | | | todo |
| `RL.carry` | §14 | | | | | todo |
| `RL.converge` | §14 | | | | | todo |
