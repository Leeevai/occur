# Spec coverage

One row per spec ID in `docs/spec.md`. A row is complete when it names the
surface form in `#lang occur`, the runtime function that implements it, the
tests tagged with the ID (`spec-test`), and an example that uses it.

`tests/coverage.rkt` parses the ID column of this table and fails if any ID
has no tagged test.

Status values: `todo`, `partial`, `done`.

| ID | § | Surface form | Runtime function | Tests | Example | Status |
|---|---|---|---|---|---|---|
| `ES.time` | §2 | `seconds` `minutes` `hours` `days` `utc` (available in `#lang occur`) | `time?` `interval` `utc` (private/time.rkt) | tests/time-test.rkt |  | partial |
| `ES.type` | §2 | `(event-type T [a : D] ...)` | `make-event-type` `payload-problem` (private/event.rkt) | tests/event-test.rkt, tests/lang-test.rkt |  | partial |
| `ES.event` | §2 | `(T #:at t [a v] ...)` constructor | `make-event` `event-attr` (private/event.rkt) | tests/event-test.rkt, tests/lang-test.rkt |  | partial |
| `ES.batch` | §2 | (M2) `#:n` in source data | `make-batch-event` `batch-share` (private/event.rkt) | tests/batch-test.rkt |  | partial |
| `ES.reference` | §2 | (M2) | `make-reference-event` `store-read` (private/event.rkt) | tests/reference-test.rkt |  | partial |
| `ES.composite` | §2 | (M10) `#:emit` | `make-composite` (private/event.rkt) | tests/composite-test.rkt |  | partial |
| `ES.space` | §2 | compile-time schema checks in `event-type`, `source`, constructors | `event-space-member?` `event-level` (private/event.rkt) | tests/space-test.rkt, tests/lang-test.rkt |  | partial |
| `ES.precedes` | §2 | (M9) `(precedes a b)` in `#:where` | `precedes?` (private/relations.rkt) | tests/relations-test.rkt |  | partial |
| `ES.membership` | §2 | (M13) | `constituent-of?` `provenance` `member*?` (private/relations.rkt) | tests/relations-test.rkt |  | partial |
| `ES.causality` | §2 | (M13) | `causality-from-events` `causes?` (private/relations.rkt) | tests/relations-test.rkt |  | partial |
| `ES.stream` | §2 | `(source s #:types (T ...) #:from ...)` | `make-event-stream` (private/stream.rkt) | tests/stream-test.rkt, tests/io-test.rkt, tests/lang-test.rkt |  | partial |
| `ES.history` | §2 | (M2) | `history` (private/stream.rkt) | tests/stream-test.rkt |  | partial |
| `ES.keyed` | §2 | `(key-by attr)`; source `#:key` | `partition-stream` (private/stream.rkt) | tests/stream-test.rkt, tests/keyed-prop-test.rkt |  | partial |
| `ES.substream` | §2 | source `#:key` `#:substream` `#:kind` | `attribute-keying` `watching` (private/stream.rkt) | tests/stream-test.rkt, tests/lang-test.rkt |  | partial |
| `ES.operator` | §2 | `define-stream` stages compile to operators | `make-operator` `run-operator` (private/operator.rkt, private/dataflow.rkt) | tests/operator-test.rkt, tests/dataflow-test.rkt |  | partial |
| `OP.filter` | §4 | `(filter expr)` stage | `filter-operator` (private/ops/stateless.rkt) | tests/stateless-test.rkt, tests/stateless-prop-test.rkt, tests/lang-test.rkt |  | partial |
| `OP.project` | §4 | `(project a ... #:as T)` stage | `project-operator` (private/ops/stateless.rkt) | tests/stateless-test.rkt, tests/stateless-prop-test.rkt, tests/lang-test.rkt |  | partial |
| `OP.map` | §4 | `(map T [a e] ...)` / `(map [a e] ...)` stage | `map-operator` (private/ops/stateless.rkt) | tests/stateless-test.rkt, tests/stateless-prop-test.rkt, tests/lang-test.rkt |  | partial |
| `OP.enrich` | §4 | `(enrich #:key k #:with R #:adds (...))` stage | `enrich-operator` `table-lookup` (private/ops/stateless.rkt) | tests/stateless-test.rkt, tests/lang-test.rkt |  | partial |
| `OP.split` | §4 | `(split T #:for (...) [a e] ... #:ts e)` stage | `split-operator` (private/ops/stateless.rkt) | tests/stateless-test.rkt, tests/stateless-prop-test.rkt, tests/lang-test.rkt |  | partial |
| `OP.route` | §4 | `(route #:mode m [b p] ... #:otherwise b)` final stage | `route-operator` (private/ops/stateless.rkt) | tests/stateless-test.rkt, tests/stateless-prop-test.rkt, tests/lang-test.rkt |  | partial |
| `ST.monotonic-merge` | §5 | `(monotonic-merge)` stage; source `#:merge high-water-mark` | `monotonic-merge-operator` `hwm-state` (private/ops/structure.rkt) | tests/structure-test.rkt (R2, properties), tests/lang-test.rkt |  | partial |
| `ST.merge` | §5 | `(merge s ...)` stage | `merge-operator` (private/ops/structure.rkt) | tests/structure-test.rkt, tests/lang-test.rkt |  | partial |
| `ST.key-by` | §5 | `(key-by attr)` / `(key-by #:fn e)` stage; source `#:key` | `key-by-operator` (private/ops/structure.rkt) | tests/structure-test.rkt, tests/keyed-prop-test.rkt, tests/lang-test.rkt |  | partial |
| `ST.dedupe` | §5 | `(dedupe #:key e #:horizon d #:clock c)` stage | `dedupe-operator` (private/ops/structure.rkt) | tests/structure-test.rkt, tests/lang-test.rkt |  | partial |
| `ST.reorder` | §5 | `(reorder #:late drop)` stage | `reorder-operator` (private/ops/structure.rkt) | tests/structure-test.rkt, tests/lang-test.rkt |  | partial |
| `ST.throttle` | §5 | `(throttle #:rate r #:per d)` / `#:every n` / `#:probability p #:seed s` | `throttle-operator` (private/ops/structure.rkt) | tests/structure-test.rkt, tests/lang-test.rkt |  | partial |
| `TM.clock` | §6 | `#:clock event-time|processing-time|hybrid` (checked at compile time) | `clock?` (private/time.rkt) | tests/time-test.rkt, tests/lang-test.rkt |  | partial |
| `TM.lateness` | §6 | `(lateness #:policy drop/side/(allow L) #:side-output s)` stage | `lateness-operator` (private/progress.rkt) | tests/progress-test.rkt, tests/lang-test.rkt |  | partial |
| `TM.keyed-watermark` | §6 | `(keyed-watermark #:kinds (...) #:mode any/all)` stage | `keyed-wm-any` `keyed-wm-all` `keyed-watermark-operator` (private/progress.rkt) | tests/progress-test.rkt, tests/lang-test.rkt |  | partial |
| `TM.event-time` | §6 | event time is the default clock; `#:clock event-time` | `delay-operator`, watermarks (private/progress.rkt, private/dataflow.rkt) | tests/progress-test.rkt |  | partial |
| `TM.processing-time` | §6 | `(script ...)`; `#:clock processing-time` | `run` `simulated-clock` (private/dataflow.rkt) | tests/progress-test.rkt, tests/lang-test.rkt |  | partial |
| `TM.watermark` | §6 | source `#:watermark none / ascending / (bounded-delay d) / (heartbeat T)`; script `(watermark s t)` | `watermark-to`, source strategies (private/dataflow.rkt) | tests/dataflow-test.rkt, tests/progress-test.rkt, tests/lang-test.rkt |  | partial |
| `TM.idle` | §6 | `(idle d #:clock processing-time/hybrid)` stage; `(heartbeat T)` watermarks | `idle-operator` (private/progress.rkt) | tests/progress-test.rkt, tests/lang-test.rkt |  | partial |
| `TM.skew-guard` | §6 | `(skew-guard B)` stage; source `#:skew-guard B` | `skew-guard-operator` (private/progress.rkt) | tests/progress-test.rkt, tests/lang-test.rkt |  | partial |
| `TM.timer` | §6 | `(delay d #:clock c)` stage | `delay-operator`, operator deadlines (private/progress.rkt, private/dataflow.rkt) | tests/dataflow-test.rkt, tests/progress-test.rkt, tests/lang-test.rkt |  | partial |
| `TM.schedule` | §6 | `(define-schedule name "cron")` | `parse-cron` `cron-next` `schedule-operator` (private/scheduler.rkt, private/progress.rkt) | tests/cron-test.rkt, tests/progress-test.rkt, tests/lang-test.rkt |  | partial |
| `TM.retraction` | §6 | `(retraction-of id)` in source data; JSON Lines `{"retract": id}` | `retraction` items, provenance propagation, `emit-correction!` (private/dataflow.rkt, private/progress.rkt) | tests/dataflow-test.rkt, tests/progress-test.rkt, tests/lang-test.rkt |  | partial |
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
| `DR.cycle-guard` | §13 | (M13) | `make-program` cycle check (private/dataflow.rkt) | tests/dataflow-test.rkt |  | partial |
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
