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
| `WN.tumbling` | §7 | `(window (tumbling #:size d #:offset o) ...)` | `grid-window-operator` `tumbling` (private/window.rkt) | tests/window-test.rkt, tests/window-prop-test.rkt, tests/lang-test.rkt |  | partial |
| `WN.sliding` | §7 | `(window (sliding #:size d #:slide s) ...)` | `grid-window-operator` `sliding` (private/window.rkt) | tests/window-test.rkt, tests/window-prop-test.rkt, tests/lang-test.rkt |  | partial |
| `WN.session` | §7 | `(window (session #:gap g))` | `session-window-operator` (private/window.rkt) | tests/window-test.rkt, tests/window-prop-test.rkt, tests/lang-test.rkt |  | partial |
| `WN.count` | §7 | `(window (count #:size n #:step m))` | `count-window-operator` (private/window.rkt) | tests/window-test.rkt, tests/window-prop-test.rkt, tests/lang-test.rkt |  | partial |
| `WN.landmark` | §7 | `(window (landmark #:from t0))` | `landmark-window-operator` (private/window.rkt) | tests/window-test.rkt, tests/lang-test.rkt |  | partial |
| `WN.punctuated` | §7 | `(window (punctuated #:open e #:close e #:timeout d))` | `punctuated-window-operator` (private/window.rkt) | tests/window-test.rkt, tests/lang-test.rkt |  | partial |
| `WN.since-last` | §7 | `(window (since-last #:when e))` | `since-last-window-operator` (private/window.rkt) | tests/window-test.rkt, tests/lang-test.rkt |  | partial |
| `WN.calendar` | §7 | `(window (calendar "cron" #:clock c))` | `calendar-window-operator` (private/window.rkt) | tests/window-test.rkt, tests/lang-test.rkt |  | partial |
| `WN.empty` | §7 | `#:fire-empty` window option; calendar windows fire empty by default | `grid-window-operator #:fire-empty?` (private/window.rkt) | tests/window-test.rkt, tests/lang-test.rkt |  | partial |
| `CX.temporal` | §7 | `(context (fixed-interval ...))` `(event-interval ...)` `(sliding-interval ...)` `(sliding-event-interval ...)` | `fixed-interval-context` `event-interval-context` ... (private/context.rkt) | tests/context-test.rkt, tests/lang-test.rkt |  | partial |
| `CX.segmentation` | §7 | `(context (segment a ...))` | `segmentation-context` (private/context.rkt) | tests/context-test.rkt, tests/lang-test.rkt |  | partial |
| `CX.spatial` | §7 | `(context (near ...))` `(near-entity ...)` `(near-event ...)` | `fixed-location-context` `entity-distance-context` `event-distance-context` (private/context.rkt) | tests/context-test.rkt, tests/lang-test.rkt |  | partial |
| `CX.state` | §7 | `(context (state #:from s #:active e))` | `state-context-operator` (private/context.rkt) | tests/context-test.rkt, tests/lang-test.rkt |  | partial |
| `CX.entity` | §7 | `(directory d #:from ...)`, source `#:directory d`, `(context (entity d))` | `entity-context-operator` `directory-push` (private/context.rkt) | tests/context-test.rkt, tests/lang-test.rkt |  | partial |
| `CX.activation` | §7 | `(activations)` stage; lifecycle `activate` signals | `signal` with the activation instant, `activations-operator` (private/context.rkt) | tests/context-test.rkt, tests/lang-test.rkt |  | partial |
| `CX.composite` | §7 | `(context (and c ...))` | `composite-context` (private/context.rkt) | tests/context-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.count` | §8 | `(count)` in `(aggregate ...)` | `count-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.multiplicity` | §8 | `(count)` / `(multiplicity)` in `(aggregate ...)` | `multiplicity-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.events` | §8 | `(events)` in `(aggregate ...)` | `events-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.sum` | §8 | `(sum e)` in `(aggregate ...)` | `sum-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.min` | §8 | `(min e)` in `(aggregate ...)` | `min-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.max` | §8 | `(max e)` in `(aggregate ...)` | `max-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.avg` | §8 | `(avg e)` in `(aggregate ...)` | `avg-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.variance` | §8 | `(variance e)` `(stddev e)` `#:sample` in `(aggregate ...)` | `variance-agg` `stddev-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.first` | §8 | `(first e)` in `(aggregate ...)` | `first-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.last` | §8 | `(last e)` in `(aggregate ...)` | `last-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.span` | §8 | `(span)` in `(aggregate ...)` | `span-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.sources` | §8 | `(sources)` / `(sources sub kind)` in `(aggregate ...)` | `sources-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.collect` | §8 | `(collect e)` in `(aggregate ...)` | `collect-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.argmax` | §8 | `(argmax e)` in `(aggregate ...)` | `argmax-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.argmin` | §8 | `(argmin e)` in `(aggregate ...)` | `argmin-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.median` | §8 | `(median e)` in `(aggregate ...)` | `median-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.percentile` | §8 | `(percentile e p)` in `(aggregate ...)` | `percentile-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.count-distinct` | §8 | `(count-distinct e)` in `(aggregate ...)` | `count-distinct-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.mode` | §8 | `(mode e)` in `(aggregate ...)` | `mode-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.top-k` | §8 | `(top-k e k #:by value/frequency)` in `(aggregate ...)` | `top-k-agg` (private/aggregate.rkt) | tests/aggregate-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.approx` | §8 | `(approx-distinct e)` `(approx-percentile e p)` `(approx-frequency e x)` `(count-min e)` in `(aggregate ...)` | `approx-distinct-agg` `approx-percentile-agg` `approx-frequency-agg` (private/sketch.rkt) | tests/sketch-test.rkt, tests/lang-test.rkt |  | partial |
| `AG.incremental` | §8 | windowed `(aggregate ...)` after `(window ...)` in `(aggregate ...)` | `aggregate-accumulator`, two-stack queue, `mono-deque` (private/aggregate.rkt, private/window.rkt) | tests/aggregate-test.rkt, tests/incremental-test.rkt, tests/window-prop-test.rkt |  | partial |
| `JN.window` | §9 | `(join s #:kind window #:window (tumbling #:size d) #:key a #:on θ)` | `window-join-operator` (private/join.rkt) | tests/join-test.rkt, tests/lang-test.rkt |  | partial |
| `JN.interval` | §9 | `(join s #:kind interval #:lower d1 #:upper d2 #:key a)` | `interval-join-operator` (private/join.rkt) | tests/join-test.rkt, tests/lang-test.rkt |  | partial |
| `JN.table` | §9 | `(join t #:kind table #:key a)` | `table-join-operator` (private/join.rkt) | tests/join-test.rkt, tests/lang-test.rkt |  | partial |
| `JN.as-of` | §9 | `(join v #:kind as-of #:key a #:valid-from e)` | `as-of-join-operator` (private/join.rkt) | tests/join-test.rkt, tests/lang-test.rkt |  | partial |
| `JN.outer` | §9 | `#:outer left/right/full` on a join | `#:outer` of the join operators (private/join.rkt) | tests/join-test.rkt, tests/lang-test.rkt |  | partial |
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
