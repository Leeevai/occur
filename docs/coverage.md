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
| `TM.checkpoint` | §6 | script steps `(checkpoint)` and `(restore)` | `checkpoint` `restore` `take-snapshot!` `restore-snapshot!` (private/dataflow.rkt) | tests/checkpoint-test.rkt, tests/lang-test.rkt |  | partial |
| `TM.replay` | §6 | script step `(cold-start)`; held outputs released at the first `rule-source-push` | `cold-start` `cold-start!` `release-held!` `default-coalesce` (private/dataflow.rkt), `coalesce-rule-outputs` (private/rules-engine.rkt) | tests/checkpoint-test.rkt, tests/lang-test.rkt |  | partial |
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
| `AL.before` | §10 | `(before? a b)` / `(allen-relation a b)` in stage and pattern expressions | `before?` `allen-relation` (private/allen.rkt) | tests/allen-test.rkt, tests/lang-test.rkt |  | partial |
| `AL.after` | §10 | `(after? a b)` / `(allen-relation a b)` in stage and pattern expressions | `after?` `allen-relation` (private/allen.rkt) | tests/allen-test.rkt, tests/lang-test.rkt |  | partial |
| `AL.meets` | §10 | `(meets? a b)` / `(allen-relation a b)` in stage and pattern expressions | `meets?` `allen-relation` (private/allen.rkt) | tests/allen-test.rkt, tests/lang-test.rkt |  | partial |
| `AL.met-by` | §10 | `(met-by? a b)` / `(allen-relation a b)` in stage and pattern expressions | `met-by?` `allen-relation` (private/allen.rkt) | tests/allen-test.rkt, tests/lang-test.rkt |  | partial |
| `AL.overlaps` | §10 | `(overlaps? a b)` / `(allen-relation a b)` in stage and pattern expressions | `overlaps?` `allen-relation` (private/allen.rkt) | tests/allen-test.rkt, tests/lang-test.rkt |  | partial |
| `AL.overlapped-by` | §10 | `(overlapped-by? a b)` / `(allen-relation a b)` in stage and pattern expressions | `overlapped-by?` `allen-relation` (private/allen.rkt) | tests/allen-test.rkt, tests/lang-test.rkt |  | partial |
| `AL.starts` | §10 | `(starts? a b)` / `(allen-relation a b)` in stage and pattern expressions | `starts?` `allen-relation` (private/allen.rkt) | tests/allen-test.rkt, tests/lang-test.rkt |  | partial |
| `AL.started-by` | §10 | `(started-by? a b)` / `(allen-relation a b)` in stage and pattern expressions | `started-by?` `allen-relation` (private/allen.rkt) | tests/allen-test.rkt, tests/lang-test.rkt |  | partial |
| `AL.during` | §10 | `(during? a b)` / `(allen-relation a b)` in stage and pattern expressions | `during?` `allen-relation` (private/allen.rkt) | tests/allen-test.rkt, tests/lang-test.rkt |  | partial |
| `AL.contains` | §10 | `(contains? a b)` / `(allen-relation a b)` in stage and pattern expressions | `contains?` `allen-relation` (private/allen.rkt) | tests/allen-test.rkt, tests/lang-test.rkt |  | partial |
| `AL.finishes` | §10 | `(finishes? a b)` / `(allen-relation a b)` in stage and pattern expressions | `finishes?` `allen-relation` (private/allen.rkt) | tests/allen-test.rkt, tests/lang-test.rkt |  | partial |
| `AL.finished-by` | §10 | `(finished-by? a b)` / `(allen-relation a b)` in stage and pattern expressions | `finished-by?` `allen-relation` (private/allen.rkt) | tests/allen-test.rkt, tests/lang-test.rkt |  | partial |
| `AL.equals` | §10 | `(equals? a b)` / `(allen-relation a b)` in stage and pattern expressions | `equals?` `allen-relation` (private/allen.rkt) | tests/allen-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.sequence` | §11 | `(seq P Q ...)` | `seq` `p:seq` | tests/reference-oracle-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.primitive` | §11 | `(x : T pred)` / `(T pred)` / `T` | `prim` `p:primitive` (private/pattern/ast.rkt); oracle (private/pattern/reference.rkt) | tests/reference-oracle-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.activation` | §11 | `act` | `act` `p:act`; ACT at the instance's activation instant | tests/reference-oracle-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.conjunction` | §11 | `(and P Q ...)` | `conj` `p:and` | tests/reference-oracle-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.disjunction` | §11 | `(or P Q ...)` | `disj` `p:or` | tests/reference-oracle-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.negation` | §11 | `(seq P (absent Q) R)` | `neg-between` `p:neg-between` | tests/reference-oracle-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.absence` | §11 | `(seq P (absent Q #:within d))` | `absence` `p:absence` | tests/reference-oracle-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.span-absence` | §11 | `(unless P Q)` | `span-absence` `p:unless` | tests/reference-oracle-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.kleene-plus` | §11 | `(kleene+ P)` | `kleene+` `p:repeat` | tests/reference-oracle-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.kleene-star` | §11 | `(kleene* P)` | `kleene*` `p:repeat` | tests/reference-oracle-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.repeat` | §11 | `(repeat P n m)` | `repeat` `p:repeat` | tests/reference-oracle-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.any` | §11 | `(any k P ...)` | `any-of` `p:any` | tests/reference-oracle-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.within` | §11 | `(within P d)` / `#:within d` | `within` `p:within` | tests/reference-oracle-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.where` | §11 | `(where P θ)` / `#:where θ` | `where` `p:where`, where-helpers | tests/reference-oracle-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.aperiodic` | §11 | `(aperiodic S X E)` | `aperiodic` `p:aperiodic` | tests/reference-oracle-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.aperiodic-cumulative` | §11 | `(aperiodic* S X E)` | `aperiodic*` `p:aperiodic` | tests/reference-oracle-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.periodic` | §11 | `(periodic S d E)` | `periodic` `p:periodic` | tests/reference-oracle-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.periodic-cumulative` | §11 | `(periodic* S d E)` | `periodic*` `p:periodic` | tests/reference-oracle-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.threshold` | §11 | `crosses?` / `threshold-pattern` in `#:where` | `crosses?` `threshold-pattern` (private/pattern/families.rkt) | tests/families-test.rkt |  | partial |
| `PT.trend.increasing` | §11 | `(increasing b.x)` in `#:where` | `increasing?` (private/pattern/families.rkt) | tests/families-test.rkt |  | partial |
| `PT.trend.decreasing` | §11 | `(decreasing b.x)` | `decreasing?` | tests/families-test.rkt |  | partial |
| `PT.trend.non-increasing` | §11 | `(non-increasing b.x)` | `non-increasing?` | tests/families-test.rkt |  | partial |
| `PT.trend.non-decreasing` | §11 | `(non-decreasing b.x)` | `non-decreasing?` | tests/families-test.rkt |  | partial |
| `PT.trend.stable` | §11 | `(stable b.x eps)` | `stable?` | tests/families-test.rkt |  | partial |
| `PT.trend.mixed` | §11 | `(mixed b.x)` | `mixed?` `trend` | tests/families-test.rkt |  | partial |
| `PT.relative` | §11 | `(relative-max? x xs)` in `#:where` | `relative-max?` `relative-min?` | tests/families-test.rkt |  | partial |
| `PT.spatial.distance` | §11 | `within-distance?` `min-distance` ... in `#:where` | `within-distance?` `beyond-distance?` `min/max/avg-distance` | tests/families-test.rkt |  | partial |
| `PT.spatial.moving` | §11 | `moving-toward?` `moving-away?` `constant-direction?` | (private/pattern/families.rkt) | tests/families-test.rkt |  | partial |
| `PT.accumulate` | §11 | `(pt:accumulate #:count n #:span s)`; `accumulate-pattern` | accumulate condition (private/rules-engine.rkt), `accumulate-pattern` (private/pattern/families.rkt) | tests/families-test.rkt, tests/reflex-traces-runtime-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.flush` | §11 | `#:timeout-partial (d processing-time)` with `#:retain (timeout)` | `policy-partials` (private/pattern/policies.rkt) | tests/policies-test.rkt, tests/lang-test.rkt |  | partial |
| `PT.silence` | §11 | `(pt:silence #:after d #:recovery b)`; `silence-pattern` | silence condition (private/rules-engine.rkt), `silence-pattern` | tests/families-test.rkt, tests/reflex-traces-runtime-test.rkt |  | partial |
| `PT.recovery` | §11 | `(pt:silence ... #:recovery #t)`; `recovery-pattern` | silence condition, `recovery-pattern` | tests/families-test.rkt, tests/reflex-traces-runtime-test.rkt |  | partial |
| `PT.window-close` | §11 | `(pt:window-close #:size w #:offset o #:mode all)` | window-close condition (private/rules-engine.rkt), `window-close-operator` (private/pattern/families.rkt) | tests/families-test.rkt, tests/reflex-traces-runtime-test.rkt |  | partial |
| `PT.schedule` | §11 | `(pt:schedule "cron")`, `(window (calendar "cron"))`, `define-schedule` | schedule condition (private/rules-engine.rkt), `calendar-window-operator` | tests/families-test.rkt, tests/reflex-traces-runtime-test.rkt |  | partial |
| `SEL.next` | §12 | `#:selection next` / `skip-till-next-match` | `strategy-check` (private/pattern/policies.rkt) | tests/selection-test.rkt, tests/policies-test.rkt, tests/lang-test.rkt |  | partial |
| `SEL.strict` | §12 | `#:selection strict` | `strategy-check` registered with the engine; `select-matches` (private/pattern/policies.rkt) | tests/selection-test.rkt, tests/policies-test.rkt, tests/lang-test.rkt |  | partial |
| `SEL.partition` | §12 | `#:selection partition` | `strategy-check` (private/pattern/policies.rkt) | tests/selection-test.rkt, tests/lang-test.rkt |  | partial |
| `SEL.any` | §12 | `#:selection any` (default) | engine delta joins (private/pattern/nfa.rkt) | tests/nfa-test.rkt, tests/selection-test.rkt |  | partial |
| `CON.cumulative` | §12 | `#:consumption cumulative` | policy layer | tests/policies-test.rkt, tests/lang-test.rkt |  | partial |
| `CON.retain` | §12 | `#:retain (timeout)` | `make-match-policy #:retain` | tests/policies-test.rkt, tests/lang-test.rkt |  | partial |
| `CON.recent` | §12 | `#:consumption recent` | policy layer, `initial-leaves` | tests/policies-test.rkt, tests/lang-test.rkt |  | partial |
| `CON.unrestricted` | §12 | `#:consumption unrestricted` (default) | policy layer (private/pattern/policies.rkt) | tests/policies-test.rkt, tests/nfa-test.rkt |  | partial |
| `CON.chronicle` | §12 | `#:consumption chronicle` | policy layer | tests/policies-test.rkt, tests/lang-test.rkt |  | partial |
| `CON.continuous` | §12 | `#:consumption continuous` | policy layer | tests/policies-test.rkt |  | partial |
| `OUT.rate-limit` | §12 | `(rate-limit d #:mode drop/defer)` stage | `rate-limit-operator` (private/ops/output.rkt) | tests/output-test.rkt, tests/lang-test.rkt |  | partial |
| `OUT.coalesce` | §12 | `(coalesce #:sum (a ...))` stage; schedules and calendar windows coalesce ticks | `coalesce-operator` `coalesce-events` (private/ops/output.rkt); held replay outputs (private/dataflow.rkt) | tests/output-test.rkt, tests/progress-test.rkt, tests/families-test.rkt, tests/checkpoint-test.rkt |  | partial |
| `OUT.first` | §12 | `#:output first` | policy layer | tests/policies-test.rkt, tests/lang-test.rkt |  | partial |
| `OUT.timeout-partial` | §12 | `#:timeout-partial (d clock)` | `policy-partials` (private/pattern/policies.rkt) | tests/policies-test.rkt, tests/lang-test.rkt |  | partial |
| `OUT.edge` | §12 | `(edge #:enter e #:leave e)` stage | `edge-operator` (private/ops/output.rkt) | tests/output-test.rkt, tests/lang-test.rkt |  | partial |
| `OUT.every` | §12 | `#:output every` (default) | policy layer | tests/policies-test.rkt, tests/nfa-test.rkt |  | partial |
| `OUT.last` | §12 | `#:output last` | policy layer | tests/policies-test.rkt |  | partial |
| `OUT.maximal` | §12 | `#:output maximal` | policy layer | tests/policies-test.rkt, tests/lang-test.rkt |  | partial |
| `SKIP.no-skip` | §12 | `#:skip no-skip` (default) | `skip-range` (private/pattern/policies.rkt) | tests/policies-test.rkt |  | partial |
| `SKIP.to-next` | §12 | `#:skip to-next` | `skip-range` | tests/policies-test.rkt |  | partial |
| `SKIP.past-last` | §12 | `#:skip past-last` | `skip-range` | tests/policies-test.rkt, tests/lang-test.rkt |  | partial |
| `SKIP.to-first` | §12 | `#:skip (to-first x)` | `skip-range` | tests/policies-test.rkt |  | partial |
| `SKIP.to-last` | §12 | `#:skip (to-last x)` | `skip-range` | tests/policies-test.rkt |  | partial |
| `EVAL.immediate` | §12 | `#:eval immediate` (compile-time check: monotone only) | policy layer | tests/policies-test.rkt, tests/lang-test.rkt |  | partial |
| `EVAL.deferred` | §12 | `#:eval deferred`; forced by last/maximal | engine deferred nodes, policy releases | tests/nfa-test.rkt, tests/policies-test.rkt |  | partial |
| `DR.summary` | §13 | `(summarize)` stage | `summarize` `summarized-output` `make-summary` (private/derive.rkt) | tests/derive-test.rkt, tests/lang-test.rkt |  | partial |
| `DR.view` | §13 | `(define-view ...)`, `view-ref`, `classify-health` | `view-operator` `view-ref` `classify-health` (private/derive.rkt) | tests/derive-test.rkt, tests/lang-test.rkt |  | partial |
| `DR.output-identity` | §13 | `(dedupe-outputs)` stage | `output-identity` `event-identity` `dedupe-outputs` (private/derive.rkt) | tests/derive-test.rkt, tests/lang-test.rkt, tests/checkpoint-test.rkt |  | partial |
| `DR.derive` | §13 | `#:emit (T [a e] ...)` in `define-pattern` | `derive-match` (private/pattern/operators.rkt) | tests/derive-test.rkt, tests/lang-test.rkt |  | partial |
| `DR.ordered` | §13 | delivery grouped by key per instant (runner) | `flush-outbox!` (private/dataflow.rkt), `delivery-groups` (private/derive.rkt) | tests/derive-test.rkt, tests/lang-test.rkt |  | partial |
| `DR.closure` | §13 | any stage or pattern reads derived streams | programs over nodes (private/dataflow.rkt) | tests/derive-test.rkt, tests/lang-test.rkt |  | partial |
| `DR.cycle-guard` | §13 | `(feedback s)` stage, `#:recursive n` | `make-program` cycle check, recursive bound (private/dataflow.rkt) | tests/dataflow-test.rkt, tests/lang-test.rkt |  | partial |
| `DR.act` | §13 | `(act name #:on s #:do e #:completion T)` | `action-operator` (private/derive.rkt) | tests/derive-test.rkt, tests/lang-test.rkt |  | partial |
| `DR.retract` | §13 | retractions in source data propagate | provenance index in the runner (private/dataflow.rkt) | tests/derive-test.rkt, tests/dataflow-test.rkt, tests/lang-test.rkt |  | partial |
| `RL.rejudge` | §14 | job-only edits and new bindings on keys with data | `reconcile`, `config` outputs (private/rules-engine.rkt) | tests/rules-engine-test.rkt, tests/reflex-traces-runtime-test.rkt |  | partial |
| `RL.template` | §14 | `(rule-template id #:condition c ...)`, `define-condition` | `make-rule` `rule` (private/rules.rkt) | tests/rules-test.rkt, tests/lang-test.rkt |  | partial |
| `RL.binding` | §14 | `(bind t #:on k #:key bk override ... #:params (...))` | `make-binding` `effective-spec` (private/rules.rkt) | tests/rules-test.rkt, tests/lang-test.rkt |  | partial |
| `RL.fan-out` | §14 | `(bind t #:on k #:also (k ...))` | `binding-keys` (private/rules.rkt) | tests/rules-test.rkt, tests/lang-test.rkt |  | partial |
| `RL.identity` | §14 | instances keyed by (rule, key, binding key) | `instances-for` `ispec-identity` (private/rules.rkt) | tests/rules-test.rkt |  | partial |
| `RL.source` | §14 | `(rule-source name item ...)`, `rule-source-push`, JSON Lines items | `apply-source-msg` `rules-in-force` `read-jsonl-rule-items` (private/rules.rkt) | tests/rules-test.rkt, tests/rules-json-test.rkt, tests/lang-test.rkt |  | partial |
| `RL.ownership` | §14 | a rule defined in two sources is refused | `rules-in-force` (private/rules.rkt) | tests/rules-test.rkt |  | partial |
| `RL.dangling` | §14 | reports in the run result | `rules-in-force`, `rules-report` (private/rules.rkt) | tests/rules-test.rkt, tests/rules-engine-test.rkt, tests/rules-json-test.rkt |  | partial |
| `RL.semantics` | §14 | edits classified on push | `fingerprint` `classify-edit` (private/rules.rkt) | tests/rules-test.rkt, tests/reflex-traces-runtime-test.rkt |  | partial |
| `RL.carry` | §14 | `#:on-update continue / restart` | `reconcile` (private/rules-engine.rkt) | tests/rules-engine-test.rkt, tests/reflex-traces-runtime-test.rkt |  | partial |
| `RL.converge` | §14 | re-pushing an identical source | `reconcile` (no edit, no output) | tests/rules-engine-test.rkt |  | partial |
