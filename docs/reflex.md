# Reflex in occur

Spec §16 describes Reflex, a stateful trigger engine that turns data
arrivals into triggers, and claims that Reflex is one program in the
specification's vocabulary. [`lib/reflex.rkt`](../lib/reflex.rkt) makes the
claim concrete: it is a small library written in `#lang occur` with public
forms only (`tests/reflex-purity.rkt` checks that it requires no private
module), and the runtime has no case for it. Everything below cites §16
and nothing beyond it.

A Reflex program is a module that requires the library:

```racket
#lang occur
(require "../../lib/reflex.rkt")

(define-devices devices ["d1" watched])
(define-reflex #:arrivals arrivals #:devices devices
               #:contributions contributions #:triggers triggers)

(reflex-rule threshold-check
  #:condition (accumulate #:min-samples 3 #:min-span-s 0)
  #:min-interval-s 60
  #:target "threshold-evaluator"
  #:applies-to ("d1"))
```

`define-reflex` declares the `DataArrival` source, the normalization of
§16.1, and the trigger stream of §16.3 in the module that uses it:

```
arrivals → split per sensor → key by owning device → skew guard
         → device directory → high-water-mark merge          = contributions
contributions → rules → Trigger, delivered by device         = triggers
```

## §16.6, row by row

| Reflex | Spec IDs | occur |
|---|---|---|
| Data-arrival event (a pointer to committed rows, with counts) | `ES.reference`, `ES.batch` | The `DataArrival` event type lists, per sensor written, its owner, sensor id, measurement type, n, `[first_ts, last_ts]`, and newest value (`data-arrival`, `sensor-write`). `define-reflex` splits it with `(split SensorData #:for ([w writes]) … #:ts first_ts #:te last_ts #:n n #:reference sensor #:summary ([newest …]))`: one batch event per sensor, whose `ref` points at the sensor's samples over its interval. |
| Device owns datastreams; measurement types | `ES.substream`, `ST.key-by` | `(key-by owner #:substream sensor #:kind type)`: keyed by the device that owns the sensor, not the one that sent the write; sensors are sub-streams, types their kinds. |
| Monotonic merge: `last_ts` only moves forward; straddling arrivals pro-rated | `ST.monotonic-merge`, `ES.batch` share | `(monotonic-merge)`, keyed by device with one high-water mark per sensor; its contributions are the only input the conditions see. Trace R2. |
| Arrival over an hour ahead of the clock dropped and counted | `TM.skew-guard` | `(skew-guard (hours 1))`, counted as `future-skew`. |
| Thing directory: watched, deprecated, deleted | `CX.entity`, `CX.activation`, `JN.table` | `(define-devices name [device watched] …)` is a `directory` whose statuses map onto active, inactive, and removed; `push-devices` replaces or patches it from a script. `(context (entity devices))` looks each arrival's device up in the directory's current state (a stream–table lookup): watched devices are activated before they send anything, deprecated ones have their state dropped and their arrivals ignored and counted, and deleted ones are forgotten. |
| Rule, binding, `also_thing_ids`, binding key | `RL.template`, `RL.binding`, `RL.fan-out`, `RL.identity` | `reflex-rule` expands into `rule-template`; `(bind rule #:on device #:also (device …) #:key binding-key override … #:params ([k v] …))`, where an override is a condition parameter (`#:min-samples`, `#:min-span-s`, `#:after-s`). An instance is identified by (rule, device, binding key). |
| Sources replaced whole; one owner per rule; dangling bindings reported | `RL.source`, `RL.ownership`, `RL.dangling`, `RL.converge` | `(rule-source name item …)` with templates, bindings, and `(jsonl path)` items; a script pushes a source with `(rule-source-push 'name items)`, replacing it whole. A rule claimed by a second source is refused, a binding of a missing rule is reported, and the instances in force converge to the sources after every push. `examples/reflex/fleet.rkt`. |
| `accumulate` (`min_samples`, `min_span_s`) | `PT.accumulate`, `WN.since-last`, `AG.multiplicity`, `AG.span`, `CON.cumulative` | `(accumulate #:min-samples N #:min-span-s S)` is `(pt:accumulate #:count N #:span S)`: the contributions since the last consuming trigger, all of them reported together, ready once their multiplicity reaches N and their span S. Trace R1. |
| `max_delay_s`, `reset_on_max_delay` | `PT.flush`, `OUT.timeout-partial`, `CON.retain`, `TM.clock` (processing) | `#:max-delay-s Δ` becomes the template's `#:flush Δ`: an accumulation not ready Δ after its first contribution arrived, on the processing clock, is emitted anyway as a timed-out partial with reason `max_delay`. `#:reset-on-max-delay #f` adds `#:retain (max_delay)`: that output keeps its contributions counting. |
| `window` (`window_s`, `offset_s`, `require_all`) | `PT.window-close`, `WN.tumbling`, `TM.keyed-watermark`, `OUT.coalesce` | `(window #:window-s w #:offset-s o #:require-all all?)` is `(pt:window-close #:size w #:offset o #:mode (if all? 'all 'any))`: a tumbling grid anchored at the first contribution, closed by the device's keyed watermark over the watched kinds, windows that closed together coalesced. Trace R3. |
| `silence` (`after_s`, `emit_recovery`), counted from watch start | `PT.silence`, `PT.recovery`, `PT.activation`, `OUT.edge`, `TM.clock` (hybrid), `TM.idle` | `(silence #:after-s Δ #:emit-recovery r)` is `(pt:silence #:after Δ #:recovery r)`: on the hybrid clock (the newest sample time plus Δ, compared with now), with the device's activation as the initiator ACT, so silence counts from watch start and a total silence is seen; with recovery, the first contribution after a silence; only transitions are reported. Trace R4. |
| `cron` (five fields, UTC) | `PT.schedule`, `WN.calendar`, `WN.empty`, `TM.schedule`, `OUT.coalesce` | `(cron "0 * * * *")` is `(pt:schedule "0 * * * *")`: calendar windows in processing time, read in UTC, firing even when empty, ticks missed together coalesced. Trace R5. |
| `min_interval_s`, with capped data carried forward | `OUT.rate-limit` (defer) | `#:min-interval-s Δ` becomes `#:rate-limit (Δ defer)`: an output inside the cap is held until it ends, and an accumulation recomputes its window and samples at release, so nothing is lost. Trace R1. |
| `on_update: continue / restart`; reason `config` | `RL.semantics`, `RL.carry`, `RL.rejudge` | `#:on-update continue` or `restart`. An edit is a new version of the rule (`#:id`, `#:in-source`) in a source a script pushes (`#:at-start? #f`): a job-only edit re-judges with reason `config`, a semantic edit carries progress under `continue` and restarts otherwise. Trace R6. |
| Trigger fields; FIFO grouped by device; content-derived deduplication id | `DR.derive`, `DR.summary`, `DR.ordered`, `DR.output-identity` | `(map Trigger …)` gives each trigger its target, device, binding key, reason, interval, samples, windows, sources, params, emission time, and `identity`; `(key-by device)` delivers them grouped by device; downstream `(dedupe-outputs)` or `dedupe-outputs` drops repeats by identity, which includes the emission time for `config`. |
| Fleet health states | `DR.view`, `TM.schedule` | `(define-fleet-health name #:from contributions #:per device \| sensor #:live d #:silent d #:stale d)` is a `define-view` that exports `classify-health` of arrival age and data age every minute: live, late, quiet, or silent. `examples/reflex/health.rkt`. |
| Checkpoint and restore; cold-start replay with held-back triggers | `TM.checkpoint`, `TM.replay`, `OUT.coalesce` | The script steps `(checkpoint)`, `(restore)`, and `(cold-start)`. A cold start holds the rules node's triggers, releases them coalesced to one per instance at the first rules push, judges each against the instances then in force, and keeps processing-time deadlines waiting until the replay has caught up. `examples/reflex/recovery.rkt`. |

## §16.2, the four conditions

Each Reflex condition is a `define-condition` that names a core condition,
with Reflex's parameter names:

```racket
(define-condition accumulate #:as pt:accumulate
  #:rename ([#:min-samples #:count] [#:min-span-s #:span]))
(define-condition window #:as pt:window-close
  #:rename ([#:window-s #:size] [#:offset-s #:offset]
            [#:require-all #:mode (λ (all?) (if all? 'all 'any))]))
(define-condition silence #:as pt:silence
  #:rename ([#:after-s #:after] [#:emit-recovery #:recovery]))
(define-condition cron #:as pt:schedule #:rename ())
```

The modifiers of §16.2 belong to the rule, so `reflex-rule` maps them onto
`rule-template`: `#:max-delay-s` to `#:flush`, `#:reset-on-max-delay #f`
to `#:retain (max_delay)`, `#:min-interval-s` to `#:rate-limit (Δ defer)`,
and `#:on-update` through.

## §16.7, the acceptance traces

| Trace | Program | Test |
|---|---|---|
| R1: accumulate with a deferring rate limit | `examples/reflex/r1-accumulate.rkt` | `tests/reflex-test.rkt` |
| R2: high-water-mark merge | `examples/reflex/r2-merge.rkt` | `tests/reflex-test.rkt` |
| R3: window with `require_all` | `examples/reflex/r3-window.rkt` | `tests/reflex-test.rkt` |
| R4: silence from activation, with recovery | `examples/reflex/r4-silence.rkt` | `tests/reflex-test.rkt` |
| R5: schedule with missed ticks | `examples/reflex/r5-cron.rkt` | `tests/reflex-test.rkt` |
| R6: edits | `examples/reflex/r6-job-edit.rkt`, `examples/reflex/r6-semantic-edit.rkt` | `tests/reflex-test.rkt` |

Each test checks every output of its trace exactly: reason, window,
samples, windows, params, and emission time. Run a program with, for
example, `racket examples/reflex/r1-accumulate.rkt`.

## §16.5, outside the language

The library makes these possible without implementing them, as §16.5
asks. Every operator is keyed by device and none reads another device's
state, so one evaluator per key partition suffices; checkpoints are values
of the runner, wherever a deployment keeps them; the transport is the
scripted runner's steps and outputs; and there is no HTTP API, metrics
export, or service that computes rule sources from other systems.
