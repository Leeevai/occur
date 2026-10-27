# 0015. Checkpoint, restore, and cold-start replay

- Status: accepted
- Date: 2026-10-27
- Spec: §6 (TM.checkpoint, TM.replay), §13 (OUT.coalesce, DR.output-identity), §16.4

## Context

The specification asks for a snapshot of every operator's state, the read
position of every source, and the outputs not yet delivered; restoring it
resumes exactly after the snapshot. Without a checkpoint the engine rebuilds
state from retained input, holds outputs back, coalesces them per operator
instance, releases them once the sources have caught up and the
configuration has loaded, and judges each one against the instances then in
force. It leaves open what "retained input" and "caught up" mean for a
single-process scripted runner, and how held outputs merge.

The plan placed this in `private/checkpoint.rkt`. A snapshot has to read and
replace every runner field, so it lives in `private/dataflow.rkt` next to the
runner instead.

## Decision

- **Retained input** is the runner's log of every data, watermark, control,
  and clock step, in order. Checkpoint and restore steps are not logged.
- **Checkpoint** (`(checkpoint)`) first delivers the outbox, then records the
  processing time, every node's global and keyed state, both deadline sets,
  watermarks, counters, provenance, the id counter, per-node arrival
  sequence numbers, the timer suspension flag, and the read position (the
  log's length). Runner state is immutable, so a snapshot is a set of
  references, not a copy.
- **Restore** (`(restore)`) reinstates the snapshot and re-runs the logged
  steps after its read position, as a source would re-read them. Outputs
  already delivered stay delivered, so the outputs of that interval repeat,
  with the same identities; `dedupe-outputs` removes the repeats. A restore
  without an earlier checkpoint is an error.
- **Cold start** (`(cold-start)`) resets every node to its initial state,
  restarts the generators, and re-runs the whole log at the current
  processing time with the clock frozen: clock steps are skipped and
  processing-time deadlines do not fire, so replayed arrivals are not taken
  for silence or for flush deadlines. Event-time timers still fire, because
  watermarks are replayed.
- **Caught up** means the log has been fully replayed. Processing-time
  deadlines then resume, and fire at once if they came due during the
  replay.
- **Release.** If the program has a configuration operator (the rules node),
  held outputs wait for the next configuration control step, the first rules
  push after the restart; otherwise they are released when the replay has
  caught up. Held outputs are grouped per operator instance: by node and
  output key, where an operator may name its outputs' instance (the rules
  operator names (rule, key, binding key)). A group of more than one is
  merged by the operator's coalesce function, or by the default (the latest
  output, its interval the hull, its multiplicity the sum, a `merged`
  count). The rules operator keeps the latest reason, takes the hull of the
  windows, sums windows and samples, and recomputes the identity. Each merged output is then judged against the
  state in force; an output whose instance is gone is dropped and counted as
  `dropped-after-replay`.

## Consequences

- The property "checkpoint, restore, continue gives the uninterrupted run's
  outputs after deduplication" holds for arbitrary programs and is tested
  over generated runs (`tests/checkpoint-test.rkt`).
- A cold start differs from the uninterrupted run only in the held outputs,
  which arrive merged at release time.
- The runner keeps the whole log; a deployment would bound retention, which
  §16.5 places outside the language.
