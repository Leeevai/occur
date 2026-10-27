# 0013. Derivation, identity, delivery, views, and actions

- Status: accepted
- Date: 2026-10-27
- Spec: §13

## Decision

- **Output identity.** `output-identity` hashes (SHA-1 of the printed tuple)
  the instance, key, reason, covered interval, and multiplicity, and adds the
  emission time only for outputs that cover no new data. For any other event,
  `event-identity` hashes its type, payload (without `identity` and
  `emitted-at`), interval, and multiplicity, so redeliveries with new ids
  still deduplicate. Downstream deduplication keeps the first occurrence.
- **Ordered delivery.** The runner delivers the outputs of one processing
  instant grouped by key, keys in order of their first output, FIFO within a
  key; outputs of an earlier instant are always delivered first.
- **Closure.** Every derived stream, including a pattern's, is a stream that
  any stage or pattern can read. A stream may read itself, or a later stream,
  only through `(feedback name)`.
- **Cycle guard.** `make-program` rejects any cycle that does not pass through
  a node declared recursive; the compiler rejects a direct self-loop without
  `#:recursive`. A recursive node drops (and counts) input events whose
  abstraction level reaches the bound, which terminates the loop because each
  pass derives a new level.
- **Actions** run their side effect during the run, once per delivery; after a
  restore or a replay an action may run again (at-least-once), so actions
  should be idempotent.
- **Views** are per-key states queried with `view-ref` after a run, or
  exported every period of processing time as `View` events, aligned to the
  epoch. `classify-health` orders the tests: silent (no arrival within the
  silence bound), quiet (no arrival within the live bound), late (arriving,
  but the newest data is older than the stale bound), live.
- **Retraction** of a constituent retracts every derived event whose
  provenance includes it; operators that keep state of their own (windows)
  emit corrections instead.
