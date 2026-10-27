# 0005. Choices in the stream-structure operations

- Status: accepted
- Date: 2026-10-26
- Spec: §5 `ST.dedupe`, `ST.reorder`, `ST.throttle`, `ST.monotonic-merge`; §2.9 `ES.keyed`

## Context

§5 defines each operation in one line and leaves details open.

## Decision

- **Dedupe.** An event is a duplicate when an earlier-arriving event with the
  same dedup key has a time within the horizon: `|t(e) − t(e')| <= h`. Times
  are end times on the event-time clock (default) or arrival times on the
  processing clock. Duplicates are dropped and counted (`duplicates`); a
  key's remembered times are forgotten a horizon behind the watermark (or
  the clock). The default dedup key is the event id.
- **Reorder.** Release order is `(te, ts, arrival)`. An event is released
  when the watermark reaches its end time; an event arriving with
  `te < W` is late and is dropped and counted (`late`) or, with
  `#:late emit`, passed on at once.
- **Throttle.** Rate buckets are aligned to the epoch, `[kΔ, (k+1)Δ)`.
  "Every n-th" keeps the n-th, 2n-th, ... events. Sampling uses a 48-bit
  linear congruential generator seeded by the program and kept in the
  operator state.
- **High-water-mark merge.** The sub-stream of a source declared with
  `#:key` and `#:substream` is the pair (entity, sub-stream), so sub-stream
  names need not be globally unique. Contributions keep the arrival's type
  and payload, get fresh ids, and record the arrival as their constituent.
- **Partitioning.** The runner partitions every operator by its key
  function. A global watermark is the only cross-key information a run
  carries, which is why the partition property test uses watermark-free
  sources.

## Consequences

- Throttle and dedupe are deterministic under the simulated clock.
- Keyed watermarks (`TM.keyed-watermark`, M5) are per key and do not break
  partitioning.
