# 0008. Aggregate definitions

- Status: accepted
- Date: 2026-10-26
- Spec: §8

## Context

§8 lists the aggregates and their classes but leaves exact definitions open:
which percentile method, how ties break, sample or population variance,
what "first" means for equal times, and which sketches and parameters back
`AG.approx`.

## Decision

- `count` is the multiplicity sum Σ n(e) (`AG.multiplicity`); `events` is
  the number of events. Value aggregates read one value per event, not
  weighted by multiplicity.
- `variance` and `stddev` are population statistics by default; `#:sample`
  selects n − 1.
- `percentile p` uses the nearest-rank method: the value at rank ⌈p·n⌉ of
  the sorted values, the minimum for p = 0. `median` is the middle value, or
  the mean of the two middle values for an even count.
- Ties: `first` keeps the earliest arrival among equal start times and
  `last` the latest; `argmax`/`argmin` keep the earliest arrival; `mode` and
  `top-k #:by frequency` prefer the value seen first.
- `top-k` by value returns the k largest values with repetitions, largest
  first.
- Holistic aggregates keep a multiset, so they also combine and support
  remove.
- Sketches use a deterministic 64-bit hash (FNV-1a of the printed value,
  then splitmix64). HyperLogLog uses 2^12 registers with linear counting for
  small cardinalities; the t-digest is the merging variant with the k1 scale
  function and compression 100, keeping the exact minimum and maximum;
  Count-Min is 5 × 272 by default (ε ≈ 1%, δ ≈ 0.7%).
- A windowed aggregation emits a `Window` event; a standalone aggregate
  emits a running `Aggregate` event per key after each input.
- Incremental maintenance: invertible accumulators use remove, associative
  ones a two-stack queue, others are refolded.

## Consequences

- Results are reproducible bit for bit across runs.
