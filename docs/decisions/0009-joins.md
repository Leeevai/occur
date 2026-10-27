# 0009. Joins

- Status: accepted
- Date: 2026-10-26
- Spec: §9

## Context

§9 defines the join relations but not their payloads, how often a pair is
emitted under overlapping windows, when outer results appear, or how a table
and versions are maintained.

## Decision

- Joins are equi-joins on a key shared by both sides (the runner partitions
  them), refined by θ(s, r). The left input is port 0, the right port 1.
- The default payload is the union of both payloads, the left winning on
  conflicts; `#:select` builds it from `left.attr` and `right.attr`.
- A windowed join emits each pair once, when its second event arrives, even
  if the pair shares several sliding windows (the spec defines a relation).
- Outer results carry the other side's attributes as `#f` (null) and are
  emitted when the watermark closes the event's last window (windowed) or
  passes its match range (interval).
- A table join's right input upserts the row of its key; a retraction of the
  current row deletes it. Each left event is matched at once against the
  current row; misses are counted (`unmatched`) or, as a left outer join,
  passed on.
- An as-of join keeps every version and holds each left event until the
  watermark reaches t(s), so late versions still count; it then picks the
  version with the greatest valid-from not after t(s). Releases happen in
  event-time order.

## Consequences

- Inner windowed and interval joins are monotone and emit immediately;
  outer and as-of joins need watermarks.
