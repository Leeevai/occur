# 0007. Windows and contexts

- Status: accepted
- Date: 2026-10-26
- Spec: §7

## Context

§7 defines window assignments and contexts in a table. It leaves open how
interval events are assigned, what count, landmark, punctuated and
since-last windows emit and when, how empty windows behave at the end of the
stream, how contexts reach downstream operators, and the geometry of
spatial contexts.

## Decision

- **Assignment.** Interval events are assigned by `ts` (default), by `te`,
  or by overlap (every window intersecting `[ts, te]`). Processing-time
  windows assign by arrival time.
- **Firing.** An event-time window `[s, e)` fires when `W >= e`. With allowed
  lateness `L`, a late event updates the fired window and the window emits a
  correction (⊖old, then new) until `W >= e + L`; afterwards it is dropped and
  later events for it are counted as `late`. The first window is the one
  containing the first event; events for earlier windows are counted as
  `before-first-window`.
- **Empty windows.** With `#:fire-empty`, every grid window from the first
  fires. At the end of the stream (`W = +∞`) the walk stops at the newest
  window holding data. Calendar windows fire empty by default.
- **Count windows** emit every m-th event once n events have been seen.
- **Landmark windows** emit the running result after every event, with
  `te` the newest end time.
- **Punctuated windows** include the opening and closing events as
  constituents; the accumulator sees the events in between; one window is
  open per key at a time; an opening event while open is an ordinary event.
  The output's `reason` is `closed` or `timeout`.
- **Since-last windows** fire when a predicate over the accumulated result
  holds, then reopen empty.
- **Calendar windows** on the processing clock start mid-period at the
  previous tick and coalesce ticks due together into one output with a
  `windows` count; on the event-time clock each period fires on its own.
- **Contexts** tag events with a `context` attribute naming the instance
  (one derived copy per instance), so downstream stages key by it. The state
  and entity contexts are gates. Entities unknown to a directory are
  activated by their first event.
- **Spatial contexts** use planar Euclidean distance on `Location (x, y)`.
  Geodesic distance can be computed with an ordinary function in a
  predicate.

## Consequences

- Windows with a keyed watermark close per key without event-time timers,
  which is what Reflex's window rule needs.
