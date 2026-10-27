# 0006. Calendar schedules and keyed watermarks

- Status: accepted
- Date: 2026-10-26
- Spec: §6 `TM.schedule`, `TM.keyed-watermark`, `TM.idle`, `TM.lateness`; §7 `WN.calendar`; §16.7 R5

## Context

`TM.schedule` names "a five-field cron expression read in UTC" without
defining the dialect, and says missed ticks fire once, together. R5 says an
instance created at 10:20 on an hourly schedule has its first tick at 11:00
and its first period starting at 10:00. `TM.keyed-watermark` leaves open
which kinds count when an operator watches "all kinds".

## Decision

- **Dialect.** Fields are minute (0-59), hour (0-23), day of month (1-31),
  month (1-12 or `JAN`-`DEC`), and day of week (0-7 or `SUN`-`SAT`, with 0
  and 7 both Sunday). A field is a comma list of `*`, `n`, or `a-b`, each with
  an optional `/step`; `a/step` means `a-max/step`. When both day fields are
  restricted, a day matches if either matches; when one is `*`, the other
  decides. Everything is UTC; there are no time zones or seconds.
- **Ticks.** `cron-next` is the first tick strictly after a time and
  `cron-prev` the last at or before it. A schedule created at `t` first
  fires at `cron-next(t)`; its first calendar period starts at
  `cron-prev(t)` (10:00 for R5).
- **Coalescing.** A schedule wakes once per deadline. If several ticks are
  due when it wakes (after a suspension), it emits one Tick event with the
  count of ticks and the interval `[first tick, last tick]`.
- **Compile-time validation.** `define-schedule` parses its expression at
  expansion time, so a malformed schedule is a syntax error.
- **Keyed watermarks.** With an explicit list of watched kinds, a kind never
  seen counts as −∞. With no list ("all kinds"), the watched kinds are the
  kinds seen so far on the key.
- **Idleness.** The idle operator notifies once per silence and re-arms when
  data arrives. The heartbeat watermark strategy lets a source keep event
  time moving while idle.
- **Lateness.** "Late" means `te(e) < W` for the node's input watermark.
  Allowed lateness accepts `te(e) >= W − L`.

## Consequences

- R5 is reproduced by the schedule generator and, in M14, by the rules
  engine's cron condition.
