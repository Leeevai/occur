# 0001. The time domain is the extended reals, in seconds

- Status: accepted
- Date: 2026-10-26
- Spec: §2.1 `ES.time`

## Context

The spec lets the time domain 𝕋 be any totally ordered set with addition of
durations: ℕ, ℤ, or ℝ≥0. High-water marks start at −∞ (§5), keyed watermarks
are −∞ for a kind never seen (§6), and bounded repetition allows m = ∞ (§11).
Calendar schedules read five-field cron expressions in UTC (§6), which needs a
mapping from 𝕋 to civil time.

## Decision

- 𝕋 is the Racket reals that are not NaN, extended with `-inf.0` and `+inf.0`.
- The unit is the second. `(minutes 10)` is `600`. Times that represent
  instants are seconds since the Unix epoch (UTC) whenever calendar operations
  apply to them.
- Exact numbers are recommended and every runtime operation preserves
  exactness, so shares, rounding, and interval arithmetic are exact.
- Intervals are closed `[start, end]` with `start <= end`; a point is the
  degenerate interval. Windows are half-open `[s, e)` (§7) and are modelled
  separately.

## Consequences

- Traces given in minutes (§15) are written in seconds in tests
  (t = 1 min becomes 60).
- `round` on exact rationals is round-half-to-even, exactly as `ES.batch`
  requires.
