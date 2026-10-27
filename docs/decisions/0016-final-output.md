# 0016. When last and maximal output are final, and the oracle's policies

- Status: accepted (amends 0012)
- Date: 2026-10-27
- Spec: §12.3 (OUT.last, OUT.maximal), §12.4 (EVAL.deferred), §15, §17

## Context

Decision 0012 released a deferred candidate once the watermark passed the
end of its scope: ts + Δ under a top-level `within Δ`, else te. For last
and maximal output the specification asks for more: evaluation "must be
deferred until no further extension is possible". Without a bound a later
event can always extend a match, so releasing at te reported a match and
then its extension. A differential property of the policy layer against
the oracle found this, on a pattern without `within` and on the empty
match of a Kleene star.

The reference engine implemented only the most permissive semantics, so it
could not check the card-testing example of §15 under the strategies of
its table.

## Decision

- **Scope end.** Under a top-level `within Δ` a candidate's scope ends at
  ts + Δ, as in 0012: a match that contains it starts no later and spans
  at most Δ, so it has been seen by then. Without a top-level bound, last and
  maximal output wait for the end of the stream (the final watermark).
  Under maximal output a match without events also waits for the end of
  the stream, because every other match contains it. Every and first
  output keep 0012's rule.
- **Warning.** `define-pattern` warns when a pattern with last or maximal
  output has no bound.
- **The oracle's policies.** The reference engine filters the oracle's
  matches by any selection strategy, reading strict contiguity from each
  event's arrival index over all keys, and reports every output or the
  maximal matches of the complete set. Other consumption, output, and skip
  policies still need the incremental engine.

## Consequences

- The incremental and reference operators agree per key under every
  selection strategy with every or maximal output (a property over random
  patterns and histories).
- The card-testing example runs on both engines, and the acceptance test
  compares them on traces A, A′, and B.
- Unbounded last and maximal patterns hold their candidates until the end
  of the stream, which the warning makes visible.
