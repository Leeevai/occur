# 0011. The incremental pattern engine

- Status: accepted
- Date: 2026-10-27
- Spec: §11, §12 (`EVAL.*`), §17

## Context

The architecture names the operational engine `pattern/nfa.rkt`. A flat
NFA handles sequences, Kleene closure, and negation well, but not
conjunction, k-of-n, or the aperiodic and periodic forms over composite
sub-patterns, all of which the algebra allows to nest arbitrarily. The
engine must agree with the oracle exactly under the most permissive
semantics.

## Decision

- The engine is a **tree of incremental match automata**, one per AST
  node, each with buffers of the complete sub-matches it has seen (the
  partial-match buffers of NFA^b, kept per node). On each input a node
  returns only its new matches. Sequence, conjunction, and k-of-n nodes join
  a child's new matches with the other children's buffers (delta joins), so
  events may arrive in any order. A Kleene node grows chains on both sides
  of each new copy.
- **Deferred decisions.** Non-monotone nodes hold candidates until the
  watermark guarantees the answer:
  - negation between: when `W >= ts(m₂)` (exact: a blocking q needs
    `te(q) < ts(m₂)`);
  - span absence: when `W >= te(m)` (exact);
  - absence: when `W > te(m) + Δ`, as §11.3 says, or on the hybrid clock when
    `now >= te(m) + Δ`;
  - aperiodic pairs when `W > te(e₂)`, cumulative intervals and periodic
    closes when `W > ts(close)`, periodic ticks when `W > t`.
  The rules with `>` are exact for point events and the spec's heuristic
  for interval events. Strict event-time deadlines (`strictly-after`) wake
  the operator once `W` passes them.
- **Horizon.** A periodic pattern without a close ticks as the watermark
  passes; at the end of the stream it ticks up to the stream's newest finite
  watermark, or the key's newest end time if larger. The reference operator
  evaluates the oracle with the same horizon.
- **Selection** is checked where a step follows another (decision 0012), so
  strict and skip-till-next-match prune while matching.
- **State** is immutable and kept per key. The engine reports its next
  event-time and processing-time deadlines; only undecided work is reported.
- **Equivalence.** Property tests (1000 random patterns and histories each)
  check that the engine equals the oracle for shuffled arrivals with the
  watermark only at the end, and for time-ordered point events with the
  watermark advancing; and that the incremental and reference operators
  agree on keyed streams.

## Consequences

- Buffers grow with the history of a key; `within` bounds what is useful but
  buffers are not pruned yet (§17 "State bounds").
- The file keeps the planned name `nfa.rkt`.
