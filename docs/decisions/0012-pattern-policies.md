# 0012. Selection, consumption, output, skip, and evaluation policies

- Status: accepted
- Date: 2026-10-27
- Spec: §12

## Context

§12 defines the policies informally, for binary patterns (consumption, after
Snoop) or for flat sequences (selection, skipping, after Flink CEP). The
algebra nests arbitrarily, so each policy needs a definition for any pattern.

## Decision

**Selection** is a condition on every event that follows a step. Each event
in a match records the end point (te, arrival index) of the step before it,
or none at the start of the pattern; in a repetition the step before a copy
is the previous copy. Strict contiguity requires the next global arrival
index (over all keys); partition contiguity the next index within the key;
skip-till-next-match that no event qualifying for the same leaf (type,
predicate, ts > te of the step) arrived in between; skip-till-any-match
nothing. After ACT, "next" is the instance's first event. The engine checks
the condition whenever it joins a step to its predecessor; the oracle
filters by it. Strict ⊆ partition ⊆ next ⊆ any holds on one key.

**Initiator and terminator.** The initiator of a match is its earliest
arriving event (ACT for a match without events); the terminator its latest
arriving event. Candidates completed by one terminator form a group,
ordered by initiator and then larger matches first.

**Order of the policy layer:** evaluation timing, then skipping, then
consumption, then output.

- *Evaluation.* Immediate releases a candidate at once (valid only for
  monotone patterns; the compiler rejects it otherwise). Deferred, forced by
  last and maximal output, releases once the watermark passes the end of the
  match's scope: ts + Δ under a top-level `within Δ`, else te.
- *Skipping.* A reported match discards later candidates whose initiator
  index lies in `[initiator, boundary)`: boundary = initiator + 1 for
  to-next, the terminator + 1 for past-last, the first or last event bound
  to x for to-first(x) and to-last(x). The start is included.
- *Consumption.* Candidates using consumed events are dropped. Unrestricted
  and recent never consume. Recent keeps only candidates whose initiator is
  the newest event that could begin the pattern when they completed.
  Chronicle reports the oldest initiator's candidate per terminator.
  Continuous reports one candidate per initiator and consumes them together.
  Cumulative reports the union of a group as one match. A reported match
  consumes all its events.
- *Output.* First per initiator; last per initiator (the latest terminator,
  the larger on a tie) once its scope has passed; maximal: not strictly
  contained (by events) in any candidate seen.
- *Timed-out partials* (with `#:timeout-partial (Δ clock)`, which needs a
  top-level where clause): the matches of the pattern's body that the where
  clause has not accepted are partials. The largest partial of an initiator
  that has had no reported match for Δ (since the initiator arrived on the
  processing clock, or since its ts on the event-time clock) is reported with
  reason `timeout`; it covers pending partials within it.
- *Retain.* `#:retain (timeout)` makes outputs with that reason consume
  nothing (`CON.retain`).

**Stream output policies.** Edge reports only transitions into and out of a
condition. Rate limit allows one output per Δ of processing time; defer
merges what arrived during the cap into one output (coalescing). Coalesce
merges the outputs of one key at one processing instant: hull interval,
summed multiplicities and named attributes, a count of merged outputs.

## Consequences

- Trace A's full table of §15 is reproduced.
- Selection and consumption compose with every operator of the algebra.
