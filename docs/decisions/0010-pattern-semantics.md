# 0010. Pattern semantics: choices left open by §11

- Status: accepted
- Date: 2026-10-26
- Spec: §11.1–§11.4

## Context

§11.3 gives the denotation of each operator but leaves some points open:
ties in sequences, negated sub-patterns that use outer bindings, nullable
neighbours of a negation, the meaning of "q ∉ m", which closer ends an
aperiodic interval, and how bindings behave under repetition.

## Decision

- **Ties.** Events with equal timestamps are not in sequence: `P₁ ; P₂`
  requires `te(m₁) < ts(m₂)` strictly. There is no tiebreak rule.
- **Closed negation.** A negated pattern (`¬Q`, `unless Q`, the closers of
  aperiodic and periodic forms) is evaluated on its own: it sees no outer
  bindings. Correlate it with key-by (the spec's recommendation for
  per-key patterns) or use a `where` clause on the whole match.
- **Nullable neighbours.** The neighbours of a negation must not be
  nullable, and a repeated pattern must not be nullable; such patterns are
  rejected (`pattern-problem`). This keeps every negation gap bounded by
  real events.
- **Span absence.** `q ∉ m` means q is not part of m: its events are not all
  among m's events.
- **Aperiodic and periodic closers.** The interval opened by e₁ closes at
  the first later closer, the one with the smallest start; ties go to the
  earliest end, then the earliest arrival. PER without a closer ticks up to
  the evaluation horizon (the newest end time by default).
- **Bindings.** A binding under a repetition is a list in time order;
  repetitions of repetitions concatenate. The empty match of `P*` binds its
  names to empty lists. A binding may not be used both inside and outside a
  repetition.
- **ACT** is the empty match at the activation instant of the context
  instance: the moment a key's operator instance was created, or the
  activation signal of an entity context.
- **Derivation.** A match without events (from ACT) derives an event with a
  summary instead of constituents, starting at its deadline when it has no
  start.
- **Leaves and predecessors.** Each event in a match records the leaf it
  was taken for and the end point of the step before it, which is how the
  selection strategies of §12 are defined (decision 0011).

## Consequences

- The oracle and the engine agree on every pattern the AST accepts.
