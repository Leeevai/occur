# 0002. Event identifiers are deterministic counters

- Status: accepted
- Date: 2026-10-26
- Spec: §2.3 `ES.event`, §13 `DR.output-identity`, §6 `TM.checkpoint`

## Context

Every event needs a globally unique identifier. The runner must be
deterministic so that acceptance traces and property tests (checkpoint and
restore, partitioned runs) are reproducible. Random or time-based ids would
break both.

## Decision

- An id is a string: a prefix followed by a counter (`"e1"`, `"e2"`, ...).
- The counter is the parameter `current-id-source`. The runner installs a
  fresh counter per run and saves it in checkpoints, so a restored run
  regenerates the same ids.
- Source events may carry ids from their data; derived events get fresh ids.
- Deduplication never relies on these ids: it uses the content-derived output
  identity of `DR.output-identity`.

## Consequences

- Two runs of the same script produce byte-identical output.
- Partitioned runs (`ES.keyed`) assign different ids than a single run, so
  tests that compare them compare events modulo ids.
