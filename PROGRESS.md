# Progress

This file is the resume point for work on occur. Read it, together with
`CLAUDE.md` and `git log --oneline -30`, before continuing.

## Milestones

| # | Milestone | Spec | Status |
|---|---|---|---|
| M1 | Event space | §2 | done (runtime; surface forms follow in M2) |
| M2 | `#lang occur` skeleton and scripted runner | §2, §6 | done |
| M3 | Single-event operations | §4 | done |
| M4 | Stream structure, including high-water-mark merging | §5 | done |
| M5 | Time and progress | §6 | done (TM.checkpoint and TM.replay come with M15) |
| M6 | Windows and contexts | §7 | done |
| M7 | Aggregation | §8 | done |
| M8 | Joins | §9 | done |
| M9 | Allen relations | §10 | done |
| M10 | Pattern AST and reference oracle | §11 | todo |
| M11 | Incremental pattern engine, deadline scheduler, differential tests | §11, §17 | todo |
| M12 | Selection, consumption, output, skip, and evaluation policies | §12 | todo |
| M13 | Derivation | §13 | todo |
| M14 | Rules as data | §14 | todo |
| M15 | Checkpoint, restore, and replay | §6 | todo |
| M16 | Acceptance: card testing | §15 | todo |
| M17 | Acceptance: Reflex | §16 | todo |
| M18 | Examples, Scribble docs, README, full coverage | all | todo |
| M19 | Release v0.1.0 | all | todo |

## Next step

M10: the pattern AST (`private/pattern/ast.rkt`, mirroring §11.2 with ACT)
and the reference oracle (`private/pattern/reference.rkt`, a direct
transcription of §11.3), one operator per commit, then the derived
families of §11.4. From M10 on, `tests/coverage.rkt` enforces that every ID
in docs/coverage.md has a tagged test.

Notes:
- Hosted CI runs are blocked by the account's GitHub billing; every commit
  is verified locally (the commit helper stashes unstaged work and runs
  `raco make -v main.rkt` and `raco test .` on the staged tree).
- Stage handlers register in `syntax/stages.rkt` with `register-stage!`.

## Spec IDs

Each ID is checked once it is implemented, exposed in the surface language,
tested with a `spec-test`, and documented in `docs/coverage.md`.

### ES (§2)

- [ ] `ES.time`
- [ ] `ES.type`
- [ ] `ES.event`
- [ ] `ES.batch`
- [ ] `ES.reference`
- [ ] `ES.composite`
- [ ] `ES.space`
- [ ] `ES.precedes`
- [ ] `ES.membership`
- [ ] `ES.causality`
- [ ] `ES.stream`
- [ ] `ES.history`
- [ ] `ES.keyed`
- [ ] `ES.substream`
- [ ] `ES.operator`

### OP (§4)

- [ ] `OP.filter`
- [ ] `OP.project`
- [ ] `OP.map`
- [ ] `OP.enrich`
- [ ] `OP.split`
- [ ] `OP.route`

### ST (§5)

- [ ] `ST.monotonic-merge`
- [ ] `ST.merge`
- [ ] `ST.key-by`
- [ ] `ST.dedupe`
- [ ] `ST.reorder`
- [ ] `ST.throttle`

### TM (§6)

- [ ] `TM.clock`
- [ ] `TM.lateness`
- [ ] `TM.keyed-watermark`
- [ ] `TM.event-time`
- [ ] `TM.processing-time`
- [ ] `TM.watermark`
- [ ] `TM.idle`
- [ ] `TM.skew-guard`
- [ ] `TM.timer`
- [ ] `TM.schedule`
- [ ] `TM.retraction`
- [ ] `TM.checkpoint`
- [ ] `TM.replay`

### WN (§7)

- [ ] `WN.tumbling`
- [ ] `WN.sliding`
- [ ] `WN.session`
- [ ] `WN.count`
- [ ] `WN.landmark`
- [ ] `WN.punctuated`
- [ ] `WN.since-last`
- [ ] `WN.calendar`
- [ ] `WN.empty`

### CX (§7)

- [ ] `CX.temporal`
- [ ] `CX.segmentation`
- [ ] `CX.spatial`
- [ ] `CX.state`
- [ ] `CX.entity`
- [ ] `CX.activation`
- [ ] `CX.composite`

### AG (§8)

- [ ] `AG.count`
- [ ] `AG.multiplicity`
- [ ] `AG.events`
- [ ] `AG.sum`
- [ ] `AG.min`
- [ ] `AG.max`
- [ ] `AG.avg`
- [ ] `AG.variance`
- [ ] `AG.first`
- [ ] `AG.last`
- [ ] `AG.span`
- [ ] `AG.sources`
- [ ] `AG.collect`
- [ ] `AG.argmax`
- [ ] `AG.argmin`
- [ ] `AG.median`
- [ ] `AG.percentile`
- [ ] `AG.count-distinct`
- [ ] `AG.mode`
- [ ] `AG.top-k`
- [ ] `AG.approx`
- [ ] `AG.incremental`

### JN (§9)

- [ ] `JN.window`
- [ ] `JN.interval`
- [ ] `JN.table`
- [ ] `JN.as-of`
- [ ] `JN.outer`

### AL (§10)

- [ ] `AL.before`
- [ ] `AL.after`
- [ ] `AL.meets`
- [ ] `AL.met-by`
- [ ] `AL.overlaps`
- [ ] `AL.overlapped-by`
- [ ] `AL.starts`
- [ ] `AL.started-by`
- [ ] `AL.during`
- [ ] `AL.contains`
- [ ] `AL.finishes`
- [ ] `AL.finished-by`
- [ ] `AL.equals`

### PT (§11)

- [ ] `PT.sequence`
- [ ] `PT.primitive`
- [ ] `PT.activation`
- [ ] `PT.conjunction`
- [ ] `PT.disjunction`
- [ ] `PT.negation`
- [ ] `PT.absence`
- [ ] `PT.span-absence`
- [ ] `PT.kleene-plus`
- [ ] `PT.kleene-star`
- [ ] `PT.repeat`
- [ ] `PT.any`
- [ ] `PT.within`
- [ ] `PT.where`
- [ ] `PT.aperiodic`
- [ ] `PT.aperiodic-cumulative`
- [ ] `PT.periodic`
- [ ] `PT.periodic-cumulative`
- [ ] `PT.threshold`
- [ ] `PT.trend.increasing`
- [ ] `PT.trend.decreasing`
- [ ] `PT.trend.non-increasing`
- [ ] `PT.trend.non-decreasing`
- [ ] `PT.trend.stable`
- [ ] `PT.trend.mixed`
- [ ] `PT.relative`
- [ ] `PT.spatial.distance`
- [ ] `PT.spatial.moving`
- [ ] `PT.accumulate`
- [ ] `PT.flush`
- [ ] `PT.silence`
- [ ] `PT.recovery`
- [ ] `PT.window-close`
- [ ] `PT.schedule`

### SEL (§12)

- [ ] `SEL.next`
- [ ] `SEL.strict`
- [ ] `SEL.partition`
- [ ] `SEL.any`

### CON (§12)

- [ ] `CON.cumulative`
- [ ] `CON.retain`
- [ ] `CON.recent`
- [ ] `CON.unrestricted`
- [ ] `CON.chronicle`
- [ ] `CON.continuous`

### OUT (§12)

- [ ] `OUT.rate-limit`
- [ ] `OUT.coalesce`
- [ ] `OUT.first`
- [ ] `OUT.timeout-partial`
- [ ] `OUT.edge`
- [ ] `OUT.every`
- [ ] `OUT.last`
- [ ] `OUT.maximal`

### SKIP (§12)

- [ ] `SKIP.no-skip`
- [ ] `SKIP.to-next`
- [ ] `SKIP.past-last`
- [ ] `SKIP.to-first`
- [ ] `SKIP.to-last`

### EVAL (§12)

- [ ] `EVAL.immediate`
- [ ] `EVAL.deferred`

### DR (§13)

- [ ] `DR.summary`
- [ ] `DR.view`
- [ ] `DR.output-identity`
- [ ] `DR.derive`
- [ ] `DR.ordered`
- [ ] `DR.closure`
- [ ] `DR.cycle-guard`
- [ ] `DR.act`
- [ ] `DR.retract`

### RL (§14)

- [ ] `RL.rejudge`
- [ ] `RL.template`
- [ ] `RL.binding`
- [ ] `RL.fan-out`
- [ ] `RL.identity`
- [ ] `RL.source`
- [ ] `RL.ownership`
- [ ] `RL.dangling`
- [ ] `RL.semantics`
- [ ] `RL.carry`
- [ ] `RL.converge`
