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
| M5 | Time and progress | §6 | done (TM.checkpoint and TM.replay came with M15) |
| M6 | Windows and contexts | §7 | done |
| M7 | Aggregation | §8 | done |
| M8 | Joins | §9 | done |
| M9 | Allen relations | §10 | done |
| M10 | Pattern AST and reference oracle | §11 | done (PT.flush follows with M12) |
| M11 | Incremental pattern engine, deadline scheduler, differential tests | §11, §17 | done |
| M12 | Selection, consumption, output, skip, and evaluation policies | §12 | done |
| M13 | Derivation | §13 | done |
| M14 | Rules as data | §14 | done |
| M15 | Checkpoint, restore, and replay | §6 | done |
| M16 | Acceptance: card testing | §15 | done |
| M17 | Acceptance: Reflex | §16 | done |
| M18 | Examples, Scribble docs, README, full coverage | all | done |
| M19 | Release v0.1.0 | all | done |

## Next step

Version 0.1.0 is released (annotated tag `v0.1.0` and its GitHub
release). Further work starts from `docs/decisions/` and the known
limitations: the engine keeps its buffers for a pattern's whole bound
rather than pruning them by the watermark, deferral of interval events
with long spans is conservative, windows other than tumbling and sliding
retract a whole window when a retracted event was inside, and the runner
keeps the whole input log for replay.

Notes:
- Hosted CI runs on every push (GitHub Actions, `.github/workflows/ci.yml`)
  and is green on main; check it with `gh run list --limit 3` after
  each milestone push. Every commit is also verified locally first (the
  commit helper stashes unstaged work and runs `raco make -v main.rkt` and
  `raco test .` on the staged tree; `scripts/ci-local.sh` mirrors CI).
- Stage handlers register in `syntax/stages.rkt` with `register-stage!`.

## Spec IDs

Each ID is checked once it is implemented, exposed in the surface language,
tested with a `spec-test`, and documented in `docs/coverage.md`. All 175
are; `docs/.coverage-complete` keeps the strict coverage check on.

### ES (§2)

- [x] `ES.time`
- [x] `ES.type`
- [x] `ES.event`
- [x] `ES.batch`
- [x] `ES.reference`
- [x] `ES.composite`
- [x] `ES.space`
- [x] `ES.precedes`
- [x] `ES.membership`
- [x] `ES.causality`
- [x] `ES.stream`
- [x] `ES.history`
- [x] `ES.keyed`
- [x] `ES.substream`
- [x] `ES.operator`

### OP (§4)

- [x] `OP.filter`
- [x] `OP.project`
- [x] `OP.map`
- [x] `OP.enrich`
- [x] `OP.split`
- [x] `OP.route`

### ST (§5)

- [x] `ST.monotonic-merge`
- [x] `ST.merge`
- [x] `ST.key-by`
- [x] `ST.dedupe`
- [x] `ST.reorder`
- [x] `ST.throttle`

### TM (§6)

- [x] `TM.clock`
- [x] `TM.lateness`
- [x] `TM.keyed-watermark`
- [x] `TM.event-time`
- [x] `TM.processing-time`
- [x] `TM.watermark`
- [x] `TM.idle`
- [x] `TM.skew-guard`
- [x] `TM.timer`
- [x] `TM.schedule`
- [x] `TM.retraction`
- [x] `TM.checkpoint`
- [x] `TM.replay`

### WN (§7)

- [x] `WN.tumbling`
- [x] `WN.sliding`
- [x] `WN.session`
- [x] `WN.count`
- [x] `WN.landmark`
- [x] `WN.punctuated`
- [x] `WN.since-last`
- [x] `WN.calendar`
- [x] `WN.empty`

### CX (§7)

- [x] `CX.temporal`
- [x] `CX.segmentation`
- [x] `CX.spatial`
- [x] `CX.state`
- [x] `CX.entity`
- [x] `CX.activation`
- [x] `CX.composite`

### AG (§8)

- [x] `AG.count`
- [x] `AG.multiplicity`
- [x] `AG.events`
- [x] `AG.sum`
- [x] `AG.min`
- [x] `AG.max`
- [x] `AG.avg`
- [x] `AG.variance`
- [x] `AG.first`
- [x] `AG.last`
- [x] `AG.span`
- [x] `AG.sources`
- [x] `AG.collect`
- [x] `AG.argmax`
- [x] `AG.argmin`
- [x] `AG.median`
- [x] `AG.percentile`
- [x] `AG.count-distinct`
- [x] `AG.mode`
- [x] `AG.top-k`
- [x] `AG.approx`
- [x] `AG.incremental`

### JN (§9)

- [x] `JN.window`
- [x] `JN.interval`
- [x] `JN.table`
- [x] `JN.as-of`
- [x] `JN.outer`

### AL (§10)

- [x] `AL.before`
- [x] `AL.after`
- [x] `AL.meets`
- [x] `AL.met-by`
- [x] `AL.overlaps`
- [x] `AL.overlapped-by`
- [x] `AL.starts`
- [x] `AL.started-by`
- [x] `AL.during`
- [x] `AL.contains`
- [x] `AL.finishes`
- [x] `AL.finished-by`
- [x] `AL.equals`

### PT (§11)

- [x] `PT.sequence`
- [x] `PT.primitive`
- [x] `PT.activation`
- [x] `PT.conjunction`
- [x] `PT.disjunction`
- [x] `PT.negation`
- [x] `PT.absence`
- [x] `PT.span-absence`
- [x] `PT.kleene-plus`
- [x] `PT.kleene-star`
- [x] `PT.repeat`
- [x] `PT.any`
- [x] `PT.within`
- [x] `PT.where`
- [x] `PT.aperiodic`
- [x] `PT.aperiodic-cumulative`
- [x] `PT.periodic`
- [x] `PT.periodic-cumulative`
- [x] `PT.threshold`
- [x] `PT.trend.increasing`
- [x] `PT.trend.decreasing`
- [x] `PT.trend.non-increasing`
- [x] `PT.trend.non-decreasing`
- [x] `PT.trend.stable`
- [x] `PT.trend.mixed`
- [x] `PT.relative`
- [x] `PT.spatial.distance`
- [x] `PT.spatial.moving`
- [x] `PT.accumulate`
- [x] `PT.flush`
- [x] `PT.silence`
- [x] `PT.recovery`
- [x] `PT.window-close`
- [x] `PT.schedule`

### SEL (§12)

- [x] `SEL.next`
- [x] `SEL.strict`
- [x] `SEL.partition`
- [x] `SEL.any`

### CON (§12)

- [x] `CON.cumulative`
- [x] `CON.retain`
- [x] `CON.recent`
- [x] `CON.unrestricted`
- [x] `CON.chronicle`
- [x] `CON.continuous`

### OUT (§12)

- [x] `OUT.rate-limit`
- [x] `OUT.coalesce`
- [x] `OUT.first`
- [x] `OUT.timeout-partial`
- [x] `OUT.edge`
- [x] `OUT.every`
- [x] `OUT.last`
- [x] `OUT.maximal`

### SKIP (§12)

- [x] `SKIP.no-skip`
- [x] `SKIP.to-next`
- [x] `SKIP.past-last`
- [x] `SKIP.to-first`
- [x] `SKIP.to-last`

### EVAL (§12)

- [x] `EVAL.immediate`
- [x] `EVAL.deferred`

### DR (§13)

- [x] `DR.summary`
- [x] `DR.view`
- [x] `DR.output-identity`
- [x] `DR.derive`
- [x] `DR.ordered`
- [x] `DR.closure`
- [x] `DR.cycle-guard`
- [x] `DR.act`
- [x] `DR.retract`

### RL (§14)

- [x] `RL.rejudge`
- [x] `RL.template`
- [x] `RL.binding`
- [x] `RL.fan-out`
- [x] `RL.identity`
- [x] `RL.source`
- [x] `RL.ownership`
- [x] `RL.dangling`
- [x] `RL.semantics`
- [x] `RL.carry`
- [x] `RL.converge`
