# occur: project conventions

occur is a Racket DSL (`#lang occur`) for event processing. Its source of
truth is the specification in `docs/spec.md`. Every concept there has a stable
ID such as `PT.sequence`; every ID must be implemented, exposed in the surface
language, tested, and documented (`docs/coverage.md`).

## Resuming work

Before resuming, always read `PROGRESS.md` and run `git log --oneline -20`
(or `-30`). `PROGRESS.md` is the resume point: it lists the milestones, the
spec IDs that are done, and what comes next.

## Layout

```
info.rkt  main.rkt          main.rkt re-exports the public runtime API
lang/reader.rkt             #lang occur
private/                    runtime library (plain Racket, contracts on every export)
  pattern/                  pattern AST, reference oracle, incremental engine, policies
syntax/                     expander: syntax-parse forms for #lang occur
lib/reflex.rkt              the Reflex vocabulary, written in occur's public forms only
tests/                      rackunit and rackcheck tests, mirroring private/
examples/                   runnable #lang occur programs plus data/
scribblings/occur.scrbl     the manual
docs/                       spec.md, coverage.md, reflex.md, decisions/NNNN-title.md
```

## Commands

```
raco make -v main.rkt        # must pass before every commit
raco test -j 8 .             # must pass before every commit
raco setup --check-pkg-deps --pkgs occur
OCCUR_TRIALS=1000 raco test tests/   # property tests (default trial count is 1000)
```

The package is installed as a link: `raco pkg install --auto --batch --link --name occur`.

## Commit rules

- One logical change per commit; the log reads like a narrated build.
- Every commit is green: `raco make -v main.rkt` and `raco test .` pass.
- Conventional Commits with scopes: `feat(merge): ...`, `test(reflex): ...`,
  `docs(scribble): ...`, `fix(nfa): ...`, `refactor(event): ...`, `ci: ...`,
  `chore(progress): ...`.
- Bodies say why, cite the spec (`Spec: §11.3 PT.negation`), and reference
  the milestone issue (`Refs #N`; the milestone's last commit says `Closes #N`).
- Stage explicit paths only; never `git add -A` or `git add .`.
- Push after every commit. Never force-push, rebase, amend, or squash pushed
  work; never delete branches or tags; never use `--no-verify`.
- No empty, padding, or cosmetic-only commits; no generated artifacts.
- No AI co-author trailers on commits.
- Each milestone ends with: tests green, `docs/coverage.md` and `PROGRESS.md`
  updated, a `CHANGELOG.md` entry, a push, a lightweight tag `mNN-<slug>`,
  and its issue closed.

## Spec and decisions

- `docs/spec.md` is verbatim. Change it only with `docs(spec): ...` errata
  commits, each linked to a decision record.
- Where the spec leaves a choice open, write `docs/decisions/NNNN-title.md`
  (context, decision, consequences) and continue.

## Tests

- Tag tests with spec IDs: `(spec-test 'PT.sequence "description" body ...)`
  from `tests/util.rkt`.
- `tests/coverage.rkt` fails if any ID in `docs/coverage.md` lacks a tagged test.
- `tests/reflex-purity.rkt` fails if `lib/reflex.rkt` or `examples/reflex/`
  requires a `private/` module.
- Property tests use rackcheck; `OCCUR_TRIALS` overrides the trial count.

## Confidentiality

Reflex (spec §16) is an internal system of the repository owner's employer.
Spec §16 is the only description of it. Never read, search, or copy anything
from any other repository or directory, and never add names, infrastructure
details, or behavior beyond what §16 states. The repository stays private.
