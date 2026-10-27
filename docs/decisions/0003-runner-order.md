# 0003. The runner's processing order

- Status: accepted
- Date: 2026-10-26
- Spec: §6 (`TM.processing-time`, `TM.timer`, `TM.watermark`, `TM.schedule`), §13 `DR.ordered`, §17

## Context

The spec requires deterministic acceptance traces that depend on processing
time (§16.7) and a single deadline heap with no polling loop (§17). It does
not say in which order simultaneous things happen.

## Decision

The runner executes the script one step at a time. For a step at
processing time `t`:

1. Every processing-time deadline due at or before `t` fires first, in
   `(time, arming order)` order, each at its own time (`now` is set to the
   deadline). An instance whose deadline is superseded by an earlier one
   wakes once. An early wake-up re-checks and does nothing.
2. `now` becomes `t`, and the step runs.
   - An arrival is delivered to every node reading the source, in
     node-name order. A node's emissions are delivered depth-first: each
     emitted item reaches all of its consumers before the next item is
     delivered.
   - A watermark advance recomputes node watermarks in topological order,
     each node's input watermark being the minimum over its inputs. A node
     fires its due event-time timers before the next node is updated, so
     events emitted at a watermark reach downstream nodes before their own
     watermark moves.
3. Deadlines armed during the step that are already due fire.
4. The step's outputs are delivered grouped by key, in order of each key's
   first output, FIFO within a key (`DR.ordered`).

A suspension `(suspend t1 t2)` fires deadlines up to `t1`, then jumps to
`t2`. Every instance with an overdue deadline wakes once at `t2`, which is
how missed schedule ticks coalesce (`TM.schedule`).

At the end of the script the runner advances every source to the final
watermark `+inf.0` (unless `#:final-watermark? #f`), which completes
deferred evaluation. Processing time does not advance at the end.

## Consequences

- Two runs of the same script produce the same outputs in the same order.
- A deadline and an arrival at the same instant: the deadline fires first.
