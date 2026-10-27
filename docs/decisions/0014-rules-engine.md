# 0014. The rules engine

- Status: accepted
- Date: 2026-10-27
- Spec: §14, §11.4 (PT.accumulate, PT.flush, PT.window-close, PT.silence, PT.recovery, PT.schedule), §16.7

## Decision

- **Instances.** The rules node is keyed by entity. A key's instances are
  created when the key is activated (an entity activation signal, or its
  first contribution) and reconciled after every rule push. An instance is
  identified by (rule id, key, binding key); a template's own instance has
  the binding key `""`.
- **Pending contributions.** Every instance keeps the contributions it
  watched since its previous consuming output; an output's samples are their
  multiplicity sum and its sources their (sub-stream, kind) pairs. This
  matches R3: "a trigger's samples are all contributions since the previous
  trigger".
- **Rate limit.** With `(Δ defer)` an output inside the cap becomes an
  intent held until the cap ends; at release the held intents become one
  output with the latest reason, the hull of their windows, and their
  windows summed. An accumulate intent recomputes its window and samples
  from the pending contributions at release, so data that arrives during the
  cap rides in the output (R1). In mode drop the output is discarded and
  counted.
- **Accumulate** is ready when the pending multiplicity reaches N and their
  span reaches S. **Flush** (`max_delay`) fires on the processing clock Δ
  after the first pending contribution arrived; with retain it consumes
  nothing and its clock restarts at the flush.
- **Window close** anchors a tumbling grid at the instance's first
  contribution and closes every grid window whose end the key's
  data-driven watermark (all or any of the watched kinds; −∞ for a watched
  kind never seen; the kinds seen so far when the template watches all)
  has passed, coalesced into one output.
- **Silence** fires once the processing clock reaches the newest sample time
  plus Δ, or the activation instant plus Δ before any data (ACT); its window
  starts at that sample time, or `none`. Recovery reports the first
  contribution after a silence, its window from the silence's start to the
  contribution's end. Only transitions are reported.
- **Schedule** starts its first period at the previous tick before the
  instance was created and coalesces ticks that came due together.
- **Edits.** A job-only edit keeps all progress; an accumulate instance then
  re-judges: one `config` output covering the newest contribution of its
  watched kinds, with samples 0, consuming nothing, subject to the rate
  limit, and dropped if a data output is released first (the data output
  carries the new params). A new binding on a key that already has data
  re-judges too. A semantic edit restarts progress, or under `continue`
  carries it when the condition is the same kind (with the same grid, or
  the same cron expression) and evaluates at once; under `continue` the
  rate-limit state always carries.
- **Outputs** are `RuleOutput` events: rule, key, binding key, target,
  reason, window, samples, windows, sources, params, emission time, a
  summary, and an identity from `output-identity` (with the emission time
  for `config`).
- **Sources.** Rule pushes are configuration control steps. Templates
  declared with `rule-template` form the module's templates source, pushed at
  start. Reports (refused ownership, dangling or invalid items, duplicate
  instances) are recorded once, when they first appear.
