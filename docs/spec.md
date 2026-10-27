# Event Space and Event Operations

*A reference specification for event processing engines (stream processing and complex event processing).*

This document serves two purposes. It is a learning reference, and it is precise enough to implement against. Every concept has a stable ID, such as `PT.sequence`, so implementations, tests, and documentation can trace back to the exact definition they realize. Section 16 shows that a real production trigger engine, Reflex, is one program in this vocabulary.

## Contents

1. Intuition
2. The event space
3. Operation families at a glance
4. Single-event operations
5. Stream-structure operations
6. Time, progress, and correctness
7. Windows and contexts
8. Aggregation
9. Joins and correlation
10. Temporal relations (Allen's interval algebra)
11. Pattern operators (event algebra)
12. Pattern semantics: selection, consumption, output
13. Derivation, closure, and action
14. Rules as data
15. Worked example: card testing
16. Case study: Reflex
17. Implementation notes
18. Glossary
19. References

---

## 1. Intuition

An **event** is a record that something happened at some time. Examples include a card swipe, a sensor crossing a threshold, or a batch of readings landing in a database.

An **event processing engine** watches streams of these records and does four kinds of things:

- It **cleans and reshapes** individual events.
- It **scopes** the infinite stream into finite pieces (windows and contexts).
- It **summarizes and relates** what is in those pieces (aggregates and joins).
- It **recognizes** meaningful combinations, including the meaningful *absence* of events, and announces them as new, higher-level events.

The **event space** is the universe all of this lives in. It contains every event the engine could possibly observe or produce: every type of happening, every possible attribute value, and every possible time of occurrence. A stream is the small slice of that universe that actually happens.

Real engines add one more layer. The *rules* that say which operators run, for which entities, and with which parameters are themselves data that changes at runtime (§14).

---

## 2. The event space

### 2.1 Time — `ES.time`

- A **time domain** 𝕋 is a totally ordered set with addition of durations. Typical choices are ℕ (ticks), ℤ, or ℝ≥0 (for example, seconds since the epoch).
- A **duration** Δ is a non-negative element of 𝕋.
- An **occurrence interval** is a pair [tₛ, tₑ] with tₛ ≤ tₑ. A **point** is the degenerate interval tₛ = tₑ.
- There are two clocks: **event time** (when the event happened) and **processing time** (when the engine observed it). See §6. All semantics in this document use event time unless an operator declares another clock (`TM.clock`).

### 2.2 Event types and schemas — `ES.type`

- T is a finite set of **event type** names.
- Each type τ has a **schema** ⟨a₁ : D₁, …, aₖ : Dₖ⟩. The attribute names must be distinct. Each value domain Dᵢ is one of Integer, Real, String, Boolean, Time, Duration, Location, or a nested record or list.
- The **payload domain** of τ is D_τ = D₁ × … × Dₖ. A type may have no attributes (k = 0); a pure signal such as `Heartbeat` is an example.
- Every type is either **primitive**, meaning it is produced by sources outside the engine, or **derived**, meaning it is produced by an operator.

### 2.3 Event instances — `ES.event`

An event instance is a tuple

```
e = ⟨id, τ, v, tₛ, tₑ, n, src, C⟩
```

| Field | Meaning |
|---|---|
| `id` | Globally unique identifier |
| `τ` | Type, τ ∈ T |
| `v` | Payload, v ∈ D_τ; `e.a` reads attribute `a` |
| `tₛ, tₑ` | Occurrence interval in event time, tₛ ≤ tₑ |
| `n` | Multiplicity: how many underlying occurrences the event stands for (1 for an ordinary event; see `ES.batch`) |
| `src` | Producer: a source name, or the operator that derived the event |
| `C` | Constituents: the events this one was derived from (∅ for primitive events) |

Notation: `type(e)`, `ts(e)`, `te(e)`, `n(e)`. For point events, `t(e) = ts(e) = te(e)`. An event's **arrival time** `arr(e)` is the processing time at which the engine observed it; it is not part of the event.

### 2.4 Batch events — `ES.batch`

A **batch event** stands for n(e) ≥ 0 underlying samples written together and spread evenly over [ts(e), te(e)]. A sensor uploading a chunk of 50 readings is one batch event with n = 50.

The **share** of e lying strictly past a boundary b is:

```
share(e, b) = 0                                                   if n(e) = 0 or te(e) ≤ b
            = n(e)                                                if ts(e) > b
            = min(n(e), max(1, round(n(e) · (te(e) − b) / (te(e) − ts(e)))))   otherwise
```

`round` is round-half-to-even. The share's interval is [max(ts(e), b), te(e)]. This share is what high-water-mark merging (`ST.monotonic-merge`) counts.

### 2.5 Reference payloads — `ES.reference`

An event may carry a *pointer* to data stored elsewhere instead of the data itself. Such a pointer names a sub-stream and an interval, and may include a small summary such as the newest value. A consumer that needs the values reads the store over the event's interval. Operators can still count, window, and time such events, because those depend only on (ts, te, n) and the summary.

### 2.6 Composite events — `ES.composite`

A **composite event** is a derived event c with a non-empty set of constituents C. Unless an operator specifies otherwise:

```
ts(c) = min { ts(x) : x ∈ C }
te(c) = max { te(x) : x ∈ C }
n(c)  = 1
```

There are three documented exceptions:
- Absence and timeout detections end at their deadline (§11.3).
- Window aggregates take the window's bounds (§8).
- Summarized outputs carry a summary instead of their constituents (`DR.summary`).

### 2.7 Definition of the event space — `ES.space`

The event space is defined inductively. Provenance is what makes it inductive: a derived event refers to the events it came from.

```
𝓔⁰    = { ⟨id, τ, v, ts, te, n, src, ∅⟩ : τ primitive, v ∈ D_τ, ts ≤ te, n ∈ ℕ }
𝓔ⁿ⁺¹  = 𝓔ⁿ ∪ { ⟨id, τ, v, ts, te, n, src, C⟩ : τ derived, v ∈ D_τ, ts ≤ te,
                                                C a finite non-empty subset of 𝓔ⁿ }
𝓔     = ⋃ₙ 𝓔ⁿ
```

- 𝓔 is the disjoint union 𝓔_P ⊎ 𝓔_D of primitive and derived events.
- 𝓔_τ denotes the restriction of 𝓔 to type τ.
- The **abstraction level** of an event is the smallest n with e ∈ 𝓔ⁿ.

If provenance is ignored, the space has the compact form

```
𝓔 ≅ ⋃_{τ ∈ T} {τ} × D_τ × { (ts, te) ∈ 𝕋² : ts ≤ te } × ℕ
```

The event space contains every *possible* event. Any actual run observes only a tiny subset of it.

### 2.8 Relations on the event space

| ID | Relation | Definition | Properties |
|---|---|---|---|
| `ES.precedes` | a ≺ b | te(a) < ts(b) | A strict partial order. If neither a ≺ b nor b ≺ a, the events are concurrent or overlapping (a ∥ b). |
| `ES.membership` | a ∈ C(c) | a is a constituent of c | Its transitive closure ∈* is the event hierarchy. |
| `ES.causality` | a → b | a caused b, either recorded (for example `b.cause = a.id`) or inferred | A strict partial order. a → b implies ts(a) ≤ ts(b). Temporal order does **not** imply causality. |

The structure (observed events, ≺, →, ∈) is what Luckham calls the **event cloud**. It is a partially ordered set, not a sequence.

### 2.9 Streams, histories, keys, and sub-streams

- **`ES.stream`**: A stream S is a possibly infinite sequence of events in **arrival order**, with a declared set of types T_S ⊆ T. Arrival order may disagree with event-time order; such events are out of order.
- **`ES.history`**: The history of S at event time t is H_S(t) = { e ∈ S : te(e) ≤ t }. It contains every event that has completely occurred by t. Whether those events have *arrived* yet is a separate question, answered by watermarks (§6).
- **`ES.keyed`**: A key function κ : 𝓔 → K partitions S into substreams S_k = { e ∈ S : κ(e) = k }. Keyed operators run independently for each key. If no operator reads another key's state, the program **scales by partition**: N engines that each own a slice of K emit exactly what one engine would.
- **`ES.substream`**: Keys can be hierarchical. An *entity* key k (a device, an account) owns *sub-streams* s (its individual sensors or channels), and each sub-stream has a *kind* m(s), such as a measurement type. An operator keyed by k may keep per-(k, s) state and may watch only the sub-streams of certain kinds M.

### 2.10 Operators, closure, and monotonicity — `ES.operator`

An operator with n inputs maps input histories to output events:

```
op : H₁(t) × … × Hₙ(t) → O(t) ⊆ 𝓔
```

**Closure.** Outputs are themselves events in 𝓔, so the output of any operator can be the input of any other operator, including a pattern. Event hierarchies arise from this property.

**Monotonicity.** An operator is *monotone* if adding events to its input never invalidates an output it has already produced.

- Filters, maps, sequences, and inner joins are monotone.
- Negation, absence, outer joins, window aggregates, and maximal-match output are **not** monotone. A late-arriving event can invalidate their results. Such operators may emit only once the engine knows no more relevant events will arrive, which is what watermarks provide (§6). The alternative is to emit speculatively and retract later.

### 2.11 Boundaries and counterexamples

- **State versus event.** "Machine is ON" is a state. "Machine switched ON at 10:02" is an event. Engines detect changes; a state is what you get by folding events over time (see `DR.view`).
- **Samples are events.** In a sensor reporting every 5 seconds, each reading occurrence is an event, or a share of a batch event. "The temperature" itself is not.
- **Query results are not events** until they are emitted into 𝓔.
- **Naming collision with probability theory.** There, an "event space" is a σ-algebra 𝓕 ⊆ 2^Ω, and an event is a *set of outcomes*, not an occurrence. The two notions meet only in probabilistic CEP, which reasons about uncertain events.

---

## 3. Operation families at a glance

| § | Family | Signature | Stateful | Monotone |
|---|---|---|---|---|
| 4 | Single-event | Stream → Stream | No | Yes |
| 5 | Stream structure | Streamⁿ → Stream(s) | Some (dedupe, reorder, merge) | Yes |
| 6 | Time and progress | Stream → Stream + progress | Yes | Not applicable |
| 7 | Windows and contexts | Stream → windowed Stream | Yes | Not applicable |
| 8 | Aggregation | windowed Stream → Stream | Yes | No (emits on window close) |
| 9 | Joins | Stream × (Stream or Table) → Stream | Yes | Inner: yes. Outer: no |
| 10 | Allen relations | 𝓔 × 𝓔 → Bool | No | Predicates |
| 11 | Pattern operators | Stream → Stream of composites | Yes | Not for negation or absence |
| 12 | Pattern semantics | Parameters of §11 | Not applicable | Maximal output: no |
| 13 | Derivation and action | Matches → 𝓔 or side effects | Not applicable | Not applicable |
| 14 | Rules as data | Rule sets → running operator instances | Yes | Not applicable |

---

## 4. Single-event operations

Filter returns the same events it receives. Every other operation in this section produces derived events whose constituent set is C = {e}.

| ID | Operation | Definition | Example |
|---|---|---|---|
| `OP.filter` | Filter σ_p | Keep e if and only if p(e) holds | `amount > 1000` |
| `OP.project` | Project π_A | Restrict the payload to attributes A ⊆ schema(τ), giving type τ restricted to A | Keep only `card` and `amount` |
| `OP.map` | Translate / map μ_f | e ↦ f(e), possibly with a new type; timestamps are set explicitly or inherited | °F → °C; rename fields |
| `OP.enrich` | Enrich | e ↦ e ⊕ R(k(e)), where R is a reference lookup keyed by k(e) | Add the merchant category |
| `OP.split` | Split | e ↦ {e₁, …, eₙ} = f(e); each child gets a fresh id | A write covering five sensors becomes one batch event per sensor |
| `OP.route` | Route | Given predicates p₁, …, pₙ, produce streams Sᵢ = σ_{pᵢ}(S); in exclusive mode the first matching predicate wins, in inclusive mode all do | Send alerts by severity |

---

## 5. Stream-structure operations

| ID | Operation | Definition | Example |
|---|---|---|---|
| `ST.merge` | Merge / union | S₁ ∪ S₂, interleaved by arrival | Combine two sensor feeds |
| `ST.key-by` | Key-by | Partition by κ; downstream operators run per key. Re-keying by a different attribute (for example the entity that owns a reading rather than the device that sent it) is the same operation. | Per card; per owning device |
| `ST.dedupe` | Deduplicate | Drop e if an earlier event has the same dedup key within horizon h | Handle at-least-once delivery |
| `ST.monotonic-merge` | High-water-mark merge | Per sub-stream, count only what lies past the newest time already seen (below) | Idempotent ingestion of replayed batches |
| `ST.reorder` | Reorder | Buffer events and release them in event-time order, up to the watermark | Run before order-sensitive patterns |
| `ST.throttle` | Throttle / sample | Emit at most r events per Δ, or every n-th event, or each event with probability p; excess events are dropped (compare `OUT.rate-limit`) | Downsample high-rate telemetry |

**`ST.monotonic-merge` in detail.** Each sub-stream s keeps a **high-water mark** hw(s), which starts at −∞, plus counters. For each arrival e on s:

```
new(e) = share(e, hw(s))                       (ES.batch)
if new(e) > 0:
    emit a contribution c with ts(c) = max(ts(e), hw(s)), te(c) = te(e), n(c) = new(e)
    hw(s) := te(e)
late(s) += n(e) − new(e)
```

Downstream operators consume **contributions**, not raw arrivals. Three properties follow:

- Re-applying an event that was already applied is a no-op, so duplicates and replays converge on the same state.
- Data wholly behind hw(s) is dropped and counted, which is a lateness policy (`TM.lateness`).
- hw(s) is a per-sub-stream watermark (`TM.keyed-watermark`).

---

## 6. Time, progress, and correctness

| ID | Concept | Definition |
|---|---|---|
| `TM.event-time` | Event time | The timestamps carried in the event. All semantics default to event time. |
| `TM.processing-time` | Processing time | The engine's wall clock at observation. Results based on it can differ between runs. |
| `TM.clock` | Operator clock | Every timed operator declares its clock: event time (driven by a watermark), processing time (driven by the wall clock), or **hybrid**. A hybrid deadline is anchored at an event-time instant and compared against processing time, for example "the newest sample time plus 10 minutes, compared with now". |
| `TM.watermark` | Watermark W | A monotone non-decreasing assertion: "no future arrival will have te < W". |
| `TM.keyed-watermark` | Keyed, data-driven watermark | A watermark per key derived from the data itself. With high-water marks hw_k(m) per kind m of key k (−∞ for a kind never seen) and watched kinds M: W_k^any = max_{m∈M} hw_k(m); W_k^all = min_{m∈M} hw_k(m). "All" waits for the slowest kind. |
| `TM.idle` | Idleness | Event time advances only when data arrives. An operator that must notice that *nothing* arrives cannot rely on an event-time watermark. It needs a processing-time or hybrid timer, or a source that emits idle watermarks or heartbeats. |
| `TM.skew-guard` | Future-skew guard | Drop and count any event with te(e) > now + B for a bound B. Under high-water-mark merging, one far-future timestamp would otherwise make every later real event late. |
| `TM.lateness` | Lateness policy | Applies to an event that arrives with te(e) < W. The engine can drop it (and count it), send it to a side output, or accept it within an allowed lateness L and update earlier results. |
| `TM.timer` | Timers | Callbacks at an event time t, which fire when W ≥ t, or at a processing time t. |
| `TM.schedule` | Calendar schedule | Processing-time timers at every tick of a calendar expression, for example a five-field cron expression read in UTC. Ticks that came due while the engine was suspended fire once, together (`OUT.coalesce`). |
| `TM.retraction` | Retraction | Emitting ⊖e cancels a previously emitted e. A correction is ⊖e_old followed by e_new. |
| `TM.checkpoint` | Checkpoint and restore | A snapshot of every operator's state, the read position in every source, and the outputs produced but not yet delivered. Restoring it resumes processing exactly after the snapshot. Combined with `ST.monotonic-merge` and `DR.output-identity`, outputs repeat only within the last snapshot interval, and downstream deduplication removes those repeats. |
| `TM.replay` | Replay with held-back output | Without a usable checkpoint, the engine rebuilds state by re-reading the retained input. Outputs produced during the replay are held back and coalesced to one per operator instance (`OUT.coalesce`). They are released only once every source has caught up and the configuration is loaded (§14), and each is judged against the instances then in force; an output whose instance is gone is dropped and counted. Processing-time timers stay suspended until the replay has caught up, so that replayed arrivals are not mistaken for silence or for flush deadlines. |

An event-time window [s, e) is **complete** when W ≥ e. A *perfect* watermark guarantees that no event will be late. A *heuristic* watermark is an estimate, and lateness policies handle the events it misses. A data-driven keyed watermark is heuristic: it assumes each sub-stream arrives in order, which `ST.monotonic-merge` enforces by dropping what does not.

---

## 7. Windows and contexts

A window assignment ω : 𝓔 → 2^Windows maps each event to the windows it belongs to. Windows are half-open intervals [s, e). Interval events are assigned by ts(e) by default; assignment by te(e) or by overlap may be configured.

| ID | Window | Assignment | Example |
|---|---|---|---|
| `WN.tumbling` | Tumbling(Δ, offset o) | ω(e) = { [o + kΔ, o + (k+1)Δ) } with k = ⌊(t(e) − o) / Δ⌋. The grid is aligned to the epoch plus o. The first window that exists is the one containing the first event; earlier windows are never created. | Per-minute counts |
| `WN.sliding` | Sliding / hopping(Δ, slide σ) | ω(e) = { [kσ, kσ + Δ) : kσ ≤ t(e) < kσ + Δ }; each event is in at most ⌈Δ/σ⌉ windows | 10-minute average, updated every minute |
| `WN.session` | Session(gap g) | Per key, maximal runs of events whose consecutive gaps are < g; the window is [first, last + g). An arriving event may merge two sessions. | User activity sessions |
| `WN.count` | Count(n, step m) | The last n events, advancing every m events | Moving average of the last 20 readings |
| `WN.landmark` | Landmark(t₀) | [t₀, now), growing forever | Totals since midnight |
| `WN.punctuated` | Punctuated / data-driven | Boundaries are marked by events: opened by A, closed by B or by a timeout | Between shift-start and shift-end |
| `WN.since-last` | Since last output | Per operator instance, the window opens at the first contribution after the instance's previous consuming output, and closes when the instance next fires. The operator's own output is the punctuation. | "Enough new data since the last trigger" |
| `WN.calendar` | Calendar | Windows between consecutive ticks of a calendar schedule, [tickᵢ₋₁, tickᵢ), in processing time (`TM.schedule`) or in event time. A window started mid-period ends at the period's next tick. | Hourly, or "every weekday at 06:00 UTC" |

**Empty windows — `WN.empty`.** By default, a window with no events emits nothing. A window may be declared to fire even when empty, which calendar windows usually are: they report "nothing arrived in this period".

**Firing.** An event-time window [s, e) emits when W ≥ e. With an allowed lateness L, it may emit updated results until W ≥ e + L, after which its state is dropped. Under a keyed watermark, windows close per key. A processing-time window emits when the wall clock reaches e.

**Contexts** (from Etzion and Niblett) generalize windows:

| ID | Context | Meaning |
|---|---|---|
| `CX.temporal` | Temporal | A fixed interval, an event interval (opened on A, closed on B or a timeout), a sliding fixed interval, or a sliding event interval |
| `CX.segmentation` | Segmentation | Partition by attribute values; key-by is a special case |
| `CX.spatial` | Spatial | A fixed location, within distance r of an entity, or within distance r of an event |
| `CX.state` | State-oriented | The operator is active only while an external state holds, for example "during maintenance" |
| `CX.entity` | Entity lifecycle | The set of keys comes from a reference *directory*, not only from the data. An entity the directory lists as active is **activated** at once, before it sends anything. An entity marked **inactive** (deprecated) has its state dropped, and its later events are ignored and counted. An entity **removed** from the directory has its state dropped; if it sends data again, it is activated afresh. The directory can be replaced whole or patched per entity. |
| `CX.activation` | Activation instant | The moment a context instance begins: an entity's activation, or an operator instance's creation. It can serve as an initiator with no event (`ACT`, §11). |
| `CX.composite` | Composite | An intersection of contexts, for example per customer × per hour |

---

## 8. Aggregation

Aggregates fall into three classes, following Gray et al.:

| Class | Property | Examples |
|---|---|---|
| Distributive | F(X ⊎ Y) = G(F(X), F(Y)) | count, sum, min, max |
| Algebraic | Computable from a fixed-size tuple of distributive aggregates | avg = sum / count; variance from (n, Σx, Σx²) |
| Holistic | No constant-size partial state exists | median, percentile, exact distinct count, mode, top-k |

| ID | Aggregate | Class |
|---|---|---|
| `AG.count` | count. Over batch events this is the **multiplicity sum** Σ n(e) (`AG.multiplicity`); the number of events is `AG.events` | Distributive |
| `AG.sum` | sum | Distributive |
| `AG.min` / `AG.max` | min / max | Distributive |
| `AG.avg` | average | Algebraic |
| `AG.variance` | variance and standard deviation | Algebraic |
| `AG.first` / `AG.last` | first / last by event time | Distributive (with timestamps) |
| `AG.span` | max te − min ts over the events | Algebraic (from min and max) |
| `AG.sources` | the set of contributing sub-streams and kinds | Distributive (set union) |
| `AG.collect` | list of values | Holistic |
| `AG.argmax` / `AG.argmin` | event with the max / min value | Distributive |
| `AG.median` | median | Holistic |
| `AG.percentile` | p-th percentile | Holistic |
| `AG.count-distinct` | number of distinct values | Holistic |
| `AG.mode` | most frequent value | Holistic |
| `AG.top-k` | k largest or most frequent | Holistic |
| `AG.approx` | sketches: HyperLogLog (distinct), t-digest (quantiles), Count-Min (frequencies) | Approximate, constant size |

**Incremental maintenance — `AG.incremental`.** An aggregate keeps a partial state with a combine operation. Invertible aggregates (count, sum, Σx²) also support *remove*, which makes sliding windows cheap. Min and max are not invertible; they need a monotonic deque or a two-stack queue. Holistic aggregates must keep all values or use a sketch.

**Output.** Each (key, window) pair produces one event of a derived aggregate type, with ts = s and te = e. For count and since-last windows, ts and te are the minimum and maximum over the contents. The constituents C are the window contents; implementations may elide them or replace them with a summary (`DR.summary`).

---

## 9. Joins and correlation

| ID | Join | Definition |
|---|---|---|
| `JN.window` | Windowed join | s ⋈ r if θ(s, r) holds and both events are in the same window |
| `JN.interval` | Interval join | s ⋈ r if θ(s, r) holds and t(r) ∈ [t(s) − Δ₁, t(s) + Δ₂] |
| `JN.table` | Stream–table (lookup) | Each s is matched against the table's current state. Enriching outputs with an entity directory's labels is a lookup join. |
| `JN.as-of` | Temporal / as-of | Each s is matched with the version of r that was valid at t(s): for that key, the r with the greatest valid_from ≤ t(s) |
| `JN.outer` | Outer variants | Left, right, or full. An unmatched s is emitted with nulls once its join window closes. This needs a watermark and is not monotone. |

The output is a derived event with C = {s, r}, or C = {s} for an unmatched outer result.

---

## 10. Temporal relations (Allen's interval algebra)

For proper intervals a = [a⁻, a⁺] and b = [b⁻, b⁺] with a⁻ < a⁺ and b⁻ < b⁺, **exactly one** of the following 13 relations holds.

| ID | Relation | Condition | Inverse (ID) |
|---|---|---|---|
| `AL.before` | a before b | a⁺ < b⁻ | after (`AL.after`) |
| `AL.meets` | a meets b | a⁺ = b⁻ | met-by (`AL.met-by`) |
| `AL.overlaps` | a overlaps b | a⁻ < b⁻ < a⁺ < b⁺ | overlapped-by (`AL.overlapped-by`) |
| `AL.starts` | a starts b | a⁻ = b⁻ and a⁺ < b⁺ | started-by (`AL.started-by`) |
| `AL.during` | a during b | b⁻ < a⁻ and a⁺ < b⁺ | contains (`AL.contains`) |
| `AL.finishes` | a finishes b | a⁺ = b⁺ and b⁻ < a⁻ | finished-by (`AL.finished-by`) |
| `AL.equals` | a equals b | a⁻ = b⁻ and a⁺ = b⁺ | equals (self-inverse) |

**Points.** Between two points, only before, equals, and after are distinguishable. Between a point p and an interval b there are five relations:

- p before b (p < b⁻)
- p starts b (p = b⁻)
- p during b (b⁻ < p < b⁺)
- p finishes b (p = b⁺)
- p after b (p > b⁺)

---

## 11. Pattern operators (event algebra)

### 11.1 Matches

The semantics map a pattern P and a history H to a set of matches ⟦P⟧(H).

A **match** m is a finite set of events from H together with **bindings**. A binding maps a name either to an event or, for Kleene closure, to a time-ordered list of events. Its interval is ts(m) = min ts and te(m) = max te over its events.

The definitions below are the **most permissive** semantics: every qualifying combination of events counts. This is equivalent to skip-till-any-match with no consumption. The parameters in §12 restrict which of these matches an engine reports.

### 11.2 Grammar

```
P ::= x:τ[p]                  primitive (binding x and predicate p are optional)
    | ACT                     the activation of the current context (CX.activation); matches no event
    | P ; P                   sequence
    | P ∧ P                   conjunction
    | P ∨ P                   disjunction
    | P ; ¬Q ; P              negation between
    | P ; ¬Q within Δ         absence / timeout
    | P unless Q              span absence
    | P+  |  P*  |  P{n,m}    Kleene plus, Kleene star, bounded repetition
    | ANY(k; P₁, …, Pₙ)       k-of-n
    | P within Δ              time bound
    | P where θ               cross-event predicate over bindings
    | APER(P₁, P₂, P₃)        aperiodic (Snoop A)
    | APER*(P₁, P₂, P₃)       cumulative aperiodic (Snoop A*)
    | PER(P₁, Δ, P₃)          periodic (Snoop P)
    | PER*(P₁, Δ, P₃)         cumulative periodic (Snoop P*)
```

To restrict a pattern to a single key, apply key-by (`ST.key-by`) to the input stream first. This is equivalent to a `where` clause requiring all constituents to share κ. Any pattern with a time bound may declare its clock (`TM.clock`).

### 11.3 Semantics

**`PT.primitive`**
```
⟦x:τ[p]⟧ = { {e} : e ∈ H, type(e) = τ, p(e) }          binds x ↦ e
```

**`PT.activation`**
```
⟦ACT⟧ = { ∅ }   with ts = te = the activation instant of the current context
```
`ACT` lets an absence count from when watching began, so that an entity that never sends anything still goes silent.

**`PT.sequence`**
```
⟦P₁ ; P₂⟧ = { m₁ ∪ m₂ : m₁ ∈ ⟦P₁⟧, m₂ ∈ ⟦P₂⟧, te(m₁) < ts(m₂) }
```
Events with equal timestamps are not in sequence. Implementations that allow ties must declare a tiebreak rule.

**`PT.conjunction`**
```
⟦P₁ ∧ P₂⟧ = { m₁ ∪ m₂ : m₁ ∈ ⟦P₁⟧, m₂ ∈ ⟦P₂⟧, m₁ ∩ m₂ = ∅ }        any order
```

**`PT.disjunction`**
```
⟦P₁ ∨ P₂⟧ = ⟦P₁⟧ ∪ ⟦P₂⟧
```
The bindings of the side that did not match are absent.

**`PT.negation`** (negation between)
```
⟦P₁ ; ¬Q ; P₂⟧ = { m₁ ∪ m₂ : m₁ ∈ ⟦P₁⟧, m₂ ∈ ⟦P₂⟧, te(m₁) < ts(m₂),
                             no q ∈ ⟦Q⟧ with te(m₁) < ts(q) and te(q) < ts(m₂) }
```
The forbidden region is only the open gap between the two neighbors. When the right-hand side is a Kleene closure, that gap ends at its *first* repetition. To forbid Q anywhere in the whole match, use span absence.

**`PT.absence`** (absence / timeout)
```
⟦P ; ¬Q within Δ⟧ = { m ∈ ⟦P⟧ : no q ∈ ⟦Q⟧ with te(m) < ts(q) ≤ te(m) + Δ }
```
The detection's end time is te(m) + Δ, which is the deadline and not the maximum over constituents. On the event-time clock it can be emitted once W > te(m) + Δ. On a hybrid clock it is emitted when now ≥ te(m) + Δ; that is the only way to detect a *total* silence (`TM.idle`). P may be `ACT`.

**`PT.span-absence`**
```
⟦P unless Q⟧ = { m ∈ ⟦P⟧ : no q ∈ ⟦Q⟧, q ∉ m, with ts(m) < ts(q) and te(q) < te(m) }
```

**`PT.kleene-plus`, `PT.kleene-star`, `PT.repeat`**
```
⟦P+⟧     = ⋃_{k ≥ 1} ⟦P ; P ; … ; P⟧   (k copies; each copy's binding is appended to a list)
⟦P*⟧     = ⟦P+⟧ ∪ {∅}
⟦P{n,m}⟧ = as P+, but with n ≤ k ≤ m   (m may be ∞)
```
The empty match is meaningful only inside a sequence: P₁ ; P* ; P₂ includes the case P₁ ; P₂.

**`PT.any`** (k-of-n)
```
⟦ANY(k; P₁, …, Pₙ)⟧ = { m_{i₁} ∪ … ∪ m_{iₖ} : i₁ < … < iₖ, m_{iⱼ} ∈ ⟦P_{iⱼ}⟧, pairwise disjoint }
```
ANY(n; …) is the conjunction of all the patterns, and ANY(1; …) is their disjunction.

**`PT.within`**
```
⟦P within Δ⟧ = { m ∈ ⟦P⟧ : te(m) − ts(m) ≤ Δ }
```

**`PT.where`**
```
⟦P where θ⟧ = { m ∈ ⟦P⟧ : θ(bindings(m)) }
```
θ may compare attributes across bindings (`a.card = b.card`), use Allen relations (§10), or aggregate over Kleene lists (`sum(b.amount) > 1000`, `count(b) ≥ 3`, `span(b) ≥ 60`).

**`PT.aperiodic`, `PT.aperiodic-cumulative`.** An interval is opened by e₁ ∈ ⟦P₁⟧ and closed by the first subsequent e₃ ∈ ⟦P₃⟧. If no such e₃ ever occurs, the interval stays open.
- APER: every e₂ ∈ ⟦P₂⟧ strictly inside an open interval yields the match {e₁, e₂}.
- APER*: when e₃ closes the interval, a single match is produced: {e₁, e₃} ∪ { every e₂ inside the interval }.

**`PT.periodic`, `PT.periodic-cumulative`.**
- PER(P₁, Δ, P₃): after e₁, a tick occurs at te(e₁) + jΔ for j = 1, 2, …, as long as that time is earlier than ts(e₃) of the closing e₃. Each tick is the match {e₁}, with te equal to the tick time.
- PER*: a single match when e₃ closes the interval, carrying the list of tick times.

### 11.4 Derived pattern families

These families can all be expressed with the operators above, windows, aggregation, and `where` clauses. Engines usually offer them as named forms. The first group comes from Etzion and Niblett's catalog; the second group comes from data-arrival trigger engines (§16).

| ID | Pattern | Condition | Example |
|---|---|---|---|
| `PT.threshold` | Threshold | count, sum, avg, min, or max of the matched or windowed events crosses k | More than 100 errors per minute |
| `PT.trend.increasing` | Increasing | for all i, bᵢ.x < bᵢ₊₁.x over a Kleene binding b | Rising vibration |
| `PT.trend.decreasing` | Decreasing | for all i, bᵢ.x > bᵢ₊₁.x | Falling pressure |
| `PT.trend.non-increasing` | Non-increasing | for all i, bᵢ.x ≥ bᵢ₊₁.x | |
| `PT.trend.non-decreasing` | Non-decreasing | for all i, bᵢ.x ≤ bᵢ₊₁.x | |
| `PT.trend.stable` | Stable | max(b.x) − min(b.x) ≤ ε | Steady temperature |
| `PT.trend.mixed` | Mixed | none of the above holds | |
| `PT.relative` | Relative min / max | a binding is the minimum or maximum among the matched events | Peak reading in an episode |
| `PT.spatial.distance` | Distance | distance(loc(e), anchor) is within or beyond r (with min, max, and avg variants) | Vehicle enters a geofence |
| `PT.spatial.moving` | Movement | moving toward, moving away, or moving in a constant direction, judged from successive locations | Truck approaching a depot |
| `PT.accumulate` | Accumulate | Over `WN.since-last`: the contributions so far reach count(b) ≥ N (`AG.multiplicity`) and span(b) ≥ S (`AG.span`). The pattern is `(b:X)+ where count(b) ≥ N and span(b) ≥ S`, with `SEL.next`, `OUT.first`, and `CON.cumulative`. | "3 new readings since the last evaluation" |
| `PT.flush` | Timeout flush | An accumulation that is not ready Δ after its first contribution arrived (processing clock) is emitted anyway, as a timed-out partial match (`OUT.timeout-partial`). It consumes its contributions, or with `CON.retain` keeps them counting. | "Evaluate at least every 15 minutes" |
| `PT.silence` | Silence | `(ACT ∨ x:X) ; ¬X within Δ` on the hybrid clock, with `CON.recent` and `OUT.first` per initiator. The initiator is the newest contribution, or the activation if there is none. The pattern fires once when the watched data goes quiet for Δ. | Device disconnected |
| `PT.recovery` | Recovery | `s:Silence ; x:X` with `SEL.next` and `OUT.first`: the first contribution after a silence. Together with `PT.silence` this is edge-triggered output (`OUT.edge`). | Device reconnected |
| `PT.window-close` | Window close | Every window of `WN.tumbling` that closes under the keyed watermark (`TM.keyed-watermark`, any or all), with windows that closed together coalesced into one output (`OUT.coalesce`) | "Run the 15-minute job once all its sensors have covered the window" |
| `PT.schedule` | Schedule | Every `WN.calendar` window in processing time, firing even when empty (`WN.empty`) and reporting what arrived in the period; ticks that came due together are coalesced | Nightly per-device report |

---

## 12. Pattern semantics: selection, consumption, output

### 12.1 Selection strategies

A selection strategy applies to sequences and Kleene closure. It decides which events may be skipped between consecutive steps of a pattern.

| ID | Strategy | Rule | Flink CEP |
|---|---|---|---|
| `SEL.strict` | Strict contiguity | Consecutive steps must be adjacent in the stream | `next()` |
| `SEL.partition` | Partition contiguity | Consecutive steps must be adjacent within the same key partition | Not applicable |
| `SEL.next` | Skip-till-next-match | Skip non-qualifying events; each step takes the first qualifying event (deterministic) | `followedBy()` |
| `SEL.any` | Skip-till-any-match | Each qualifying event may or may not be taken, giving all combinations; this equals the §11 semantics | `followedByAny()` |

The table goes from most to least restrictive. On a single key the match sets nest: strict ⊆ partition ⊆ next ⊆ any. Cost grows in the same direction. Skip-till-any-match combined with Kleene closure can produce exponentially many matches.

### 12.2 Consumption policies (Snoop parameter contexts)

An **initiator** is the event that starts a partial match. A **terminator** is the event that completes it.

| ID | Policy | Rule | Typical use |
|---|---|---|---|
| `CON.unrestricted` | Unrestricted | Every event may participate in any number of matches | Analysis, testing |
| `CON.recent` | Recent | Only the most recent initiator is kept, and a newer one replaces it. Detection does not consume it. | Latest-value sensor logic; silence measured from the newest reading |
| `CON.chronicle` | Chronicle | Initiators and terminators are paired oldest-first (FIFO); each is consumed once | Request / response pairing |
| `CON.continuous` | Continuous | Each initiator opens its own partial match. A terminator completes all of them, and they are then consumed. | Sliding detection |
| `CON.cumulative` | Cumulative | All constituent occurrences accumulate. The terminator emits one match containing all of them, and all are consumed. | "Batch up until X" |
| `CON.retain` | Retain (per output) | An output that consumes nothing: its constituents stay pending and keep counting towards the next output. An operator may choose consumption per output *reason*: for example, a threshold output consumes, while a timeout flush retains. | Flush without losing progress |

### 12.3 Output, skipping, rate, and evaluation timing

| ID | Parameter | Rule |
|---|---|---|
| `OUT.every` | Every match | Report every match. |
| `OUT.first` | First | Report only the first match per initiator. |
| `OUT.last` | Last | Report only the last match per initiator. Evaluation must be deferred. |
| `OUT.maximal` | Maximal | Report only matches not strictly contained in another match. Evaluation must be deferred until no further extension is possible. |
| `OUT.timeout-partial` | Timed-out partial | When a partial match exceeds its time bound without completing, report it as a *timed-out* match with its own reason, instead of discarding it. (Flink CEP offers the same through its timeout handling.) |
| `OUT.edge` | Edge-triggered | Report only transitions of a condition, entering it and optionally leaving it, never while it persists. Silence and recovery (`PT.silence`, `PT.recovery`) are the standard pair. |
| `OUT.rate-limit` | Rate limit (Δ, mode) | Per operator instance, at most one output per Δ of processing time since the previous output. In mode **drop**, outputs inside the cap are discarded. In mode **defer**, the output waits until the cap ends, and contributions that arrive meanwhile join it, so nothing is lost. |
| `OUT.coalesce` | Coalesce | Several outputs of one instance that are due together become one output. Its interval is [min start, max end]; its multiplicities are summed; and it records how many were merged (for example `windows = 3`). This applies to windows that closed together, missed schedule ticks, and outputs held back during a replay. |
| `SKIP.no-skip` | No skip | After a match, keep every partial match. |
| `SKIP.to-next` | Skip to next | Discard partial matches that started with the same event as the emitted match. |
| `SKIP.past-last` | Skip past last event | Discard partial matches that started after the emitted match started and before it ended. |
| `SKIP.to-first` | Skip to first (x) | Discard partial matches that started after the emitted match started but before the first event bound to x. |
| `SKIP.to-last` | Skip to last (x) | Discard partial matches that started after the emitted match started but before the last event bound to x. |
| `EVAL.immediate` | Immediate | Emit as soon as the terminator arrives. Valid only for monotone patterns. |
| `EVAL.deferred` | Deferred | Emit when the watermark passes the end of the match's scope. Required for trailing negation, absence, and last or maximal output. |

---

## 13. Derivation, closure, and action

| ID | Operation | Rule |
|---|---|---|
| `DR.derive` | Derive | From a match m, build ⟨fresh id, τ′, f(bindings(m)), ts(m), te(m), 1, op, m⟩. The function f may aggregate over Kleene lists. |
| `DR.summary` | Summarized output | Instead of its constituents, an output may carry a summary: the interval it covers, the multiplicity sum, the number of windows or ticks merged, and the contributing sub-streams and kinds (`AG.sources`). A consumer that needs the data re-reads it (`ES.reference`). |
| `DR.output-identity` | Output identity | An output's deduplication identity is derived from its content: operator instance, key, reason, covered interval, and multiplicity, but not the emission time. A retried or replayed emission therefore has the same identity, and downstream deduplication drops it. An output that covers no new data (`RL.rejudge`) also includes its emission time, so that two such outputs are not mistaken for one. |
| `DR.ordered` | Ordered delivery | Outputs are delivered grouped by key, in order within each key, so that a consumer handles one key's outputs one at a time. |
| `DR.closure` | Closure | Derived events re-enter the event space and may be inputs to any operator. |
| `DR.cycle-guard` | Cycle guard | An operator must not consume its own outputs, directly or through a loop, unless it is declared recursive with a termination bound. Detect such loops statically where possible. |
| `DR.act` | Act | A side effect such as an alert, a service call, or a view update. An action is not an event unless it emits one, such as `ActionCompleted`. |
| `DR.view` | Materialized view | A per-key state derived continuously from events, queried on demand or exported on a schedule (`TM.schedule`), but not itself an event unless it is emitted. An example is a health classification by *arrival age* (now − last arrival) and *data age* (now − newest event time): live, late (arriving, but with old data), quiet, or silent. |
| `DR.retract` | Retraction propagation | If any constituent is retracted, every derived event whose provenance (∈*) includes it is retracted. |

---

## 14. Rules as data

In a long-running engine, the program is not fixed at deploy time. The **rules**, meaning which operators run, for which keys, and with which parameters, are data that arrives and changes while the engine runs. This section defines that layer.

| ID | Concept | Definition |
|---|---|---|
| `RL.template` | Rule template | A named, parameterized operator: an id, the kinds of sub-stream it watches (all kinds when omitted), a condition (an operator from §7–§12, such as `PT.accumulate`, `PT.window-close`, `PT.silence`, or `PT.schedule`), modifiers (`OUT.rate-limit`, `PT.flush`, consumption per reason), a target naming what the output is for, default params copied into every output, and the keys it applies to without a binding (all keys, or a list). |
| `RL.binding` | Binding | An instantiation of a template on key k. It has a **binding key** that tells several instances of one rule on one key apart. It may narrow or replace the watched kinds, override the condition's numeric parameters (such as N for accumulate, or Δ for silence), and add params, which override the template's params. A binding with the empty binding key replaces the rule's own instance on that key. |
| `RL.fan-out` | Fan-out | A binding may name several keys. Each key gets its own independent instance with the binding's key and params, firing on its own data. This keeps partitioning intact (`ES.keyed`). |
| `RL.identity` | Instance identity | An instance is identified by (rule id, key, binding key). No two bindings may produce the same instance, whichever sources they come from. |
| `RL.source` | Source | Rules and bindings belong to named sources. The rule set in force is the union of all sources. A source is replaced as a whole, atomically, or edited one item at a time. |
| `RL.ownership` | Ownership | A rule belongs to exactly one source; defining it in a second source is refused. A binding may bind a rule that another source owns. |
| `RL.dangling` | Dangling and invalid items | A binding whose rule is missing is left out of the rules in force and reported, never failing the whole set. Likewise, an item that fails validation is left out and reported with its reason, and the rest of the source applies. |
| `RL.semantics` | Semantic fingerprint | Each instance has a fingerprint: its condition, its watched kinds, and its numeric limits. An edit falls into one of three classes: **no change**; **job-only** (target or params changed, fingerprint unchanged); or **semantic** (fingerprint changed). |
| `RL.carry` | Progress carry | After a semantic edit, an instance's progress (pending contributions, open windows, schedule position, silence clock) restarts by default. Under the policy `continue`, progress carries into the new version if it means the same there: the same condition type, and also the same grid for windows or the same expression for schedules. The carried instance is evaluated at once. Under `continue`, the rate-limit state always carries. A job-only edit keeps all progress. |
| `RL.rejudge` | Re-judge on edit | A job-only edit, or a new binding on a key that already has data, makes an accumulate instance emit once at its next evaluation, with reason `config`. That output covers the newest contribution of its watched kinds (or none) and carries the new params. It consumes nothing (`CON.retain`), it is subject to `OUT.rate-limit`, and it is superseded if a data output goes first, since that output carries the new params too. Window, silence, and schedule instances ignore job-only edits, because their own grids, clocks, and schedules decide when they fire. |
| `RL.converge` | Convergence | Replacing a source with identical content changes nothing and emits nothing. Replacement is therefore idempotent, and repeated syncs converge. |

---

## 15. Worked example: card testing

**Types**
```
Swipe(card: String, amount: Real)                       primitive
Verified(card: String)                                  primitive
SuspectedFraud(card: String, n: Integer, total: Real)   derived
```

**Pattern: "card testing".** A tiny charge is followed by escalating large charges, with no customer verification in between.
```
key-by card
  ( a:Swipe[amount < 5] ; ¬Verified ; (b:Swipe[amount > 500])+ )
  where increasing(b.amount)
  within 10 min
derive SuspectedFraud(card = a.card, n = count(b), total = sum(b.amount))
```

**Trace A** (card c1, times in minutes, consumption `CON.unrestricted`)

| Event | t | Type | amount |
|---|---|---|---|
| e1 | 1 | Swipe | 2 |
| e2 | 2 | Swipe | 600 |
| e3 | 3 | Swipe | 50 |
| e4 | 4 | Swipe | 900 |

Here e1 is the only possible `a`, and e3 qualifies for neither binding.

| Selection | `OUT.every` | `OUT.maximal` |
|---|---|---|
| `SEL.strict` | {e1, e2} | {e1, e2} |
| `SEL.partition` | {e1, e2} | {e1, e2} |
| `SEL.next` | {e1, e2}, {e1, e2, e4} | {e1, e2, e4} |
| `SEL.any` | {e1, e2}, {e1, e4}, {e1, e2, e4} | {e1, e2, e4} |

- Under strict contiguity, e3 breaks the Kleene run, so the match ends at e2.
- Under skip-till-next-match, e3 is skipped and the run extends greedily to e4. The match {e1, e4} is never formed, because e2 was taken deterministically.
- Under skip-till-any-match, b can be any time-ordered, increasing subset of {e2, e4}.

The maximal match derives `SuspectedFraud(card = c1, n = 2, total = 1500)` with ts = 1, te = 4, and C = {e1, e2, e4}. Because maximal output is deferred, it is emitted only once the watermark passes 11 (ts = 1 plus the 10-minute bound), when no further extension is possible.

**Trace A′: negation scope.** Add `Verified(c1)` at t = 3.5 to trace A and consider `SEL.any` with `OUT.every`.

- With `¬Verified` between `a` and `b+`, the forbidden gap runs from te(a) to the *first* b. The result is {e1, e2} and {e1, e2, e4}. The match {e1, e4} is blocked, because its gap (1, 4) contains t = 3.5.
- With `unless Verified` applied to the whole pattern, the result is {e1, e2} only, because its span (1, 2) does not contain t = 3.5.

**Trace B** (card c2): `Swipe(1)` at t = 1, `Verified` at t = 2, `Swipe(800)` at t = 3. There is no match under any strategy.

---

## 16. Case study: Reflex

Reflex is a stateful trigger engine. It turns **data arrivals** into **triggers**: "run job X for device k over this data, because of reason R". A downstream job then runs because its data is there, not because a clock fired. This section shows that Reflex is one program in this specification's vocabulary. Every behavior below is cited by ID. Section 16.5 lists what is deliberately outside the language.

### 16.1 Inputs and state

- **Input.** Each storage write produces one `DataArrival` event: a reference payload (`ES.reference`) listing, per sensor written, the owning device, the sensor id, its measurement type, the sample count n, the interval [first_ts, last_ts], and the newest value.
- **Normalization.** `OP.split` turns each `DataArrival` into one **batch event** per sensor (`ES.batch`). `ST.key-by` keys those by the device that *owns* the sensor, not the device that sent the write. Sensors are sub-streams of their device, and measurement types are their kinds (`ES.substream`).
- **Guards.** `TM.skew-guard` drops and counts arrivals stamped more than an hour ahead of the clock. `CX.entity` gates by the device directory: devices that can report are activated before they send anything; deprecated devices have their state dropped and their arrivals ignored and counted; deleted devices are forgotten.
- **Merge.** `ST.monotonic-merge` runs per sensor. Its contributions are the only input that conditions see, so replays and duplicate writes converge.
- **Isolation.** Every operator is keyed by device, and no device reads another's state (`ES.keyed`). Reflex therefore scales by partition.

### 16.2 Rules

Rules arrive as data (§14): templates (`RL.template`) and per-device bindings (`RL.binding`, `RL.fan-out`), grouped into sources replaced as a whole (`RL.source`, `RL.ownership`, `RL.dangling`, `RL.converge`). The engine itself ships no rules. A rule has one of four conditions:

```
accumulate(N, S)       = PT.accumulate over WN.since-last, on the watched kinds
                         modifiers: max_delay Δd    → PT.flush on the processing clock
                                                      (CON.retain when reset_on_max_delay is false)
                                    min_interval Δc → OUT.rate-limit(Δc, defer)
                         edits:     RL.rejudge on a job-only edit or a new binding with data

window(w, o, all?)     = PT.window-close: WN.tumbling(w, o) anchored at the first contribution,
                         closed by TM.keyed-watermark (mode all when require_all, else any),
                         closed windows coalesced (OUT.coalesce), then OUT.rate-limit(Δc, defer)

silence(Δ, recovery?)  = PT.silence on the hybrid clock (newest sample time + Δ, compared with now),
                         initiator ACT = the device's activation (CX.activation),
                         + PT.recovery when emit_recovery; together OUT.edge;
                         then OUT.rate-limit(Δc, defer)

cron(expr)             = PT.schedule: WN.calendar(expr, processing time, UTC) with WN.empty,
                         missed ticks coalesced (OUT.coalesce); then OUT.rate-limit(Δc, defer)
```

Binding overrides map onto `RL.binding`: watched kinds, N for accumulate, and Δ for silence; the alarm's whole configuration travels in params. Edits follow `RL.semantics`, `RL.carry` (`on_update: continue | restart`), and `RL.rejudge`.

### 16.3 Outputs

A trigger is `DR.derive` with `DR.summary`: target, device, binding key, reason (`samples`, `max_delay`, `window`, `silence`, `recovered`, `schedule`, or `config`), covered interval, samples (`AG.multiplicity`), windows or ticks merged, contributing sensors and types (`AG.sources`), params, and emission time.

- Its deduplication id follows `DR.output-identity`, and adds the emission time for `config` triggers.
- Delivery is `DR.ordered`, grouped by device.
- The engine's fleet health (live, late, quiet, silent per device and per sensor, exported every minute) is a `DR.view` exported on a `TM.schedule`.

### 16.4 Recovery

Restart is `TM.checkpoint`. The snapshot holds instance progress, high-water marks, read positions, the directory's lifecycle flags, and unsent triggers.

A cold start is `TM.replay`:
- triggers are held back and coalesced to one per instance;
- they are released once every shard is at its tip and the first rules push has landed;
- each is judged against the instances then in force, and dropped if its instance is gone;
- processing-time deadlines wait until the replay has caught up.

### 16.5 Outside the language

These are runtime and deployment concerns: an implementation of the language must make them *possible*, but they are not operators.

- Leader election, lease fencing, and standby forwarding. The language requires only that one evaluator is active per key partition.
- Where checkpoints are stored.
- The transport: stream shards and the output queue.
- The HTTP API and its authentication.
- Metrics and heartbeat export.
- The services that compute rule sources from other systems' tables.

### 16.6 Mapping

| Reflex | Spec IDs |
|---|---|
| Data-arrival event (a pointer to committed rows, with counts) | `ES.reference`, `ES.batch` |
| Device owns datastreams; measurement types | `ES.substream`, `ST.key-by` |
| Monotonic merge: `last_ts` only moves forward; straddling arrivals pro-rated | `ST.monotonic-merge`, `ES.batch` share |
| Arrival over an hour ahead of the clock dropped and counted | `TM.skew-guard` |
| Thing directory: watched, deprecated, deleted | `CX.entity`, `CX.activation`, `JN.table` |
| Rule, binding, `also_thing_ids`, binding key | `RL.template`, `RL.binding`, `RL.fan-out`, `RL.identity` |
| Sources replaced whole; one owner per rule; dangling bindings reported | `RL.source`, `RL.ownership`, `RL.dangling`, `RL.converge` |
| `accumulate` (`min_samples`, `min_span_s`) | `PT.accumulate`, `WN.since-last`, `AG.multiplicity`, `AG.span`, `CON.cumulative` |
| `max_delay_s`, `reset_on_max_delay` | `PT.flush`, `OUT.timeout-partial`, `CON.retain`, `TM.clock` (processing) |
| `window` (`window_s`, `offset_s`, `require_all`) | `PT.window-close`, `WN.tumbling`, `TM.keyed-watermark`, `OUT.coalesce` |
| `silence` (`after_s`, `emit_recovery`), counted from watch start | `PT.silence`, `PT.recovery`, `PT.activation`, `OUT.edge`, `TM.clock` (hybrid), `TM.idle` |
| `cron` (five fields, UTC) | `PT.schedule`, `WN.calendar`, `WN.empty`, `TM.schedule`, `OUT.coalesce` |
| `min_interval_s`, with capped data carried forward | `OUT.rate-limit` (defer) |
| `on_update: continue / restart`; reason `config` | `RL.semantics`, `RL.carry`, `RL.rejudge` |
| Trigger fields; FIFO grouped by device; content-derived deduplication id | `DR.derive`, `DR.summary`, `DR.ordered`, `DR.output-identity` |
| Fleet health states | `DR.view`, `TM.schedule` |
| Checkpoint and restore; cold-start replay with held-back triggers | `TM.checkpoint`, `TM.replay`, `OUT.coalesce` |

### 16.7 Acceptance traces

In these traces, *a* is processing time, sample intervals are in event time, both in seconds, and all data is for one device. These traces reproduce the reference engine's behavior exactly; an implementation of this spec must too.

**R1: accumulate with a deferring rate limit.** Rule: `accumulate(N = 3, S = 0)`, `min_interval = 60`, no `max_delay`.

| a | Contribution (interval, n) | Pending after | Output |
|---|---|---|---|
| 0 | [0, 0], 1 | 1 | — |
| 10 | [10, 10], 1 | 2 | — |
| 20 | [20, 20], 1 | 3 → 0 | `samples` at a = 20, window [0, 20], samples 3; capped until a = 80 |
| 30 | [25, 30], 2 | 2 | — |
| 40 | [40, 40], 1 | 3 | ready, but capped |
| 50 | [50, 50], 1 | 4 | capped |
| 80 | (timer) | 4 → 0 | `samples` at a = 80, window [25, 50], samples 4 |

Nothing is lost: the capped data rides in the output at a = 80.

**R2: high-water-mark merge.** One sensor.

| Arrival (interval, n) | hw before | New share | Contribution | hw after | late |
|---|---|---|---|---|---|
| [90, 100], 10 | −∞ | 10 | [90, 100], 10 | 100 | 0 |
| [95, 105], 10 | 100 | round(10 · 5/10) = 5 | [100, 105], 5 | 105 | 5 |
| [95, 105], 10 (duplicate) | 105 | 0 | — | 105 | 15 |
| [80, 90], 10 (stale) | 105 | 0 | — | 105 | 25 |

**R3: window with `require_all`.** Rule: `window(w = 60, o = 0, require_all)` on kinds {load, position}. W is W_all.

| Step | Contribution (kind, interval, n) | W | Output |
|---|---|---|---|
| 1 | load, [0, 50], 50 | −∞ (no position yet) | — (grid anchored: first window [0, 60)) |
| 2 | position, [0, 70], 70 | 50 | — |
| 3 | load, [51, 130], 80 | 70 | `window` [0, 60], windows 1, samples 200 |
| 4 | position, [71, 250], 180 | 130 | `window` [60, 120], windows 1, samples 180 |
| 5 | load, [131, 300], 170 | 250 | `window` [120, 240], windows 2, samples 170 |

A trigger's samples are all contributions since the previous trigger. Under mode *any* instead, the window [0, 60] would close at step 2, because W_any = 70 there.

**R4: silence from activation, with recovery.** Rule: `silence(Δ = 600, emit_recovery)`. The device is activated at a = 0 and sends nothing at first.

| a | Event | Output |
|---|---|---|
| 0 | activation | — |
| 600 | (timer: 0 + 600) | `silence`, window [none, 600] |
| 900 | contribution [880, 890], 2 | `recovered`, window [none, 890] |
| 1490 | (timer: 890 + 600, the hybrid clock) | `silence`, window [890, 1490] |
| 1600 | contribution [1595, 1600], 1 | `recovered`, window [890, 1600] |

Between those transitions nothing else fires, because the output is edge-triggered.

**R5: schedule with missed ticks.** Rule: `cron("0 * * * *")`. The instance is created at 10:20, so its first tick is 11:00 and its first period starts at 10:00. Contributions arrive at 10:30 (n = 2) and 10:45 (n = 3). The evaluator is suspended from 10:50 to 13:10.

- At 13:10: one `schedule` output, window [10:00, 13:00], windows 3, samples 5.
- At 14:00: `schedule` output, window [13:00, 14:00], windows 1, samples 0 (an empty window still fires).

**R6: edits.** Rule: `accumulate(N = 3)`, `min_interval = 0`, params {threshold: 50}. The device has one pending contribution at [500, 500].

- **Job-only edit** at a = 1000, params becoming {threshold: 40}: one `config` output, window [500, 500], samples 0, params {threshold: 40}. The pending contribution stays pending. After further contributions at [510, 510] and [520, 520], the next output is `samples`, window [500, 520], samples 3.
- **Semantic edit, variant.** Start instead with two pending contributions, [500, 500] and [510, 510], and an edit from N = 3 to N = 2:
  - Under `continue`, progress carries and is evaluated at once: `samples`, window [500, 510], samples 2.
  - Under `restart`, nothing is output, and the instance waits for two new contributions.

---

## 17. Implementation notes

- **Automata.** Patterns compile to automata. A common design is an NFA with match buffers (SASE+'s NFA^b). Sharing partial-match buffers across runs (as Flink's shared versioned buffer does) avoids copying.
- **Negation and absence** are evaluated either deferred (at the watermark) or speculatively with retraction. A total silence needs a processing-time or hybrid timer (`TM.idle`).
- **No polling loop.** Each operator instance reports the next moment it needs attention without new data: a flush, the end of a rate-limit cap, a silence deadline, or a schedule tick. A single min-heap of deadlines wakes instances when due. An instance arms a deadline only if it is earlier than the one it already holds, so the heap grows with deadlines, not arrivals. A wake-up that turns out to be early is harmless, because evaluation always re-checks the condition against the current time. An arrival visits only the instances watching its kind on its key.
- **State bounds.** State is bounded only by `within`, window sizes, and watermarks. A pattern with Kleene closure or negation and no `within` can hold state forever.
- **Out-of-order input** is handled by reordering (§5) before pattern matching, by dropping what lies behind a high-water mark (`ST.monotonic-merge`), or by processing speculatively and retracting.
- **Concurrency.** Running all evaluation for a partition on one event loop removes the need for locks. Keyed isolation lets partitions run in parallel.
- **Testing.** A brute-force evaluator of §11, which enumerates subsets of a small history, is an oracle for any optimized engine. Differential testing between the two catches semantic bugs that hand-written cases miss.

---

## 18. Glossary

- **Batch event**: An event standing for n underlying samples spread over its interval (`ES.batch`).
- **Binding**: A rule template instantiated on a key, with overrides and params (§14).
- **Context**: The scope in which an operator evaluates; it generalizes windows (§7).
- **Contribution**: The new part of an arrival after high-water-mark merging (§5).
- **Event cloud**: The partially ordered set of observed events under ≺, →, and ∈.
- **High-water mark**: The newest event time already merged for a sub-stream.
- **Initiator / terminator**: The events that start and complete a partial match.
- **Instance (subscription)**: One rule running on one key under one binding key.
- **Monotone**: Adding input never invalidates an earlier output.
- **Partial match (run)**: A match in progress that is still waiting for its remaining steps.
- **Provenance**: The constituent links C, and their transitive closure ∈*.
- **Source**: A named, independently replaced set of rules and bindings.
- **Watermark**: A progress assertion about event time (§6).

---

## 19. References

- David Luckham, *The Power of Events*, Addison-Wesley, 2002.
- Opher Etzion and Peter Niblett, *Event Processing in Action*, Manning, 2010.
- S. Chakravarthy, V. Krishnaprasad, E. Anwar, S.-K. Kim, "Composite Events for Active Databases: Semantics, Contexts and Detection," VLDB 1994.
- E. Wu, Y. Diao, S. Rizvi, "High-Performance Complex Event Processing over Streams," SIGMOD 2006.
- J. Agrawal, Y. Diao, D. Gyllstrom, N. Immerman, "Efficient Pattern Matching over Event Streams," SIGMOD 2008.
- J. F. Allen, "Maintaining Knowledge about Temporal Intervals," *Communications of the ACM*, 1983.
- G. Cugola and A. Margara, "Processing Flows of Information: From Data Stream to Complex Event Processing," *ACM Computing Surveys*, 2012.
- T. Akidau et al., "The Dataflow Model," VLDB 2015.
- J. Gray et al., "Data Cube: A Relational Aggregation Operator Generalizing Group-By, Cross-Tab, and Sub-Totals," *Data Mining and Knowledge Discovery*, 1997.
- Apache Flink documentation, "FlinkCEP — Complex Event Processing for Flink."
