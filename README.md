# occur

**occur** is a language for event processing, written in Racket as
`#lang occur`. A program declares event types, sources, and streams, then
windows, aggregates, joins, patterns, and rules over them; a deterministic
runner executes it against data and a simulated clock. The language
realizes the specification in [`docs/spec.md`](docs/spec.md): every concept
there has a stable ID, such as `PT.sequence`, and
[`docs/coverage.md`](docs/coverage.md) maps each of the 175 IDs to its
surface form, runtime function, tests, and an example.

- [Quick start](#quick-start)
- [A first program](#a-first-program)
- [The algebra](#the-algebra)
- [More examples](#more-examples)
- [Reflex, a trigger engine written in occur](#reflex-a-trigger-engine-written-in-occur)
- [How it is tested](#how-it-is-tested)
- [Repository layout](#repository-layout)

Documentation: the manual (`raco docs occur`, or the Scribble sources in
[`scribblings/`](scribblings/occur.scrbl)), the specification
[`docs/spec.md`](docs/spec.md), the coverage table
[`docs/coverage.md`](docs/coverage.md), the Reflex mapping
[`docs/reflex.md`](docs/reflex.md), and the decision records in
[`docs/decisions/`](docs/decisions/README.md).

## Quick start

occur needs [Racket](https://racket-lang.org) 9.x.

```sh
git clone https://github.com/Leeevai/occur.git
cd occur
raco pkg install --auto --batch --link --name occur   # installs the package from this directory
racket examples/card-testing.rkt                       # runs a program and prints its outputs
raco test -j 8 .                                       # runs the test suite
raco docs occur                                        # opens the manual
```

Every `#lang occur` module is a program: running it with `racket` runs its
sources through its streams and prints each output as
`@processing-time node: event`.

## A first program

Spec §15's card-testing attack: a tiny charge, then escalating large
charges, with no customer verification in between.

```racket
#lang occur

;; ES.type: event types, checked at compile time
(event-type Swipe [card : String] [amount : Real])
(event-type Verified [card : String])
(event-type SuspectedFraud [card : String] [n : Integer] [total : Real] #:derived)

;; ES.stream: a source keyed by card, its watermark following the data
(source swipes #:types (Swipe Verified) #:key card #:watermark ascending
  #:from (events (Swipe #:at (minutes 1) #:id "e1" [card "c1"] [amount 2])
                 (Swipe #:at (minutes 2) #:id "e2" [card "c1"] [amount 600])
                 (Swipe #:at (minutes 3) #:id "e3" [card "c1"] [amount 50])
                 (Swipe #:at (minutes 4) #:id "e4" [card "c1"] [amount 900])))

;; §11–§12: a pattern with its selection and output policies
(define-pattern card-testing #:from swipes
  #:within (minutes 10)                       ; PT.within
  #:selection skip-till-next-match            ; SEL.next
  #:output maximal                            ; OUT.maximal, so EVAL.deferred
  (seq (a : Swipe (< amount 5))               ; PT.sequence of a primitive …
       (absent Verified)                      ; … no verification in the gap (PT.negation) …
       (kleene+ (b : Swipe (> amount 500))))  ; … then one or more large charges (PT.kleene-plus)
  #:where (increasing b.amount)               ; PT.where with PT.trend.increasing
  #:emit (SuspectedFraud [card a.card] [n (count b)] [total (sum b.amount)]))   ; DR.derive
```

Running it prints one derived event, emitted once the watermark passes
minute 11, when no later charge can extend the match:

```
@240 card-testing: SuspectedFraud [60, 240] card="c1" n=2 total=1500
```

The compiler checks the program before it runs: an unknown attribute
(`a.amont`), an unknown policy, a Kleene binding used as a single event, a
binding override that does not fit a rule's condition, or a stream that
consumes itself without a recursion bound are compile-time errors with
source locations, and a pattern with negation or Kleene closure but no
time bound draws a warning. [`examples/card-testing.rkt`](examples/card-testing.rkt)
runs this pattern under all four selection strategies and both output
policies, reproducing every cell of §15's table on the incremental engine
and on the reference oracle.

## The algebra

This section states the semantics occur implements, following the
specification section by section. The manual and
[`docs/spec.md`](docs/spec.md) have the details and the IDs.

### Time and events (§2)

Time is the extended reals, in seconds, with exact arithmetic where the
input is exact:

```math
\mathbb{T} = \mathbb{R} \cup \{-\infty, +\infty\}, \qquad \text{a duration } \Delta \in \mathbb{T},\ \Delta \ge 0.
```

An event is a tuple

```math
e = \langle \mathit{id}, \tau, v, t_s, t_e, n, \mathit{src}, C \rangle, \qquad t_s \le t_e,\quad n \in \mathbb{N},
```

with an id, a type $\tau$ whose schema types the payload $v$, an occurrence
interval $[t_s, t_e]$, a multiplicity $n$ (a batch of $n$ samples spread
evenly over the interval, `ES.batch`), a source, and constituents $C$: the
events it was derived from (`ES.composite`). Primitive events have
$C = \emptyset$. The event space is built in levels,

```math
\mathcal{E}_0 = \text{primitive events}, \qquad
\mathcal{E}_{k+1} = \mathcal{E}_k \cup \{\, e : C(e) \subseteq \mathcal{E}_k \,\},
```

and an event's level is the length of its longest derivation chain. A
*reference* event carries a pointer to a sub-stream's samples over its
interval, with a small summary, instead of the samples (`ES.reference`).

**Relations.** Precedence, membership, and causality:

```math
a \prec b \iff t_e(a) < t_s(b), \qquad
a \in C(c), \qquad
a \in^{*} c \iff a \in C(c) \lor \exists d \in C(c).\ a \in^{*} d, \qquad
a \to b \implies t_s(a) \le t_s(b).
```

Precedence and causality are strict partial orders; temporal order does not
imply causality. Causes are recorded (a `cause` attribute naming the
cause's id) or inferred by a rule (`ES.precedes`, `ES.membership`,
`ES.causality`).

**Streams and histories.** A stream $S$ is a set of events of declared
types. Its history at time $t$ is everything that completely occurred by
then, whether or not it has arrived yet:

```math
H_S(t) = \{\, e \in S : t_e(e) \le t \,\}.
```

A key function $\kappa$ partitions a stream into $S_k = \{ e \in S : \kappa(e) = k \}$
(`ES.keyed`); an entity key owns sub-streams, each of a kind, such as a
device's sensors and their measurement types (`ES.substream`).

**Batch shares.** The share of a batch event strictly past a boundary $b$:

```math
\mathrm{share}(e, b) =
\begin{cases}
0 & n(e) = 0 \text{ or } t_e(e) \le b \\
n(e) & t_s(e) > b \\
\min\!\left(n(e),\ \max\!\left(1,\ \mathrm{round}\!\left(n(e)\,\dfrac{t_e(e) - b}{t_e(e) - t_s(e)}\right)\right)\right) & \text{otherwise}
\end{cases}
```

with round-half-to-even.

### Single events and stream structure (§4, §5)

The single-event operations are filter $\sigma_p(S) = \{e \in S : p(e)\}$,
projection $\pi_A$, map $e \mapsto f(e)$, enrichment from a lookup
$e \oplus R(k(e))$, split $e \mapsto \{e_1, \dots, e_n\}$, and routing to the
first (or every) branch whose predicate holds. Stream structure adds merge,
key-by, deduplication within a horizon, reordering by the watermark,
throttling, and **high-water-mark merging**: per sub-stream $s$, a mark
$hw(s)$ starting at $-\infty$; for each arrival $e$ on $s$,

```math
\mathit{new}(e) = \mathrm{share}(e, hw(s)), \qquad
\text{if } \mathit{new}(e) > 0:\ \text{emit } \langle t_s = \max(t_s(e), hw(s)),\ t_e = t_e(e),\ n = \mathit{new}(e)\rangle,\ hw(s) := t_e(e),
```

and $\mathit{late}(s) \mathrel{+}= n(e) - \mathit{new}(e)$. Re-applying an
applied arrival is a no-op, so replays and duplicate writes converge.

```racket
(define-stream contributions (from arrivals)
  (key-by owner #:substream sensor #:kind type)   ; ES.substream
  (skew-guard (hours 1))                          ; TM.skew-guard: drop data stamped an hour ahead
  (monotonic-merge))                              ; ST.monotonic-merge
```

### Time and progress (§6)

A watermark $W$ asserts that no future arrival has $t_e < W$; it never
decreases. A source's watermark is none, ascending ($W = \max t_e$ seen),
bounded-delay ($W = \max t_e - d$), or driven by heartbeats. Per key, the
data drives a *keyed* watermark over the watched kinds $M$, from each kind's
high-water mark $hw_k(m)$ ($-\infty$ for a kind never seen):

```math
W_k^{\mathrm{any}} = \max_{m \in M} hw_k(m), \qquad W_k^{\mathrm{all}} = \min_{m \in M} hw_k(m).
```

Operators keep timers on the event-time clock (due when $W$ passes them) or
the processing clock (due when the simulated clock reaches them); a hybrid
timer compares an event time with processing time, which is the only way to
notice that nothing arrives at all. Late events are dropped, sent to a side
stream, or accepted within an allowed lateness. A retraction $\ominus e$
cancels an emitted $e$; a correction is $\ominus e_{\mathrm{old}}$ followed by
$e_{\mathrm{new}}$. Calendar schedules are five-field cron expressions read in
UTC, with ticks missed together coalesced.

### Windows and contexts (§7)

A window assignment $\omega$ maps an event to half-open windows $[s, e)$:

```math
\begin{aligned}
\text{tumbling}(\Delta, o):&\quad \omega(e) = \{[o + k\Delta,\ o + (k+1)\Delta)\},\ k = \lfloor (t(e) - o)/\Delta \rfloor \\
\text{sliding}(\Delta, \sigma):&\quad \omega(e) = \{[k\sigma,\ k\sigma + \Delta) : k\sigma \le t(e) < k\sigma + \Delta\},\quad |\omega(e)| \le \lceil \Delta/\sigma \rceil \\
\text{session}(g):&\quad \text{maximal runs with consecutive gaps} < g,\ \text{window } [\mathit{first},\ \mathit{last} + g) \\
\text{count}(n, m):&\quad \text{the last } n \text{ events, advancing every } m \\
\text{landmark}(t_0):&\quad [t_0, \mathit{now})
\end{aligned}
```

and punctuated (opened and closed by events, or a timeout), since-last
(closed when a condition on what accumulated holds), and calendar windows.
A window fires when the watermark reaches its end; with allowed lateness
$L$ a late event updates it with a correction until $W \ge e + L$; empty
windows fire on request. Contexts tag each event with the instances it
belongs to: fixed, event-delimited, and sliding intervals; segments by
attribute values; distance from a place, an entity, or an event; the time an
external state holds; an entity's lifecycle from a directory, where an
active entity is *activated* before it sends anything; and intersections of
these.

```racket
(define-stream spend (from payments)
  (key-by card)
  (window (sliding #:size (minutes 30) #:slide (minutes 10)))
  (aggregate [total (sum amount)] [n (count)] [p90 (percentile amount 0.9)]))
```

### Aggregation (§8)

An aggregate $F$ is **distributive** when partial results combine,
$F(X \uplus Y) = G(F(X), F(Y))$ (count, sum, min, max); **algebraic** when a
fixed tuple of distributive ones determines it,

```math
\mathrm{avg} = \frac{\sum x}{\sum 1}, \qquad
\mathrm{var} = \frac{\sum x^2}{N} - \left(\frac{\sum x}{N}\right)^{\!2};
```

**holistic** when no constant-size state suffices (median, percentiles,
distinct counts, mode, top-k); and **approximate** when a sketch stands in
(HyperLogLog, t-digest, Count-Min). Multiplicity counts samples, not events:
$\mathrm{count}(X) = \sum_{e \in X} n(e)$, and $\mathrm{span}(X) = \max t_e - \min t_s$.
Invertible aggregates also *remove*, so a sliding window adds the new event
and removes the oldest.

### Joins (§9)

With $\theta$ a predicate over a left event $s$ and a right event $r$ sharing a
key:

```math
\begin{aligned}
\text{window:}&\quad s \bowtie_\theta r \text{ if } \theta(s, r) \text{ and both lie in one window} \\
\text{interval:}&\quad s \bowtie_\theta r \text{ if } \theta(s, r) \text{ and } t(r) \in [t(s) - \Delta_1,\ t(s) + \Delta_2] \\
\text{table:}&\quad s \bowtie R_{\mathit{now}}(\kappa(s)) \quad \text{(the right stream upserts rows)} \\
\text{as-of:}&\quad s \bowtie r_v \text{ with } v \text{ the version valid at } t(s)
\end{aligned}
```

The output is a derived event with $C = \{s, r\}$; an outer join also emits
the unmatched events, with the other side null.

```racket
(define-stream far-logins (from payments)
  (join logins #:kind window #:window (tumbling #:size (minutes 15)) #:key card
        #:on (not (equal? left.country right.country))
        #:select ([card left.card] [paid-in left.country] [logged-in right.country])))
```

### Allen's interval algebra (§10)

For proper intervals $a = [a^-, a^+]$ and $b = [b^-, b^+]$ exactly one of
thirteen relations holds:

| Relation | Condition | Inverse |
|---|---|---|
| $a$ before $b$ | $a^+ < b^-$ | after |
| $a$ meets $b$ | $a^+ = b^-$ | met-by |
| $a$ overlaps $b$ | $a^- < b^- < a^+ < b^+$ | overlapped-by |
| $a$ starts $b$ | $a^- = b^-,\ a^+ < b^+$ | started-by |
| $a$ during $b$ | $b^- < a^-,\ a^+ < b^+$ | contains |
| $a$ finishes $b$ | $a^+ = b^+,\ b^- < a^-$ | finished-by |
| $a$ equals $b$ | $a^- = b^-,\ a^+ = b^+$ | equals |

Between a point and an interval, five remain (before, starts, during,
finishes, after). Every relation is a predicate in stage and pattern
expressions, and `(allen-relation a b)` names the one that holds:

```racket
(define promised (interval (minutes 100) (minutes 200)))
(define-stream punctuality (from stops) (map [relation (allen-relation it promised)]))
(define-stream on-time (from stops)
  (filter (or (during? it promised) (starts? it promised) (finishes? it promised) (equals? it promised))))
```

### Patterns (§11)

The grammar:

```math
\begin{aligned}
P \;::=\;& x{:}\tau[p] \;\mid\; \mathrm{ACT} \;\mid\; P ; P \;\mid\; P \land P \;\mid\; P \lor P
\;\mid\; P ; \lnot Q ; P \;\mid\; P ; \lnot Q \ \mathrm{within}\ \Delta \;\mid\; P \ \mathrm{unless}\ Q \\
\mid\;& P^{+} \;\mid\; P^{*} \;\mid\; P^{\{n,m\}} \;\mid\; \mathrm{ANY}(k; P_1, \dots, P_n)
\;\mid\; P\ \mathrm{within}\ \Delta \;\mid\; P\ \mathrm{where}\ \theta \\
\mid\;& \mathrm{APER}(P_1, P_2, P_3) \;\mid\; \mathrm{APER}^{*}(P_1, P_2, P_3)
\;\mid\; \mathrm{PER}(P_1, \Delta, P_3) \;\mid\; \mathrm{PER}^{*}(P_1, \Delta, P_3)
\end{aligned}
```

A match $m$ is a set of events with bindings (a binding names an event, or a
time-ordered list of events for a Kleene closure); its interval is
$t_s(m) = \min t_s$ and $t_e(m) = \max t_e$ over its events. The semantics
map a pattern and a history $H$ to a set of matches:

```math
\begin{aligned}
[\![x{:}\tau[p]]\!] &= \{\, \{e\} : e \in H,\ \mathrm{type}(e) = \tau,\ p(e) \,\} \quad (x \mapsto e) \\
[\![\mathrm{ACT}]\!] &= \{\, \emptyset \,\}, \quad t_s = t_e = \text{the activation instant} \\
[\![P_1 ; P_2]\!] &= \{\, m_1 \cup m_2 : m_i \in [\![P_i]\!],\ t_e(m_1) < t_s(m_2) \,\} \\
[\![P_1 \land P_2]\!] &= \{\, m_1 \cup m_2 : m_i \in [\![P_i]\!],\ m_1 \cap m_2 = \emptyset \,\} \\
[\![P_1 \lor P_2]\!] &= [\![P_1]\!] \cup [\![P_2]\!] \\
[\![P_1 ; \lnot Q ; P_2]\!] &= \{\, m_1 \cup m_2 \in [\![P_1 ; P_2]\!] : \nexists q \in [\![Q]\!].\ t_e(m_1) < t_s(q) \land t_e(q) < t_s(m_2) \,\} \\
[\![P ; \lnot Q\ \mathrm{within}\ \Delta]\!] &= \{\, m \in [\![P]\!] : \nexists q \in [\![Q]\!].\ t_e(m) < t_s(q) \le t_e(m) + \Delta \,\}, \quad t_e = t_e(m) + \Delta \\
[\![P\ \mathrm{unless}\ Q]\!] &= \{\, m \in [\![P]\!] : \nexists q \in [\![Q]\!],\ q \notin m.\ t_s(m) < t_s(q) \land t_e(q) < t_e(m) \,\} \\
[\![P^{+}]\!] &= \textstyle\bigcup_{k \ge 1} [\![P ; \cdots ; P]\!] \ (k \text{ copies}), \qquad [\![P^{*}]\!] = [\![P^{+}]\!] \cup \{\emptyset\} \\
[\![\mathrm{ANY}(k; P_1, \dots, P_n)]\!] &= \{\, m_{i_1} \cup \dots \cup m_{i_k} : i_1 < \dots < i_k,\ m_{i_j} \in [\![P_{i_j}]\!] \text{ pairwise disjoint} \,\} \\
[\![P\ \mathrm{within}\ \Delta]\!] &= \{\, m \in [\![P]\!] : t_e(m) - t_s(m) \le \Delta \,\} \\
[\![P\ \mathrm{where}\ \theta]\!] &= \{\, m \in [\![P]\!] : \theta(\mathrm{bindings}(m)) \,\}
\end{aligned}
```

$P^{\{n,m\}}$ is $P^+$ with $n \le k \le m$. An aperiodic pattern opens an
interval at $e_1 \in [\![P_1]\!]$ and closes it at the first later
$e_3 \in [\![P_3]\!]$: APER yields $\{e_1, e_2\}$ for every
$e_2 \in [\![P_2]\!]$ strictly inside, and APER* one match
$\{e_1, e_3\} \cup \{e_2, \dots\}$ when it closes. A periodic pattern ticks at
$t_e(e_1) + j\Delta$, $j = 1, 2, \dots$, while that is before $t_s(e_3)$: PER
yields each tick, PER* one match with the list of ticks.

In `#lang occur` the same patterns read:

| Algebra | Surface |
|---|---|
| $x{:}\tau[p]$ | `(x : T (> amount 5))` |
| $P ; Q$, $P \land Q$, $P \lor Q$ | `(seq P Q)`, `(and P Q)`, `(or P Q)` |
| $P ; \lnot Q ; R$ | `(seq P (absent Q) R)` |
| $P ; \lnot Q\ \mathrm{within}\ \Delta$ | `(seq P (absent Q #:within d))` |
| $P\ \mathrm{unless}\ Q$ | `(unless P Q)` |
| $P^+$, $P^*$, $P^{\{n,m\}}$ | `(kleene+ P)`, `(kleene* P)`, `(repeat P n m)` |
| $\mathrm{ANY}(k; \dots)$ | `(any k P ...)` |
| $P\ \mathrm{within}\ \Delta$, $P\ \mathrm{where}\ \theta$ | `#:within d`, `#:where θ` (or the forms `within`, `where`) |
| APER, APER*, PER, PER* | `(aperiodic S X E)`, `(aperiodic* S X E)`, `(periodic S d E)`, `(periodic* S d E)` |
| ACT | `act` |

In `#:where` and `#:emit`, `b.amount` is an attribute of the binding `b`,
or the list of them for a Kleene binding, and `count`, `sum`, `avg`, `min`,
`max`, `span`, `median`, the trends (`increasing`, `decreasing`,
`non-increasing`, `non-decreasing`, `stable`, `mixed`), Allen predicates,
`relative-max?`, and the spatial predicates are available.

**Two engines.** The reference oracle (`#:engine reference`) evaluates
these equations by enumeration over the history; it is the executable
specification. The default engine is incremental: a tree of match automata
with delta joins, deciding negation and absence when the watermark makes
the answer certain, and absence on the hybrid clock when processing time
does. Property tests check that both agree on random patterns and
histories.

### Policies (§12)

**Selection** decides which events may be skipped between consecutive
steps. On one key the match sets nest:

```math
[\![P]\!]_{\mathrm{strict}} \subseteq [\![P]\!]_{\mathrm{partition}} \subseteq [\![P]\!]_{\mathrm{next}} \subseteq [\![P]\!]_{\mathrm{any}}.
```

Strict contiguity requires the next arrival over all keys; partition
contiguity the next within the key; skip-till-next-match that no event
qualifying for the same step arrived in between; skip-till-any-match
nothing. **Consumption** decides which candidates a terminator completes:
unrestricted, recent (the newest initiator), chronicle (the oldest, each
used once), continuous (every open initiator, consumed together),
cumulative (one match with every occurrence), and retain (an output reason
that consumes nothing). **Output** reports every match, the first or the
last per initiator, or the maximal ones, those no other match strictly
contains; last and maximal wait until no extension is possible, which is
$t_s + \Delta$ under `within Δ`. **Skipping** after a match discards later
candidates by initiator: to-next, past-last, to-first($x$), to-last($x$).
**Evaluation** is deferred until the watermark passes the end of the
match's scope, or immediate for monotone patterns. On streams, edge output
reports only transitions, rate limits allow one output per $\Delta$ (and
`defer` carries what was capped into the next), and coalescing merges a
key's outputs due at one instant.

```racket
;; which login explains a large payment, under each consumption policy
(define-syntax-rule (login-then-large name consumption)
  (define-pattern name #:from activity #:key card #:within (minutes 30) #:consumption consumption
    (seq (l : Login) (p : Payment (>= amount 1000)))))
(login-then-large any-login unrestricted)   ; both logins
(login-then-large latest-login recent)      ; only the most recent
(login-then-large oldest-login chronicle)   ; only the oldest, once
(login-then-large every-login continuous)   ; both, consumed together
```

### Derived families (§11.4)

These are expressible with the operators above and named for convenience:

```math
\begin{aligned}
\text{threshold:}&\quad P\ \mathrm{where}\ \mathrm{op}(\mathrm{agg}(\mathrm{bindings}), k) \\
\text{increasing:}&\quad x_1 < x_2 < \dots < x_n \qquad \text{stable}(\varepsilon):\ \max x - \min x \le \varepsilon \\
\text{relative max:}&\quad x \ge y \text{ for every other matched } y \\
\text{accumulate}(N, S):&\quad (b{:}X)^{+}\ \mathrm{where}\ \mathrm{count}(b) \ge N \land \mathrm{span}(b) \ge S \quad (\text{SEL.next, OUT.first, CON.cumulative}) \\
\text{silence}(\Delta):&\quad (\mathrm{ACT} \lor x{:}X) ; \lnot X\ \mathrm{within}\ \Delta \quad \text{on the hybrid clock} \\
\text{recovery:}&\quad s{:}\mathrm{Silence} ; x{:}X \quad (\text{edge-triggered with silence})
\end{aligned}
```

along with spatial distance and movement, window close (a tumbling grid
closed by the keyed watermark, windows that close together coalesced),
flush (an accumulation not ready $\Delta$ after its first contribution is
emitted anyway), and schedule (calendar windows that fire even when empty).

### Derivation (§13)

A match derives an event $e'$ with $C(e') = $ the match's events (or a
summary in their place); derived events are events, so streams and
patterns read them (closure), and a loop through `feedback` is bounded by a
recursion level (the cycle guard). An output's deduplication identity
depends only on its content, the operator instance, key, reason, covered
interval, and multiplicity, so a replayed emission has the same identity.
Retraction propagates through provenance:

```math
\ominus a \implies \ominus e' \quad \text{for every derived } e' \text{ with } a \in^{*} e'.
```

An operator still holding a retracted event never emits it, or anything
derived from it; a window that has not fired forgets it. Outputs are
delivered grouped by key; actions run per event; views keep a per-key state
exported on a schedule, such as health by arrival age and data age.

### Rules as data (§14)

A rule template names a condition (an operator such as accumulate, window
close, silence, or schedule), the kinds it watches, modifiers (a rate
limit, a flush, consumption per reason), a target, default params, and the
keys it applies to. Bindings attach a template to keys, with overrides and
params; one binding may fan out to several keys. An instance is identified
by

```math
(\text{rule},\ \text{key},\ \text{binding key}),
```

and the instances in force are a function of the rule sources, each
replaced whole or patched, with one owner per rule; dangling and invalid
items are reported, and replacing a source with identical content changes
nothing (convergence). An edit is classified by a semantic fingerprint: a
job-only edit re-judges with reason `config`, a semantic edit carries
progress under `continue` or restarts.

```racket
(rule-template threshold-check
  #:condition (pt:accumulate #:count 3 #:span 0)
  #:rate-limit (60 defer)
  #:target "threshold-evaluator" #:params ([threshold 50])
  #:applies-to none)
(rule-source alarms
  (bind threshold-check #:on "d1" #:also ("d2") #:key "standard")
  (bind threshold-check #:on "d2" #:key "strict" #:count 2 #:params ([threshold 40])))
(define-stream triggers (from arrivals) (rules))
```

### Checkpoint and replay (§6, §16.4)

A checkpoint snapshots every operator's state, the read position, and the
undelivered outputs; restoring it re-reads what arrived after it, so outputs
repeat only within the last interval, with the same identities. A cold
start replays all retained input with processing-time timers suspended,
holds the outputs back, coalesces them to one per operator instance,
releases them at the first configuration push, and judges each against the
instances then in force.

```racket
(script
 (at 10 arrivals (write 10 10))
 (checkpoint)
 (at 20 arrivals (write 20 20))
 (restore)                                  ; re-reads the write at 20
 (cold-start)                               ; replays everything, holds the triggers
 (rule-source-push 'alarms alarms))         ; releases them, one per instance
```

## More examples

Every program in [`examples/`](examples/) runs with `racket file.rkt` and
has a test that checks its outputs.

| Program | What it shows |
|---|---|
| [`card-testing.rkt`](examples/card-testing.rkt) | Spec §15: traces A, A′, and B under every selection strategy and output policy |
| [`fraud.rkt`](examples/fraud.rkt) | Payments from ingestion (merge, dedupe, reorder, lateness, sampling, enrich, split, route, delay, chargeback) through windows, aggregates, all five joins, patterns with every policy, and escalation over derived alerts |
| [`iot-telemetry.rkt`](examples/iot-telemetry.rkt) | Pumps' temperatures: landmark and punctuated windows, every kind of context, trends, peaks, aperiodic and periodic patterns, silence on the hybrid clock |
| [`logistics.rkt`](examples/logistics.rkt) | Trucks: geofences, stuck and far-out trucks, heading home, trips and provenance, Allen's thirteen relations to delivery windows, causality |
| [`history.rkt`](examples/history.rkt) | A stream's history at a given time, from a program's run |
| [`reflex/`](examples/reflex/) | Reflex's acceptance traces R1–R6, a fleet on arrivals and rules as data, fleet health, recovery, convergence |

A few more fragments, adapted from those programs.

**Late events and a side stream** (`TM.lateness`, `ST.reorder`):

```racket
(source web #:types (Payment) #:watermark (bounded-delay (minutes 2)) #:from (jsonl "payments.jsonl"))
(define-stream ordered (from web) (lateness #:policy side #:side-output late-payments) (reorder))
```

**Sessions, with what they contain** (`WN.session`, `AG.*`):

```racket
(define-stream sessions (from unique)
  (key-by card)
  (window (session #:gap (minutes 10)))
  (aggregate [payments (events)] [first-merchant (first merchant)] [merchants (collect merchant)]
             [biggest (argmax amount)]))
```

**An unconfirmed large payment** (`PT.absence`):

```racket
(define-pattern unconfirmed #:from activity #:key card
  (seq (p : Payment (>= amount 1000)) (absent ThreeDS #:within (minutes 2)))
  #:emit (Alert [card p.card] [reason "unconfirmed"]))
```

**Runs of declines under each skip strategy** (`SKIP.*`):

```racket
(define-syntax-rule (decline-runs name skip)
  (define-pattern name #:from activity #:key card #:within (minutes 10) #:skip skip
    (kleene+ (d : Decline)) #:where (>= (count d) 2)))
(decline-runs runs-no-skip no-skip)          ; every run: 11 matches
(decline-runs runs-to-next to-next)          ; one per initiator
(decline-runs runs-past-last past-last)      ; none overlapping
```

**A lock-out that times out, retained** (`OUT.timeout-partial`, `CON.retain`):

```racket
(define-pattern lockout #:from activity #:key card #:within (hours 1)
  #:selection next #:output first
  #:timeout-partial ((minutes 10) processing-time) #:retain (timeout)
  (kleene+ (d : Decline))
  #:where (>= (count d) 5))
```

**Trends as maximal runs** (`PT.trend.*`, `SEL.partition`, `OUT.maximal`):

```racket
(define-pattern heating #:from readings #:key device #:within (minutes 30)
  #:selection partition #:output maximal
  (kleene+ (t : Reading)) #:where (and (>= (count t) 4) (increasing t.value)))
```

**Readings during maintenance, as a log** (`PT.aperiodic-cumulative`):

```racket
(define-pattern maintenance-log #:from telemetry #:key device
  (aperiodic* (s : Maintenance (equal? status "start")) (r : Reading) (e : Maintenance (equal? status "end"))))
```

**Geofences and movement** (`CX.spatial`, `PT.spatial.moving`):

```racket
(define-stream at-depot (from pings) (context (near #:location depot #:radius 1)))
(define-pattern heading-home #:from pings #:key truck #:within (minutes 10) #:selection partition
  (kleene+ (p : Ping)) #:where (and (= (count p) 3) (moving-toward? p.location depot)))
```

**A pattern over derived events, an action, and a bounded loop**
(`DR.closure`, `DR.act`, `DR.cycle-guard`):

```racket
(define-pattern escalations #:from alerts #:key card #:within (hours 1)
  (and (a : Alert) (b : Alert)) #:where (string<? a.reason b.reason)
  #:emit (Escalation [card a.card] [reasons (list a.reason b.reason)]))
(act notify #:on escalations #:do (send-to-desk card) #:completion Notified)
(define-stream reviews #:recursive 4 (from escalations) (feedback reviews) (map [reviewed #t]))
```

## Reflex, a trigger engine written in occur

Spec §16 describes Reflex, a stateful trigger engine that turns data
arrivals into triggers. [`lib/reflex.rkt`](lib/reflex.rkt) writes it as a
small library in `#lang occur` with public forms only (a test checks that it
requires no private module): its four conditions are renamed core
conditions, and one form declares the whole program.

```racket
#lang occur
(require "lib/reflex.rkt")

(define-devices devices ["d1" watched])
(define-reflex #:arrivals arrivals #:devices devices
               #:contributions contributions #:triggers triggers)

(reflex-rule threshold-check
  #:condition (accumulate #:min-samples 3 #:min-span-s 0)   ; PT.accumulate over WN.since-last
  #:min-interval-s 60                                       ; OUT.rate-limit (60, defer)
  #:target "threshold-evaluator"
  #:applies-to ("d1"))
```

[`docs/reflex.md`](docs/reflex.md) maps every row of §16.6 to the form
that realizes it, and [`examples/reflex/`](examples/reflex/) reproduces the
acceptance traces R1–R6 exactly: reason, window, samples, windows, params,
and emission time.

## How it is tested

- **Tagged tests.** Every test names the spec IDs it covers,
  `(spec-test 'PT.sequence "…" body …)`, and
  [`tests/coverage.rkt`](tests/coverage.rkt) fails unless every ID in
  [`docs/coverage.md`](docs/coverage.md) is done, tested, and has an
  example.
- **Differential testing.** The incremental engine is checked against the
  reference oracle on random patterns and histories in any arrival order,
  under every selection strategy and output policy, at 1000 trials per
  property; so are the selection nesting, high-water-mark idempotence,
  rate limits that defer without losing data, rule convergence,
  checkpoint-and-restore, and partitioned runs.
- **Acceptance.** §15's traces A, A′, and B and §16.7's traces R1–R6 are
  reproduced exactly, through the surface language.

```sh
raco test -j 8 .                       # everything, 1000 trials per property
OCCUR_TRIALS=5000 raco test tests/     # more trials
```

## Repository layout

```
main.rkt                 the public runtime API (require occur)
lang/                    #lang occur
private/                 the runtime: plain Racket, contracts on every export
  pattern/               pattern AST, reference oracle, incremental engine, policies
syntax/                  the surface language: syntax-parse forms with compile-time checks
lib/reflex.rkt           Reflex as a library of occur forms
examples/                runnable programs, each with a test
tests/                   rackunit and rackcheck tests
scribblings/             the manual
docs/                    the specification, coverage, the Reflex mapping, decision records
```

Decision records in [`docs/decisions/`](docs/decisions/README.md) document
every choice the specification leaves open, from the time domain and
deterministic ids to the incremental engine, the policies, the rules
engine, checkpoint and replay, and the Reflex library.

## License

MIT; see [LICENSE](LICENSE).
