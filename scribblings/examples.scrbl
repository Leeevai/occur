#lang scribble/manual
@(require (for-label racket/base occur))

@title[#:tag "examples"]{Examples}

The @filepath{examples} directory holds complete @hash-lang[]
@racketmodname[occur] programs. Run one with @exec{racket
examples/card-testing.rkt}; each output line shows the processing time, the
node that produced it, and the event.

@section[#:tag "card-testing"]{Card testing (spec §15)}

@filepath{examples/card-testing.rkt} detects a tiny charge followed by
escalating large charges with no customer verification in between, per
card, within ten minutes. The pattern is written once, as a macro over the
selection strategy and the output policy:

@codeblock|{
(define-syntax-rule (card-testing name source selection output)
  (define-pattern name #:from source #:within (minutes 10)
    #:selection selection #:consumption unrestricted #:output output
    (seq (a : Swipe (< amount 5))
         (absent Verified)
         (kleene+ (b : Swipe (> amount 500))))
    #:where (increasing b.amount)
    #:emit (SuspectedFraud [card a.card] [n (count b)] [total (sum b.amount)])))
}|

The example instantiates it for every cell of trace A's table, and runs the
two negation scopes of trace A′ and the four strategies of trace B:

@tabular[#:style 'boxed
 (list (list @bold{Selection} @bold{every} @bold{maximal})
       (list "strict" "{e1, e2}" "{e1, e2}")
       (list "partition" "{e1, e2}" "{e1, e2}")
       (list "next" "{e1, e2}, {e1, e2, e4}" "{e1, e2, e4}")
       (list "any" "{e1, e2}, {e1, e4}, {e1, e2, e4}" "{e1, e2, e4}"))]

The maximal match derives @tt{SuspectedFraud(card = c1, n = 2, total =
1500)} over minutes 1 to 4, with constituents e1, e2, and e4, once the
watermark passes 11 minutes. With @tt{Verified(c1)} at 3.5 minutes,
@racket[(absent Verified)] between @racket[a] and @racket[b] blocks
@tt{{e1, e4}} only, while @racket[(unless P Verified)] over the whole pattern
keeps only @tt{{e1, e2}}. In trace B the verification between the swipes
prevents every match. @filepath{tests/card-testing-test.rkt} checks all of
this, on the incremental engine and on the reference oracle.

@section[#:tag "fraud"]{Payment fraud}

@filepath{examples/fraud.rkt} follows card payments from two channels.
Ingestion merges them, drops redelivered webhooks, puts late payments on a
side stream, samples one in three for audit, enriches, splits baskets, and
settles each payment after a delay; a chargeback retracts a payment and
everything derived from it, including the settlement it was waiting for.
Spending is windowed per card (sliding, session, and count windows with
every kind of aggregate), joined with logins, declines, the card table, and
limits as of each payment, and matched by patterns: an unconfirmed large
payment, a far login, a burst of declines reported once, a spree over a
threshold, the four consumption policies, first and last output, the five
skip strategies, immediate evaluation, and a retained lock-out timeout.
Alerts are events too, so a pattern over them escalates a card, an action
notifies the fraud desk, and a review loop is stopped by the cycle guard.

@section[#:tag "iot"]{IoT telemetry}

@filepath{examples/iot-telemetry.rkt} watches two pumps' temperatures with
maintenance, a door, and operating modes: the hottest reading since
midnight, what the sensors saw while the door was open, every kind of
context, the six trends as maximal runs, peaks, the readings taken during
maintenance one by one and as a log, reminders while the door is open, and
silence on the hybrid clock. @filepath{examples/history.rkt} runs it and
reads a stream's history at a given time.

@section[#:tag "logistics"]{Logistics geofencing}

@filepath{examples/logistics.rkt} follows two trucks' GPS pings: geofences
at the depot, at customers, and around an incident; a stuck truck, a truck
far out, trucks heading out and home; trips and their provenance; stops
related to their promised delivery window in each of Allen's thirteen ways;
and each stop's recorded cause, its order.

@section[#:tag "reflex-examples"]{Reflex}

@filepath{examples/reflex/} holds the acceptance traces R1 to R6 of spec
§16.7 as programs on @filepath{lib/reflex.rkt} (see @secref["reflex"]), a
fleet that runs on arrivals and rules as data, fleet health, recovery after
a restart and a cold start, and a rule source that converges.
