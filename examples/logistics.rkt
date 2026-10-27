#lang occur
;; Logistics geofencing: two trucks leave a depot for their customers. GPS
;; pings come every five minutes; locations are in kilometres and times in
;; minutes past midnight.
;;
;; Run it with `racket examples/logistics.rkt`; tests/logistics-test.rkt
;; checks it.

(event-type Ping [truck : String] [location : Location])
(event-type Incident [truck : String] [location : Location])
(event-type Order [customer : String])
(event-type Stop [truck : String] [customer : String] [cause : String])
(event-type Trip [truck : String] #:derived)

(define depot (location 0 0))
(define customers (hash "acme" (location 10 0) "bolt" (location 0 8)))

(define (ping t truck x y)
  (Ping #:at (minutes t) #:id (format "~a@~a" truck t) [truck truck] [location (location x y)]))

;; t1 drives east to acme and back; t2 drives north to bolt, then breaks
;; down on the way back
(source gps #:types (Ping Incident) #:watermark ascending
  #:from (events (ping 0 "t1" 0 0) (ping 0 "t2" 0 0)
                 (ping 5 "t1" 2.5 0) (ping 5 "t2" 0 2)
                 (ping 10 "t1" 5 0) (ping 10 "t2" 0 4)
                 (ping 15 "t1" 7.5 0) (ping 15 "t2" 0 6)
                 (ping 20 "t1" 10 0) (ping 20 "t2" 0 8)
                 (ping 25 "t1" 10 0) (ping 25 "t2" 0 8)
                 (ping 30 "t1" 7.5 0) (ping 30 "t2" 1 7)
                 (Incident #:at (minutes 32) #:id "breakdown" [truck "t2"] [location (location 1 7)])
                 (ping 35 "t1" 5 0) (ping 35 "t2" 1 7)
                 (ping 40 "t1" 2.5 0) (ping 40 "t2" 1 7)
                 (ping 45 "t1" 0 0)))
(define-stream pings (from gps Ping))

;; ---------------------------------------------------------------------------
;; Geofences (§7 CX.spatial)

;; within a kilometre of the depot
(define-stream at-depot (from pings) (context (near #:location depot #:radius 1)))
;; within a kilometre of a customer
(define-stream at-customer (from pings)
  (context (near-entity #:locate (λ (c) (hash-ref customers c #f)) #:entities (λ () (hash-keys customers))
                        #:radius 1)))
;; within two kilometres of an incident, for the ten minutes after it
(define-stream near-incident (from gps)
  (context (near-event #:anchor (equal? (event-type-of it) 'Incident) #:radius 2 #:duration (minutes 10))))

;; ---------------------------------------------------------------------------
;; Movement (§11.4 PT.spatial.*)

;; PT.spatial.distance: a truck that has not moved 100 metres in ten minutes
(define-pattern stuck #:from pings #:key truck #:within (minutes 10) #:selection partition
  (seq (a : Ping) (b : Ping) (c : Ping))
  #:where (and (within-distance? a.location c.location 0.1) (within-distance? b.location c.location 0.1)))

;; and one more than 9 kilometres from the depot, at its farthest
(define-pattern far-out #:from pings #:key truck
  (p : Ping (beyond-distance? location depot 9)))

;; PT.spatial.moving: three pings in a row heading back to the depot, and
;; three in a row in a constant direction
(define-pattern heading-home #:from pings #:key truck #:within (minutes 10) #:selection partition
  (kleene+ (p : Ping))
  #:where (and (= (count p) 3) (moving-toward? p.location depot)))
(define-pattern heading-out #:from pings #:key truck #:within (minutes 10) #:selection partition
  (kleene+ (p : Ping))
  #:where (and (= (count p) 3) (moving-away? p.location depot) (constant-direction? p.location)))

;; Trips (DR.derive, ES.membership): from leaving the depot to returning to
;; it; each trip is made of its pings, its provenance
(define-pattern trips #:from pings #:key truck #:within (hours 2) #:selection partition #:output first
  (seq (start : Ping (within-distance? location depot 0.5))
       (kleene+ (away : Ping (beyond-distance? location depot 0.5)))
       (end : Ping (within-distance? location depot 0.5)))
  #:emit (Trip [truck start.truck]))
(define-stream trip-lengths (from trips) (map [pings (length (provenance it))]))

;; ---------------------------------------------------------------------------
;; Delivery windows (§10)

;; Every order was promised between 01:40 and 03:20; the stops, as
;; intervals from arrival to departure, relate to that window in each of
;; Allen's thirteen ways.
(define promised (interval (minutes 100) (minutes 200)))

(define (stop order customer from to)
  (Stop #:ts (minutes from) #:te (minutes to) #:id (string-append "stop-" order)
        [truck "t1"] [customer customer] [cause order]))

(source orders #:types (Order)
  #:from (data (for/list ([i (in-range 1 14)])
                 (Order #:at (minutes 10) #:id (format "o~a" i) [customer "acme"]))))
(source stops #:types (Stop)
  #:from (events (stop "o1" "acme" 20 60)        ; before
                 (stop "o2" "acme" 60 100)       ; meets
                 (stop "o3" "acme" 80 150)       ; overlaps
                 (stop "o4" "acme" 100 150)      ; starts
                 (stop "o5" "acme" 120 180)      ; during
                 (stop "o6" "acme" 150 200)      ; finishes
                 (stop "o7" "acme" 100 200)      ; equals
                 (stop "o8" "acme" 80 200)       ; finished-by
                 (stop "o9" "acme" 80 220)       ; contains
                 (stop "o10" "acme" 100 220)     ; started-by
                 (stop "o11" "acme" 150 250)     ; overlapped-by
                 (stop "o12" "acme" 200 240)     ; met-by
                 (stop "o13" "acme" 220 260)))   ; after

;; AL.*: how each stop relates to its promised window
(define-stream punctuality (from stops) (map [relation (allen-relation it promised)]))
;; on time when the stop lies within the window (during, starts, finishes, or equals)
(define-stream on-time (from stops)
  (filter (or (during? it promised) (starts? it promised) (finishes? it promised) (equals? it promised))))
(define-stream early (from stops) (filter (or (before? it promised) (meets? it promised))))
(define-stream late (from stops) (filter (or (after? it promised) (met-by? it promised))))

;; ES.causality, ES.precedes: each stop was caused by its order (recorded in
;; its `cause`), and the order precedes it
(define-stream work (merge orders stops))
(define-pattern fulfilled #:from work
  (and (o : Order) (s : Stop))
  #:where (and (causes? (causality-from-events (list o s)) o s) (precedes? o s)))
