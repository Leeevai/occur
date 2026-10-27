#lang occur
;; IoT telemetry: two pumps reporting temperature, with maintenance, a door,
;; and operating modes. Times are minutes past midnight.
;;
;; Run it with `racket examples/iot-telemetry.rkt`; tests/iot-test.rkt
;; checks it.
(require racket/list)

(event-type Reading [device : String] [sensor : String] [kind : String] [value : Real])
(event-type Maintenance [device : String] [status : String])
(event-type Door [device : String] [state : String])
(event-type Mode [device : String] [mode : String])

(define (temp t device value)
  (Reading #:at (minutes t) #:id (format "~a@~a" device t) [device device] [sensor (string-append device "-t")]
           [kind "temperature"] [value value]))

;; pump-a heats up, levels off, cools during maintenance, holds steady,
;; then swings while its door is open; pump-b runs steadily
(source telemetry #:types (Reading Maintenance Door Mode) #:watermark ascending
  #:from (events (Mode #:at 0 #:id "run-1" [device "pump-a"] [mode "run"])
                 (temp 0 "pump-a" 20) (temp 0 "pump-b" 50)
                 (temp 5 "pump-a" 22) (temp 10 "pump-a" 25) (temp 15 "pump-a" 29) (temp 20 "pump-a" 34)
                 (temp 25 "pump-a" 34) (temp 30 "pump-a" 33) (temp 30 "pump-b" 52)
                 (Maintenance #:at (minutes 32) #:id "m-start" [device "pump-a"] [status "start"])
                 (temp 35 "pump-a" 30)
                 (Mode #:at (minutes 38) #:id "idle-1" [device "pump-a"] [mode "idle"])
                 (temp 40 "pump-a" 26) (temp 45 "pump-a" 26)
                 (Maintenance #:at (minutes 48) #:id "m-end" [device "pump-a"] [status "end"])
                 (temp 50 "pump-a" 26.2)
                 (Door #:at (minutes 51) #:id "door-open" [device "pump-a"] [state "open"])
                 (temp 55 "pump-a" 25.9)
                 (Mode #:at (minutes 58) #:id "run-2" [device "pump-a"] [mode "run"])
                 (temp 60 "pump-a" 40) (temp 60 "pump-b" 51)
                 (temp 65 "pump-a" 22) (temp 70 "pump-a" 38)
                 (Door #:at (minutes 72) #:id "door-closed" [device "pump-a"] [state "closed"])
                 ;; the last word from the control room
                 (Mode #:at (minutes 95) #:id "idle-2" [device "pump-a"] [mode "idle"])))

(define-stream readings (from telemetry Reading))
(define-stream modes (from telemetry Mode))
(define-stream pump-a (from readings) (filter (equal? device "pump-a")))

;; ---------------------------------------------------------------------------
;; Windows (§7)

;; WN.landmark: the hottest reading of each pump since midnight
(define-stream hottest-so-far (from readings)
  (key-by device)
  (window (landmark #:from 0))
  (aggregate [hottest (max value)] [readings (count)]))

;; WN.punctuated: what a pump's sensors saw while its door was open
(define-stream door-open (from telemetry Reading Door)
  (key-by device)
  (window (punctuated #:open (equal? state "open") #:close (equal? state "closed"))))

;; ---------------------------------------------------------------------------
;; Contexts (§7)

;; CX.temporal: a fixed interval (the second half hour), an event interval
;; (from maintenance start to its end), a sliding interval, and a sliding
;; event interval (the ten minutes after a reading over 35)
(define-stream second-half-hour (from readings) (context (fixed-interval #:from (minutes 30) #:to (minutes 60))))
(define-stream during-maintenance (from telemetry Reading Maintenance)
  (context (event-interval #:open (equal? status "start") #:close (equal? status "end"))))
(define-stream half-hours (from readings) (context (sliding-interval #:size (minutes 30) #:slide (minutes 15))))
(define-stream after-hot (from pump-a) (context (sliding-event-interval #:open (> value 35) #:duration (minutes 10))))

;; CX.segmentation: one context per pump and sensor kind
(define-stream per-pump (from readings) (context (segment device kind)))

;; CX.state: pump-a's readings only while it is running
(define-stream while-running (from pump-a) (context (state #:from modes #:active (equal? mode "run"))))

;; CX.composite: per pump × per half hour
(define-stream pump-half-hours (from readings)
  (context (and (segment device) (sliding-interval #:size (minutes 30) #:slide (minutes 30)))))

;; ---------------------------------------------------------------------------
;; Patterns (§11)

(event-type Episode [device : String] [trend : String] #:derived)

;; Trends (PT.trend.*) over runs of a pump's consecutive readings: at
;; least four within half an hour, reported when no longer run contains them
(define-pattern heating #:from readings #:key device #:within (minutes 30) #:selection partition #:output maximal
  (kleene+ (t : Reading)) #:where (and (>= (count t) 4) (increasing t.value))         ; 20 → 34
  #:emit (Episode [device (first t.device)] [trend "increasing"]))
(define-pattern warming #:from readings #:key device #:within (minutes 30) #:selection partition #:output maximal
  (kleene+ (t : Reading)) #:where (and (>= (count t) 4) (non-decreasing t.value))     ; and 34 again
  #:emit (Episode [device (first t.device)] [trend "non-decreasing"]))
(define-pattern cooling #:from readings #:key device #:within (minutes 30) #:selection partition #:output maximal
  (kleene+ (t : Reading)) #:where (and (>= (count t) 4) (decreasing t.value))         ; 34 → 26
  #:emit (Episode [device (first t.device)] [trend "decreasing"]))
(define-pattern settling #:from readings #:key device #:within (minutes 30) #:selection partition #:output maximal
  (kleene+ (t : Reading)) #:where (and (>= (count t) 4) (non-increasing t.value))     ; 34, 34, 33, …
  #:emit (Episode [device (first t.device)] [trend "non-increasing"]))
(define-pattern steady #:from readings #:key device #:within (minutes 30) #:selection partition #:output maximal
  (kleene+ (t : Reading)) #:where (and (>= (count t) 4) (stable t.value 1/2))         ; within half a degree
  #:emit (Episode [device (first t.device)] [trend "stable"]))
(define-pattern swinging #:from readings #:key device #:within (minutes 15) #:selection partition #:output maximal
  (kleene+ (t : Reading)) #:where (and (>= (count t) 4) (mixed t.value))              ; up and down
  #:emit (Episode [device (first t.device)] [trend "mixed"]))

;; PT.relative: a reading hotter than or as hot as the readings around it
(define-pattern peak #:from readings #:key device #:within (minutes 20) #:selection partition
  (seq (kleene+ (b : Reading)) (p : Reading) (kleene+ (a : Reading)))
  #:where (and (= (count b) 2) (= (count a) 2) (relative-max? p.value (append b.value a.value)))
  #:emit (Episode [device p.device] [trend "peak"]))

;; PT.aperiodic, PT.aperiodic-cumulative: the readings taken during
;; maintenance, one by one and all together
(define-pattern maintenance-readings #:from telemetry #:key device
  (aperiodic (s : Maintenance (equal? status "start")) (r : Reading) (e : Maintenance (equal? status "end"))))
(define-pattern maintenance-log #:from telemetry #:key device
  (aperiodic* (s : Maintenance (equal? status "start")) (r : Reading) (e : Maintenance (equal? status "end"))))

;; PT.periodic, PT.periodic-cumulative: a reminder every five minutes while
;; the door is open, and all of them when it closes
(define-pattern door-reminders #:from telemetry #:key device
  (periodic (o : Door (equal? state "open")) (minutes 5) (Door (equal? state "closed"))))
(define-pattern door-report #:from telemetry #:key device
  (periodic* (o : Door (equal? state "open")) (minutes 5) (c : Door (equal? state "closed"))))

;; TM.clock: a pump that has sent nothing for twenty minutes, by the hybrid
;; clock (its newest reading's time against the processing clock)
(define-pattern silent #:from readings #:key device #:clock hybrid
  (seq (r : Reading) (absent Reading #:within (minutes 20))))

