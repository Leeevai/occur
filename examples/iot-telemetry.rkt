#lang occur
;; IoT telemetry: two pumps reporting temperature, with maintenance, a door,
;; and operating modes. Times are minutes past midnight.
;;
;; Run it with `racket examples/iot-telemetry.rkt`; tests/iot-test.rkt
;; checks it.

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

;; WN.punctuated: what the sensors saw while the door was open
(define-stream door-open (from telemetry Reading Door)
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
