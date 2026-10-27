#lang racket/base
;; examples/iot-telemetry.rkt: two pumps' temperatures with maintenance, a
;; door, and operating modes.
(require rackunit
         racket/list
         "util.rkt"
         "../main.rkt"
         (prefix-in iot: "../examples/iot-telemetry.rkt"))

(define r (iot:occur-run))
(define (outs node) (filter (λ (o) (eq? (output-node o) node)) (run-result-outputs r)))
(define (events node) (filter event? (map output-item (outs node))))
(define (ids node) (map event-id (events node)))
(define (m n) (minutes n))
(define (interval-of e) (list (event-ts e) (event-te e)))
;; an event by its device and minute, as the example names its readings
(define (who e) (format "~a@~a" (event-attr e 'device) (/ (event-te e) 60)))

;; --- windows -----------------------------------------------------------

(spec-test '(WN.landmark AG.max AG.count) "the hottest reading of each pump since midnight, after every reading"
  (define (last-of pump) (last (filter (λ (e) (equal? (event-attr e 'key) pump)) (events 'hottest-so-far))))
  (check-equal? (map (λ (e) (list (event-ts e) (event-attr e 'hottest) (event-attr e 'readings)))
                     (list (last-of "pump-a") (last-of "pump-b")))
                '((0 40 15) (0 52 3))))

(spec-test 'WN.punctuated "the readings taken while the door was open"
  (define w (car (events 'door-open)))
  (check-equal? (list (interval-of w) (event-attr w 'reason)) (list (list (m 51) (m 72)) 'closed))
  (check-equal? (map event-id (event-constituents w))
                '("door-open" "pump-a@55" "pump-a@60" "pump-a@65" "pump-a@70" "door-closed")))

;; --- contexts ----------------------------------------------------------

(define (tagged node) (for/list ([e (events node)]) (list (who e) (event-attr e 'context))))

(spec-test 'CX.temporal "a fixed interval, an event interval, sliding intervals, and a sliding event interval"
  (check-equal? (map who (events 'second-half-hour))
                '("pump-a@30" "pump-b@30" "pump-a@35" "pump-a@40" "pump-a@45" "pump-a@50" "pump-a@55"))
  (check-equal? (map who (events 'during-maintenance))
                '("pump-a@32" "pump-a@35" "pump-a@40" "pump-a@45" "pump-a@48"))   ; start, readings, end
  ;; every reading lies in two of the half hours that start every quarter hour
  (check-equal? (length (events 'half-hours)) (* 2 (length (events 'readings))))
  ;; the ten minutes [60, 70) after 40 at minute 60, then those after 38 at 70
  (check-equal? (tagged 'after-hot)
                '(("pump-a@60" (sliding-event "pump-a@60")) ("pump-a@65" (sliding-event "pump-a@60"))
                  ("pump-a@70" (sliding-event "pump-a@70")))))

(spec-test 'CX.segmentation "one context per pump and kind"
  (check-equal? (remove-duplicates (map (λ (e) (event-attr e 'context)) (events 'per-pump)))
                '((segment "pump-a" "temperature") (segment "pump-b" "temperature"))))

(spec-test 'CX.state "pump-a's readings only while it runs"
  (check-equal? (map cadr (tagged 'while-running))
                (append (make-list 8 '(state "run-1")) (make-list 3 '(state "run-2")))))

(spec-test 'CX.composite "per pump and per half hour"
  (check-equal? (remove-duplicates (map (λ (e) (event-attr e 'context)) (events 'pump-half-hours)))
                (list (list '(segment "pump-a") (list 'sliding 0 (m 30))) (list '(segment "pump-b") (list 'sliding 0 (m 30)))
                      (list '(segment "pump-a") (list 'sliding (m 30) (m 60))) (list '(segment "pump-b") (list 'sliding (m 30) (m 60)))
                      (list '(segment "pump-a") (list 'sliding (m 60) (m 90))) (list '(segment "pump-b") (list 'sliding (m 60) (m 90))))))

;; --- patterns ------------------------------------------------------------

(define (episodes node) (map interval-of (events node)))

(spec-test '(PT.trend.increasing PT.trend.non-decreasing PT.trend.decreasing PT.trend.non-increasing
             PT.trend.stable SEL.partition OUT.maximal)
           "the trends of pump-a's temperature, as maximal runs of consecutive readings"
  (check-equal? (episodes 'heating) (list (list 0 (m 20))))
  (check-equal? (episodes 'warming) (list (list 0 (m 25))))
  (check-equal? (episodes 'cooling) (list (list (m 25) (m 40))))
  (check-equal? (episodes 'settling) (list (list (m 20) (m 45))))
  (check-equal? (episodes 'steady) (list (list (m 40) (m 55)))))

(spec-test 'PT.trend.mixed "runs of four readings within a quarter hour that go up and down"
  (check-equal? (episodes 'swinging)
                (for/list ([t '(15 35 40 45 50 55)]) (list (m t) (m (+ t 15))))))

(spec-test 'PT.relative "a reading at least as hot as the two before and the two after it"
  ;; the peak is the third of the five readings: two before, two after
  (check-equal? (map (λ (e) (event-id (list-ref (event-constituents e) 2))) (events 'peak))
                '("pump-a@20" "pump-a@25" "pump-a@60")))

(spec-test '(PT.aperiodic PT.aperiodic-cumulative) "the readings during maintenance, one by one and as a log"
  (check-equal? (map (λ (e) (map event-id (event-constituents e))) (events 'maintenance-readings))
                '(("m-start" "pump-a@35") ("m-start" "pump-a@40") ("m-start" "pump-a@45")))
  (check-equal? (map (λ (e) (map event-id (event-constituents e))) (events 'maintenance-log))
                '(("m-start" "pump-a@35" "pump-a@40" "pump-a@45" "m-end"))))

(spec-test '(PT.periodic PT.periodic-cumulative) "a reminder every five minutes while the door is open"
  (check-equal? (map event-te (events 'door-reminders)) (list (m 56) (m 61) (m 66) (m 71)))
  (check-equal? (map (λ (e) (event-attr e 'ticks)) (events 'door-report)) (list (list (m 56) (m 61) (m 66) (m 71)))))

(spec-test '(TM.clock PT.absence TM.processing-time) "silence on the hybrid clock: twenty minutes without a reading"
  (check-equal? (sort (for/list ([o (outs 'silent)])
                        (list (output-at o) (event-id (car (event-constituents (output-item o))))))
                      < #:key car)
                (list (list (m 25) "pump-b@0") (list (m 51) "pump-b@30") (list (m 95) "pump-a@70") (list (m 95) "pump-b@60"))))

(require (only-in "../examples/history.rkt" readings-by))

(spec-test 'ES.history "the history of pump-a's readings at a given time"
  (check-equal? (readings-by 10) '("pump-a@0" "pump-a@5" "pump-a@10"))
  (check-equal? (length (readings-by 60)) 13))
