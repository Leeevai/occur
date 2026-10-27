#lang racket/base
;; The history of a stream (spec §2 ES.history). A program's run is a value:
;; run the IoT telemetry program, then read what one of its streams held by
;; a given event time. H_S(t) is every event of S that completely occurred
;; by t, whether or not it had arrived by then.
;;
;; Run it with `racket examples/history.rkt`.
(require occur
         (prefix-in iot: "iot-telemetry.rkt"))

(provide readings-by)

;; the ids of the readings that pump-a reported by minute m
(define (readings-by m)
  (define r (iot:occur-run))
  (for/list ([e (run-result-history r 'pump-a (minutes m))]) (event-id e)))

(module+ main
  (for ([m '(10 30 60)])
    (printf "pump-a's readings by minute ~a: ~a\n" m (readings-by m))))
