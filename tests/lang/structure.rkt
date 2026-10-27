#lang occur
;; Stream-structure stages and source keying (spec §5).
(event-type Reading [owner : String] [sender : String] [sensor : String] [kind : String] [v : Real])
(event-type Ping [id : String])

(define (r owner sensor ts te n) (Reading #:ts ts #:te te #:n n [owner owner] [sender "gw"] [sensor sensor] [kind "load"] [v 1]))

;; R2 through a source declared with high-water-mark merging
(source arrivals #:types (Reading)
  #:key owner #:substream sensor #:kind kind #:merge high-water-mark
  #:from (events (arrival 1 (r "d1" "s1" 90 100 10)) (arrival 2 (r "d1" "s1" 95 105 10))
                 (arrival 3 (r "d1" "s1" 95 105 10)) (arrival 4 (r "d1" "s1" 80 90 10))
                 (arrival 5 (r "d2" "s1" 80 90 4))))
(define-stream contributions (from arrivals))

(source pings #:types (Ping)
  #:from (events (Ping #:at 1 [id "a"]) (Ping #:at 2 [id "a"]) (Ping #:at 3 [id "b"])
                 (Ping #:at 40 [id "a"])))
(source more #:types (Ping) #:from (events (Ping #:at 5 [id "z"])))

(define-stream unique (from pings) (dedupe #:key id #:horizon 10))
(define-stream both (merge pings more))
(define-stream by-id (from pings) (key-by id) (throttle #:every 2))
(define-stream sampled (from pings) (throttle #:rate 1 #:per 10))
(define-stream ordered (from pings) (reorder))
