#lang occur
;; Windows and contexts as stages (spec §7).
(event-type Swipe [card : String] [amount : Real] [kind : String])
(event-type Reading [device : String] [sensor : String] [kind : String] [location : Location])
(event-type Maintenance [status : String])

(define (sw t card [kind "buy"]) (Swipe #:at t [card card] [amount 10] [kind kind]))

(source swipes #:types (Swipe)
  #:from (events (sw 1 "c1") (sw 5 "c1") (sw 12 "c2") (sw 31 "c1" "open") (sw 33 "c1") (sw 35 "c1" "close")))

(define-stream per-10 (from swipes) (window (tumbling #:size 10)))
(define-stream every-5 (from swipes) (window (sliding #:size 10 #:slide 5)))
(define-stream sessions (from swipes) (key-by card) (window (session #:gap 10)))
(define-stream last-3 (from swipes) (window (count #:size 3 #:step 3)))
(define-stream since-zero (from swipes) (window (landmark #:from 0)))
(define-stream shifts (from swipes) (window (punctuated #:open (equal? kind "open") #:close (equal? kind "close"))))
(define-stream batches (from swipes) (window (since-last #:when (>= count 2))))
(define-stream empties (from swipes) (window (tumbling #:size 10) #:fire-empty))

(directory devices #:from (entries ["d1" active] ["d2" deprecated]))
(source readings #:types (Reading) #:key device #:directory devices
  #:from (events (Reading #:at 5 [device "d1"] [sensor "s1"] [kind "load"] [location (location 1 1)])
                 (Reading #:at 6 [device "d2"] [sensor "s9"] [kind "load"] [location (location 50 50)])))
(define-stream seen (from readings))
(define-stream activated (from readings) (activations))
(define-stream near-depot (from readings) (context (near #:location (location 0 0) #:radius 5)))
(define-stream by-hour (from swipes) (context (and (segment card) (sliding-interval #:size 3600 #:slide 3600))))

(source maintenance #:types (Maintenance)
  #:from (events (Maintenance #:at 10 [status "on"]) (Maintenance #:at 20 [status "off"])))
(define-stream during (from swipes) (context (state #:from maintenance #:active (equal? status "on"))))
