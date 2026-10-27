#lang occur
;; Derivation forms (spec §13).
(event-type Reading [dev : String] [v : Real])
(event-type Pair [dev : String] #:derived)

(source readings #:types (Reading) #:key dev
  #:from (events (Reading #:at 10 #:id "r1" [dev "d1"] [v 1]) (Reading #:at 20 #:id "r2" [dev "d1"] [v 2])
                 (Reading #:at 30 #:id "r3" [dev "d2"] [v 5]) (Reading #:at 75 #:id "r4" [dev "d1"] [v 3])))

;; a running total per device, exported every minute
(define-view totals #:from readings #:init 0 #:update (+ state v) #:export-every 60)

;; health by arrival age and data age, exported every minute
(define-view health #:from readings #:init #f #:update state
  #:project (classify-health #:now now #:last-arrival last-arrival #:newest newest
                             #:live 30 #:silent 120 #:stale 60)
  #:export-every 60)

;; derived events re-enter: pairs, then a stream over the pairs (DR.closure)
(define-pattern pairs #:from readings #:within 100 (seq (a : Reading) (b : Reading))
  #:emit (Pair [dev a.dev]))
(define-stream pair-windows (from pairs) (window (tumbling #:size 100)))
(define-stream compact (from pairs) (summarize))
(source redelivered #:types (Reading)
  #:from (events (Reading #:at 10 #:id "x1" [dev "d1"] [v 1])
                 (arrival 12 (Reading #:at 10 #:id "x1-again" [dev "d1"] [v 1]))
                 (Reading #:at 13 #:id "x2" [dev "d1"] [v 2])))
(define-stream once (from redelivered) (dedupe-outputs))

(define sent (box '()))
(act alert #:on readings #:do (begin (set-box! sent (cons v (unbox sent))) 'ok) #:completion Alerted)

;; a bounded feedback loop: each reading is re-fed once more (DR.cycle-guard)
(define-stream echo #:recursive 2 (from readings) (feedback echo) (map [v (+ v 100)]))

(provide sent)
