#lang occur
;; Single-event operations as define-stream stages (spec §4).
(event-type Swipe [card : String] [amount : Real] [merchant : String])
(event-type Charge [card : String] [cents : Integer] #:derived)
(event-type Write [device : String] [sensors : (Listof Any)])
(event-type Reading [owner : String] [sensor : String] #:derived)

(define merchants (hash "m1" (hasheq 'category "grocery")))

(source feed #:types (Swipe Write)
  #:from (events (Swipe #:at 1 #:id "a" [card "c1"] [amount 1500] [merchant "m1"])
                 (Swipe #:at 2 #:id "b" [card "c2"] [amount 3] [merchant "m9"])
                 (Write #:ts 0 #:te 9 #:id "w" [device "d1"]
                        [sensors (list (hasheq 'id "s1" 'n 3 'first 0 'last 2)
                                       (hasheq 'id "s2" 'n 5 'first 1 'last 9))])))

(define-stream big (from feed Swipe) (filter (> amount 1000)))
(define-stream slim (from feed Swipe) (project card amount))
(define-stream charges (from feed Swipe) (map Charge [card card] [cents (* 100 amount)]))
(define-stream doubled (from feed Swipe) (map [amount (* 2 amount)] [flag #t]))
(define-stream categorized (from feed Swipe)
  (enrich #:key merchant #:with merchants #:adds ([category : String]) #:missing drop))
(define-stream readings (from feed Write)
  (split Reading #:for ([s (in-list sensors)])
         #:ts (hash-ref s 'first) #:te (hash-ref s 'last) #:n (hash-ref s 'n)
         [owner device] [sensor (hash-ref s 'id)]))
(define-stream by-size (from feed Swipe)
  (route #:mode exclusive [large (>= amount 100)] #:otherwise small))
