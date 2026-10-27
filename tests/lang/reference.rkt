#lang occur
;; split into reference events (ES.reference, ES.batch): one chunk per
;; sensor written, pointing at the sensor's samples over [first, last] with
;; the newest value as its summary.
(event-type Upload [device : String]
  [writes : (Listof (Record [sensor : String] [n : Integer] [first : Time] [last : Time] [newest : Real]))])
(event-type Chunk [device : String] [sensor : String] #:derived)

(source uploads #:types (Upload)
  #:from (events (Upload #:at 10 #:id "u1" [device "d1"]
                         [writes (list (hasheq 'sensor "s1" 'n 3 'first 1 'last 9 'newest 2.5)
                                       (hasheq 'sensor "s2" 'n 1 'first 4 'last 4 'newest 0.5))])))

(define-stream chunks (from uploads)
  (split Chunk #:for ([w writes]) [device device] [sensor (hash-ref w 'sensor)]
         #:ts (hash-ref w 'first) #:te (hash-ref w 'last) #:n (hash-ref w 'n)
         #:reference (hash-ref w 'sensor) #:summary ([newest (hash-ref w 'newest)])))

;; later stages read the summary through `ref`, without the samples
(define-stream high (from chunks) (filter (> (hash-ref (data-ref-summary ref) 'newest) 1)))
