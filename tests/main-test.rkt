#lang racket/base
;; The public API is reachable through (require occur) alone.
(require rackunit occur)

(test-case "the event space is available from the public API"
  (define e (make-batch-event 'Reading '() #:ts 95 #:te 105 #:n 10))
  (check-equal? (batch-share e 100) 5)
  (check-true (precedes? (make-event 'X '() #:at 1) e))
  (check-equal? (minutes 1) 60)
  (check-true (event-stream? (make-event-stream '(Reading) (list e)))))
