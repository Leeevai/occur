#lang racket/base
(require rackunit
         "util.rkt"
         "../private/time.rkt"
         "../private/event.rkt")

(spec-test 'ES.reference "a reference event points at a sub-stream interval with a summary"
  (define e (make-reference-event 'Written #:substream "s1" #:ts 10 #:te 14 #:n 5
                                  #:summary '((newest . 21.5)) #:payload '((owner . "d7"))))
  (check-true (reference-event? e))
  (define r (event-reference e))
  (check-equal? (data-ref-substream r) "s1")
  (check-equal? (data-ref-interval r) (interval 10 14))
  (check-equal? (hash-ref (data-ref-summary r) 'newest) 21.5)
  (check-equal? (event-attr e 'owner) "d7")
  (check-equal? (event-n e) 5)
  (check-false (reference-event? (make-event 'Plain '() #:at 1))))

(spec-test 'ES.reference "a consumer reads the values from a store over the event's interval"
  (define st (store-append (store-append empty-store "s1" '((9 . 20.0) (10 . 20.5) (12 . 21.0)))
                           "s1" '((14 . 21.5) (15 . 22.0))))
  (define e (make-reference-event 'Written #:substream "s1" #:ts 10 #:te 14 #:n 3))
  (check-equal? (store-read st (event-reference e)) '((10 . 20.5) (12 . 21.0) (14 . 21.5)))
  (check-equal? (store-read st (data-ref "missing" (interval 0 1) (hasheq))) '()))
