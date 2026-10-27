#lang racket/base
(require rackunit
         "util.rkt"
         "../private/event.rkt"
         "../private/dataflow.rkt"
         "../private/ops/stateless.rkt")

(define (ev id type t) (make-event type '() #:id id #:at t))

(spec-test 'OP.filter "selecting by type keeps the identical events of the chosen types"
  (define in (list (ev "a" 'Swipe 1) (ev "b" 'Verified 2) (ev "c" 'Swipe 3)))
  (define out (run-result-events (run-operator (select-types-operator '(Swipe)) (list in))))
  (check-equal? (map event-id out) '("a" "c"))
  (check-eq? (car out) (car in)))

(define (swipe id t amount) (make-event 'Swipe `((card . "c1") (amount . ,amount)) #:id id #:at t))
(define (run1 op events) (run-result-events (run-operator op (list events))))

(spec-test 'OP.filter "σ_p keeps exactly the events satisfying p, unchanged"
  (define in (list (swipe "a" 1 2) (swipe "b" 2 1500) (swipe "c" 3 999)))
  (define out (run1 (filter-operator (λ (e) (> (event-attr e 'amount) 1000))) in))
  (check-equal? (map event-id out) '("b"))
  (check-eq? (car out) (cadr in))
  (check-true (event-primitive? (car out))))

(spec-test 'OP.filter "derive-from builds C = {e} and inherits the interval and multiplicity"
  (define e (make-event 'R '((v . 1)) #:id "r" #:ts 2 #:te 5 #:n 4))
  (define d (derive-from e 'R2 '((w . 2)) #:src 'test))
  (check-equal? (map event-id (event-constituents d)) '("r"))
  (check-equal? (list (event-ts d) (event-te d) (event-n d) (event-src d)) '(2 5 4 test)))

(spec-test 'OP.project "π_A keeps only the attributes A and records C = {e}"
  (define e (make-event 'Swipe '((card . "c1") (amount . 5) (merchant . "m")) #:id "s" #:ts 1 #:te 2 #:n 3))
  (define out (run1 (project-operator '(card amount)) (list e)))
  (check-equal? (length out) 1)
  (define p (car out))
  (check-equal? (event-payload p) (hasheq 'card "c1" 'amount 5))
  (check-equal? (event-type p) 'Swipe)
  (check-equal? (map event-id (event-constituents p)) '("s"))
  (check-equal? (list (event-ts p) (event-te p) (event-n p)) '(1 2 3))
  (check-equal? (event-type (car (run1 (project-operator '(card) #:type 'CardOnly) (list e)))) 'CardOnly)
  (check-exn #rx"no such attribute" (λ () (run1 (project-operator '(zip)) (list e)))))

(spec-test 'OP.map "μ_f translates payloads, possibly into a new type, inheriting timestamps"
  (define r (make-event 'TempF '((sensor . "s1") (f . 212)) #:id "t" #:ts 10 #:te 12))
  (define to-c (map-operator (λ (e) (hasheq 'sensor (event-attr e 'sensor)
                                            'c (* 5/9 (- (event-attr e 'f) 32))))
                             #:type 'TempC))
  (define out (car (run1 to-c (list r))))
  (check-equal? (event-type out) 'TempC)
  (check-equal? (event-attr out 'c) 100)
  (check-equal? (list (event-ts out) (event-te out)) '(10 12))
  (check-equal? (map event-id (event-constituents out)) '("t")))

(spec-test 'OP.map "timestamps and multiplicity can be set explicitly"
  (define r (make-event 'R '((at . 7)) #:id "r" #:ts 10 #:te 12))
  (define out (car (run1 (map-operator (λ (e) (event-payload e)) #:ts (λ (e) (event-attr e 'at))
                                       #:n (λ (e) 2))
                         (list r))))
  (check-equal? (list (event-ts out) (event-te out) (event-n out)) '(7 7 2)))

(spec-test 'OP.enrich "e ⊕ R(k(e)) adds the reference attributes keyed by k(e)"
  (define merchants (hash "m1" (hasheq 'category "grocery") "m2" '((category . "travel"))))
  (define in (list (make-event 'Swipe '((merchant . "m1") (amount . 5)) #:id "a" #:at 1)
                   (make-event 'Swipe '((merchant . "m9") (amount . 7)) #:id "b" #:at 2)))
  (define op (enrich-operator (table-lookup merchants) #:key (λ (e) (event-attr e 'merchant))))
  (define out (run1 op in))
  (check-equal? (event-attr (car out) 'category) "grocery")
  (check-equal? (event-attr (car out) 'amount) 5)
  (check-equal? (map event-id (event-constituents (car out))) '("a"))
  ;; a key with no reference row passes unchanged by default
  (check-false (event-has-attr? (cadr out) 'category))
  (define r (run-operator (enrich-operator (table-lookup merchants) #:key (λ (e) (event-attr e 'merchant))
                                           #:missing 'drop)
                          (list in)))
  (check-equal? (length (run-result-events r)) 1)
  (check-equal? (run-result-counter r 'op 'unenriched) 1))

(spec-test '(OP.split ES.batch) "a write covering several sensors splits into one batch event per sensor"
  (define write (make-event 'Write `((sensors . (,(hasheq 'id "s1" 'n 3 'first 0 'last 2)
                                                 ,(hasheq 'id "s2" 'n 5 'first 1 'last 9))))
                            #:id "w" #:ts 0 #:te 9))
  (define op (split-operator
              (λ (e) (for/list ([s (event-attr e 'sensors)])
                       (make-batch-event 'Reading `((sensor . ,(hash-ref s 'id)))
                                         #:ts (hash-ref s 'first) #:te (hash-ref s 'last)
                                         #:n (hash-ref s 'n))))))
  (define out (run1 op (list write)))
  (check-equal? (map (λ (e) (event-attr e 'sensor)) out) '("s1" "s2"))
  (check-equal? (map event-n out) '(3 5))
  (check-equal? (map (λ (e) (list (event-ts e) (event-te e))) out) '((0 2) (1 9)))
  (check-true (andmap (λ (e) (equal? (map event-id (event-constituents e)) '("w"))) out))
  (check-false (equal? (event-id (car out)) (event-id (cadr out)))))
