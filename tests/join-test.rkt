#lang racket/base
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/event.rkt"
         "../private/operator.rkt"
         "../private/dataflow.rkt"
         "../private/window.rkt"
         "../private/join.rkt")

(define (order id t k amt) (make-event 'Order `((k . ,k) (amount . ,amt)) #:id id #:at t))
(define (pay id t k amt) (make-event 'Payment `((k . ,k) (paid . ,amt)) #:id id #:at t))
(define key (λ (e) (event-attr e 'k)))
(define always (λ (s r) #t))

(define (join-run op steps #:final? [final? #t])
  (define s (make-source 's))
  (define r (make-source 'r))
  (define n (make-node 'j op (list s r) #:sink? #t))
  (run (make-program (list n)) steps #:final-watermark? final?))
(define (pairs res)
  (for/list ([e (run-result-events res)]) (map event-id (event-constituents e))))

(spec-test 'JN.window "s ⋈ r when θ holds and both lie in the same window; each pair once"
  (define op (window-join-operator (tumbling 10) (λ (s r) (= (event-attr s 'amount) (event-attr r 'paid))) #:key key))
  (define res (join-run op (list (arrive 1 's (order "o1" 1 "a" 5)) (arrive 2 'r (pay "p1" 3 "a" 5))
                                 (arrive 3 'r (pay "p2" 4 "a" 6)) (arrive 4 'r (pay "p3" 12 "a" 5))
                                 (arrive 5 'r (pay "p4" 5 "b" 5)))))
  (check-equal? (pairs res) '(("o1" "p1")))
  (define j (car (run-result-events res)))
  (check-equal? (list (event-attr j 'amount) (event-attr j 'paid) (event-ts j) (event-te j)) '(5 5 1 3))
  ;; sliding windows share pairs, which are still emitted once
  (define sl (window-join-operator (sliding 10 5) always #:key key))
  (check-equal? (pairs (join-run sl (list (arrive 1 's (order "o1" 6 "a" 1)) (arrive 2 'r (pay "p1" 7 "a" 1)))))
                '(("o1" "p1"))))

(spec-test 'JN.interval "s ⋈ r when t(r) ∈ [t(s) − Δ₁, t(s) + Δ₂]"
  (define op (interval-join-operator 2 5 always #:key key))
  (define res (join-run op (list (arrive 1 'r (pay "early" 7 "a" 1)) (arrive 2 's (order "o1" 10 "a" 1))
                                 (arrive 3 'r (pay "in" 15 "a" 1)) (arrive 4 'r (pay "late" 16 "a" 1))
                                 (arrive 5 'r (pay "too-early" 2 "a" 1)))))
  (check-equal? (pairs res) '(("o1" "in"))))

(spec-test 'JN.outer "outer joins emit the unmatched side with nulls once its window closes"
  (define (go outer)
    (define op (window-join-operator (tumbling 10) always #:key key #:outer outer
                                     #:left-attributes '(k amount) #:right-attributes '(k paid)))
    (join-run op (list (arrive 1 's (order "o1" 1 "a" 5)) (arrive 2 's (order "o2" 2 "b" 7))
                       (arrive 3 'r (pay "p1" 3 "a" 5)) (arrive 4 'r (pay "p2" 4 "c" 9))
                       (watermark-to 's 10) (watermark-to 'r 10))
              #:final? #f))
  (check-equal? (pairs (go #f)) '(("o1" "p1")))
  (define left (go 'left))
  (check-equal? (pairs left) '(("o1" "p1") ("o2")))
  (check-equal? (event-attr (second (run-result-events left)) 'paid) #f)
  (check-equal? (pairs (go 'right)) '(("o1" "p1") ("p2")))
  (check-equal? (pairs (go 'full)) '(("o1" "p1") ("o2") ("p2")))
  ;; the outer results appear only when the watermark closes the window
  (check-equal? (map output-at (run-result-outputs (go 'left))) '(3 4)))

(spec-test 'JN.outer "interval outer joins emit once the match range has passed"
  (define op (interval-join-operator 0 5 always #:key key #:outer 'left #:right-attributes '(paid)))
  (define res (join-run op (list (arrive 1 's (order "o1" 10 "a" 1)) (watermark-to 's 20) (watermark-to 'r 20))
                        #:final? #f))
  (check-equal? (pairs res) '(("o1")))
  (check-equal? (event-attr (car (run-result-events res)) 'paid) #f))

(define gen:sides
  (gen:let ([ls (gen:list (gen:tuple (gen:integer-in 0 30) (gen:one-of '("a" "b"))) #:max-length 6)]
            [rs (gen:list (gen:tuple (gen:integer-in 0 30) (gen:one-of '("a" "b"))) #:max-length 6)])
    (cons (for/list ([x ls] [i (in-naturals)]) (order (format "o~a" i) (car x) (cadr x) 1))
          (for/list ([x rs] [i (in-naturals)]) (pay (format "p~a" i) (car x) (cadr x) 1)))))

(spec-test '(JN.interval JN.window) "property: joins emit exactly the brute-force pairs, in any arrival order"
  (check-property
   (make-config #:tests (trials))
   (property ([sides gen:sides] [lo (gen:integer-in 0 5)] [hi (gen:integer-in 0 5)])
     (define ls (car sides)) (define rs (cdr sides))
     (define steps (append (for/list ([e ls] [i (in-naturals)]) (arrive i 's e))
                           (for/list ([e rs] [i (in-naturals 100)]) (arrive i 'r e))))
     (define (norm ps) (sort ps (λ (a b) (string<? (format "~a" a) (format "~a" b)))))
     (define expected-interval
       (for*/list ([s ls] [r rs] #:when (equal? (key s) (key r))
                   #:when (<= (- (event-ts s) lo) (event-ts r) (+ (event-ts s) hi)))
         (list (event-id s) (event-id r))))
     (check-equal? (norm (pairs (join-run (interval-join-operator lo hi always #:key key) steps)))
                   (norm expected-interval))
     (define expected-window
       (for*/list ([s ls] [r rs] #:when (equal? (key s) (key r))
                   #:when (= (quotient (event-ts s) 10) (quotient (event-ts r) 10)))
         (list (event-id s) (event-id r))))
     (check-equal? (norm (pairs (join-run (window-join-operator (tumbling 10) always #:key key) steps)))
                   (norm expected-window)))))

(define (rate id t k r) (make-event 'Rate `((k . ,k) (rate . ,r)) #:id id #:at t))

(spec-test 'JN.table "each s is matched against the table's current row for its key"
  (define op (table-join-operator #:key key))
  (define res (join-run op (list (arrive 1 's (order "o0" 1 "a" 5))          ; no row yet
                                 (arrive 2 'r (rate "r1" 2 "a" 10))
                                 (arrive 3 's (order "o1" 3 "a" 5))
                                 (arrive 4 'r (rate "r2" 4 "a" 20))          ; upsert
                                 (arrive 5 's (order "o2" 5 "a" 5))
                                 (arrive 6 's (order "o3" 6 "b" 5)))))
  (check-equal? (pairs res) '(("o1" "r1") ("o2" "r2")))
  (check-equal? (run-result-counter res 'j 'unmatched) 2)
  (define outer (join-run (table-join-operator #:key key #:outer 'left #:right-attributes '(rate))
                          (list (arrive 1 's (order "o0" 1 "a" 5)))))
  (check-equal? (event-attr (car (run-result-events outer)) 'rate) #f))

(spec-test 'JN.table "a retracted row is deleted from the table"
  (define op (table-join-operator #:key key))
  (define row (rate "r1" 2 "a" 10))
  (define res (join-run op (list (arrive 2 'r row) (arrive 3 'r (retraction row)) (arrive 4 's (order "o1" 4 "a" 5)))))
  (check-equal? (pairs res) '())
  ;; and what was already joined with the row is retracted with it (DR.retract)
  (define res2 (join-run op (list (arrive 2 'r row) (arrive 4 's (order "o1" 4 "a" 5)) (arrive 5 'r (retraction row)))))
  (check-equal? (for/list ([o (run-result-outputs res2)])
                  (define x (output-item o))
                  (if (retraction? x) (cons '⊖ (map event-id (event-constituents (retraction-event x))))
                      (map event-id (event-constituents x))))
                '(("o1" "r1") (⊖ "o1" "r1"))))

(spec-test 'JN.as-of "each s meets the version valid at t(s), even when it arrives late"
  (define op (as-of-join-operator #:key key))
  (define res (join-run op (list (arrive 1 'r (rate "v1" 0 "a" 1))
                                 (arrive 2 's (order "o1" 15 "a" 5))
                                 (arrive 3 'r (rate "v3" 20 "a" 3))
                                 (arrive 4 'r (rate "v2" 10 "a" 2))         ; late version, still before o1
                                 (arrive 5 's (order "o0" 5 "a" 5))
                                 (arrive 6 's (order "o9" 25 "b" 5)))))
  (check-equal? (pairs res) '(("o0" "v1") ("o1" "v2")))
  (check-equal? (run-result-counter res 'j 'unmatched) 1))
