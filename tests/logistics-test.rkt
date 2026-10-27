#lang racket/base
;; examples/logistics.rkt: trucks, geofences, movement, and delivery windows.
(require rackunit
         racket/list
         "util.rkt"
         "../main.rkt"
         (prefix-in logistics: "../examples/logistics.rkt"))

(define r (logistics:occur-run))
(define (outs node) (filter (λ (o) (eq? (output-node o) node)) (run-result-outputs r)))
(define (events node) (filter event? (map output-item (outs node))))
(define (m n) (minutes n))
(define (who e) (format "~a@~a" (event-attr e 'truck) (/ (event-te e) 60)))
(define (ids-of e) (map event-id (event-constituents e)))

(spec-test 'CX.spatial "geofences at the depot, at customers, and around an incident"
  (check-equal? (map who (events 'at-depot)) '("t1@0" "t2@0" "t1@45"))
  (check-equal? (for/list ([e (events 'at-customer)]) (list (who e) (event-attr e 'context)))
                '(("t1@20" (near-entity "acme")) ("t2@20" (near-entity "bolt"))
                  ("t1@25" (near-entity "acme")) ("t2@25" (near-entity "bolt"))))
  (check-equal? (map who (events 'near-incident)) '("t2@32" "t2@35" "t2@40")))

(spec-test 'PT.spatial.distance "a truck that has not moved, and a truck far from the depot"
  (check-equal? (map ids-of (events 'stuck)) '(("t2@30" "t2@35" "t2@40")))
  (check-equal? (map ids-of (events 'far-out)) '(("t1@20") ("t1@25"))))

(spec-test 'PT.spatial.moving "trucks heading out in a straight line, and heading home"
  (check-equal? (map ids-of (events 'heading-out))
                '(("t1@0" "t1@5" "t1@10") ("t2@0" "t2@5" "t2@10") ("t1@5" "t1@10" "t1@15")
                  ("t2@5" "t2@10" "t2@15") ("t1@10" "t1@15" "t1@20") ("t2@10" "t2@15" "t2@20")))
  (check-equal? (map ids-of (events 'heading-home))
                '(("t1@25" "t1@30" "t1@35") ("t1@30" "t1@35" "t1@40") ("t1@35" "t1@40" "t1@45"))))

(spec-test '(ES.membership DR.derive) "a trip is made of its pings: its provenance"
  (define trip (car (events 'trips)))
  (check-equal? (list (event-attr trip 'truck) (event-ts trip) (event-te trip)) (list "t1" 0 (m 45)))
  (check-equal? (map (λ (e) (event-attr e 'pings)) (events 'trip-lengths)) '(10))
  (check-true (for/and ([t '(0 5 25 45)])
                (member*? (make-event 'Ping '() #:id (format "t1@~a" t) #:at 0) trip))))

(spec-test '(AL.before AL.meets AL.overlaps AL.starts AL.during AL.finishes AL.equals AL.finished-by
             AL.contains AL.started-by AL.overlapped-by AL.met-by AL.after)
           "each stop relates to its promised window in one of Allen's thirteen ways"
  (check-equal? (sort (for/list ([e (events 'punctuality)]) (list (event-attr e 'cause) (event-attr e 'relation)))
                      < #:key (λ (x) (string->number (substring (car x) 1))))
                '(("o1" before) ("o2" meets) ("o3" overlaps) ("o4" starts) ("o5" during) ("o6" finishes)
                  ("o7" equals) ("o8" finished-by) ("o9" contains) ("o10" started-by) ("o11" overlapped-by)
                  ("o12" met-by) ("o13" after)))
  (check-equal? (map (λ (e) (event-attr e 'cause)) (events 'on-time)) '("o4" "o5" "o6" "o7"))
  (check-equal? (map (λ (e) (event-attr e 'cause)) (events 'early)) '("o1" "o2"))
  (check-equal? (map (λ (e) (event-attr e 'cause)) (events 'late)) '("o12" "o13")))

(spec-test '(ES.causality ES.precedes PT.conjunction) "each stop was caused by its order, which precedes it"
  (check-equal? (length (events 'fulfilled)) 13)
  (check-true (for/and ([e (events 'fulfilled)])
                (define-values (o s) (apply values (event-constituents e)))
                (equal? (event-attr s 'cause) (event-id o)))))
