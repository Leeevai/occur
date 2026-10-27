#lang racket/base
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/event.rkt"
         "../private/stream.rkt")

(define (swipe id t card) (make-event 'Swipe `((card . ,card)) #:id id #:at t))

(spec-test 'ES.stream "a stream has declared types and keeps arrival order, which may be out of order"
  (define s (make-event-stream '(Swipe) (list (swipe "a" 1 "c1") (swipe "b" 5 "c2") (swipe "c" 3 "c1"))))
  (check-equal? (map event-id (event-stream-events s)) '("a" "b" "c"))
  (check-equal? (map event-id (out-of-order-events s)) '("c"))
  (check-false (stream-in-order? s))
  (check-exn #rx"not declared" (λ () (make-event-stream '(Verified) (list (swipe "a" 1 "c1"))))))

(spec-test 'ES.history "H_S(t) holds every event that has completely occurred by t"
  (define long (make-event 'Swipe '((card . "c1")) #:id "l" #:ts 2 #:te 9))
  (define s (make-event-stream '(Swipe) (list (swipe "a" 1 "c1") long (swipe "b" 5 "c2"))))
  (check-equal? (map event-id (history s 5)) '("a" "b"))
  (check-equal? (map event-id (history s 9)) '("a" "l" "b"))
  (check-equal? (history s 0) '()))

(spec-test 'ES.keyed "a key function partitions a stream into per-key substreams"
  (define s (make-event-stream '(Swipe) (list (swipe "a" 1 "c1") (swipe "b" 2 "c2") (swipe "c" 3 "c1"))))
  (define parts (partition-stream s (attribute-key 'card)))
  (check-equal? (map car parts) '("c1" "c2"))
  (check-equal? (map event-id (event-stream-events (cdr (first parts)))) '("a" "c"))
  (check-equal? (map event-id (event-stream-events (substream s (attribute-key 'card) "c2"))) '("b")))

(define gen:swipes
  (gen:let ([cards (gen:list (gen:one-of '("c1" "c2" "c3")) #:max-length 12)])
    (for/list ([c cards] [i (in-naturals)]) (swipe (format "e~a" i) i c))))

(spec-test 'ES.keyed "property: the partition is disjoint, covers S, and preserves order"
  (check-property
   (make-config #:tests (trials))
   (property ([es gen:swipes])
     (define s (make-event-stream '(Swipe) es))
     (define parts (partition-stream s (attribute-key 'card)))
     (define all (append-map (λ (p) (event-stream-events (cdr p))) parts))
     (check-equal? (sort (map event-id all) string<?) (sort (map event-id es) string<?))
     (for ([p parts])
       (define ids (map event-id (event-stream-events (cdr p))))
       (check-equal? ids (filter (λ (i) (member i ids)) (map event-id es)))))))

(spec-test 'ES.substream "an entity owns sub-streams, each with a kind; operators may watch kinds"
  (define (reading id t owner sensor kind)
    (make-event 'Reading `((owner . ,owner) (sensor . ,sensor) (measurement-type . ,kind)) #:id id #:at t))
  (define rs (list (reading "r1" 1 "d1" "s1" 'load)
                   (reading "r2" 2 "d1" "s2" 'position)
                   (reading "r3" 3 "d2" "s3" 'load)
                   (reading "r4" 4 "d1" "s1" 'load)))
  (define s (make-event-stream '(Reading) rs))
  (define ky (attribute-keying #:entity 'owner #:substream 'sensor #:kind 'measurement-type))
  (check-equal? (entity-of ky (first rs)) "d1")
  (check-equal? (substream-of ky (first rs)) "s1")
  (check-equal? (kind-of ky (first rs)) 'load)
  (define subs (entity-substreams s ky "d1"))
  (check-equal? (map car subs) '("s1" "s2"))
  (check-equal? (map event-id (event-stream-events (cdr (first subs)))) '("r1" "r4"))
  (check-equal? (entity-kinds s ky "d1") '(load position))
  (check-equal? (map event-id (watching ky '(position) rs)) '("r2"))
  (check-equal? (length (watching ky #f rs)) 4))
