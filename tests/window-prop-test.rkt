#lang racket/base
;; Property tests for windows (spec §7).
(require rackunit
         rackcheck
         racket/list
         "util.rkt"
         "../private/event.rkt"
         "../private/dataflow.rkt"
         "../private/window.rkt")

(define (pts ts) (for/list ([t ts] [i (in-naturals)]) (make-event 'X '() #:id (format "e~a" i) #:at t)))
(define (run-op op es) (run-result-events (run-operator op (list es) #:arrivals (list (for/list ([i (length es)]) i))
                                                        #:watermark 'none)))

(spec-test 'WN.sliding "property: each event is in exactly the sliding windows containing it, at most ⌈Δ/σ⌉"
  (check-property
   (make-config #:tests (trials))
   (property ([ts (gen:list (gen:integer-in 0 60) #:max-length 12)] [size (gen:integer-in 1 20)]
              [slide (gen:integer-in 1 20)])
     (define es (pts ts))
     (define out (run-op (grid-window-operator (sliding size slide) contents-accumulator) es))
     (for ([e es])
       (define holders (filter (λ (w) (memq e (event-constituents w))) out))
       (check-true (<= (length holders) (ceiling (/ size slide))))
       (for ([w holders]) (check-true (and (<= (event-ts w) (event-ts e)) (< (event-ts e) (event-te w)))))
       ;; brute force: every grid window [kσ, kσ + Δ) containing t, from the first window on
       ;; the first window is the earliest one of the first event that has any
       ;; (with σ > Δ some times fall between windows)
       (define first-with (for/first ([x es] #:when (pair? (grid-starts-at (sliding size slide) (event-ts x)))) x))
       (define first-start
         (if first-with (apply min (grid-starts-at (sliding size slide) (event-ts first-with))) +inf.0))
       (define expected
         (for/list ([s (grid-starts-at (sliding size slide) (event-ts e))]
                    #:when (>= s first-start))
           s))
       (check-equal? (sort (map event-ts holders) <) expected)))))

(define (sessions es gap)
  (sort (for/list ([w (run-op (session-window-operator gap contents-accumulator) es)])
          (list (event-ts w) (event-te w) (event-attr w 'count)))
        (λ (a b) (< (car a) (car b)))))

(spec-test 'WN.session "property: sessions do not depend on arrival order and are separated by gaps >= g"
  (check-property
   (make-config #:tests (trials))
   (property ([ts (gen:list (gen:integer-in 0 80) #:max-length 12)] [gap (gen:integer-in 1 15)] [seed (gen:integer-in 0 1000)])
     (define es (pts ts))
     (define shuffled (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
                        (random-seed seed) (shuffle es)))
     (define a (sessions es gap))
     (check-equal? (sessions shuffled gap) a)
     (check-equal? (apply + (map third a)) (length es))
     (for ([x a] [y (if (null? a) '() (cdr a))])
       ;; [first, last + g) windows never touch: the next starts at least g after the last event
       (check-true (<= (cadr x) (car y)))))))

(spec-test '(WN.count AG.incremental) "property: an incremental count window equals refolding its contents"
  (define refold (make-accumulator #:init (λ () (cons 0 0))
                                   #:add (λ (s e) (cons (+ (car s) (event-n e)) (add1 (cdr s))))
                                   #:result (λ (s) (hasheq 'count (car s) 'events (cdr s)))))
  (check-property
   (make-config #:tests (trials))
   (property ([ns (gen:list (gen:integer-in 0 9) #:max-length 15)] [n (gen:integer-in 1 5)] [m (gen:integer-in 1 5)])
     (define es (for/list ([k ns] [i (in-naturals)]) (make-event 'X '() #:id (format "e~a" i) #:at i #:n k)))
     (define (payloads acc) (map event-payload (run-op (count-window-operator n m acc) es)))
     (check-equal? (payloads contents-accumulator) (payloads refold)))))
