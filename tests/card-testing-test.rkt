#lang racket/base
;; Acceptance, spec §15: the card-testing example reproduces every listed
;; match and the derived event exactly.
(require rackunit
         racket/list
         "util.rkt"
         "../main.rkt"
         (prefix-in ex: "../examples/card-testing.rkt"))

(define (outputs-of r node) (filter (λ (o) (eq? (output-node o) node)) (run-result-outputs r)))
;; the matches of a node as constituent id lists, shorter first, then by ids
(define (matches r node)
  (define (before? a b)
    (or (< (length a) (length b))
        (and (= (length a) (length b)) (string<? (format "~s" a) (format "~s" b)))))
  (sort (for/list ([o (outputs-of r node)]) (map event-id (event-constituents (output-item o)))) before?))

(define r (ex:occur-run))

(spec-test '(SEL.strict SEL.partition SEL.next SEL.any OUT.every OUT.maximal CON.unrestricted
             PT.sequence PT.negation PT.kleene-plus PT.where PT.within)
           "trace A (§15): every cell of the table"
  (define table
    '((strict-every      (("e1" "e2")))
      (strict-maximal    (("e1" "e2")))
      (partition-every   (("e1" "e2")))
      (partition-maximal (("e1" "e2")))
      (next-every        (("e1" "e2") ("e1" "e2" "e4")))
      (next-maximal      (("e1" "e2" "e4")))
      (any-every         (("e1" "e2") ("e1" "e4") ("e1" "e2" "e4")))
      (any-maximal       (("e1" "e2" "e4")))))
  (for ([row table])
    (check-equal? (matches r (car row)) (cadr row) (format "~a" (car row)))))

(spec-test '(DR.derive OUT.maximal ES.composite)
           "trace A (§15): the maximal match derives SuspectedFraud(c1, 2, 1500) over [1, 4] with C = {e1, e2, e4}"
  (for ([node '(next-maximal any-maximal)])
    (define e (output-item (car (outputs-of r node))))
    (check-equal? (list (event-type e) (event-attr e 'card) (event-attr e 'n) (event-attr e 'total))
                  '(SuspectedFraud "c1" 2 1500))
    (check-equal? (list (event-ts e) (event-te e)) (list (minutes 1) (minutes 4)))
    (check-equal? (map event-id (event-constituents e)) '("e1" "e2" "e4"))))

(spec-test '(OUT.maximal EVAL.deferred PT.within TM.watermark)
           "trace A (§15): maximal output is emitted only once the watermark passes 11 minutes"
  (define steps (append (ex:occur-steps)
                        (list (watermark-to 'trace-a (minutes 11)) (advance-to (minutes 12))
                              (watermark-to 'trace-a (+ (minutes 11) 1)))))
  (define r2 (run (ex:occur-program) steps #:final-watermark? #f))
  (for ([node '(strict-maximal partition-maximal next-maximal any-maximal)])
    (check-equal? (map output-at (outputs-of r2 node)) (list (minutes 12)) (format "~a" node))))

(spec-test '(PT.negation PT.span-absence SEL.any OUT.every)
           "trace A′ (§15): ¬Verified scopes the gap before the first b; unless Verified scopes the span"
  (check-equal? (matches r 'gap-negation) '(("e1" "e2") ("e1" "e2" "e4")))
  (check-equal? (matches r 'span-negation) '(("e1" "e2"))))
