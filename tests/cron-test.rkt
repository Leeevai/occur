#lang racket/base
(require rackunit
         rackcheck
         "util.rkt"
         "../private/time.rkt"
         "../private/scheduler.rkt")

(define (at y mo d h mi) (utc y mo d h mi))

(spec-test 'TM.schedule "fields, ranges, steps, lists, and names parse into sets"
  (define c (parse-cron "*/15 9-17 * JAN,mar 1-5"))
  (check-equal? (cron-minutes c) '(0 15 30 45))
  (check-equal? (cron-hours c) '(9 10 11 12 13 14 15 16 17))
  (check-equal? (cron-months c) '(1 3))
  (check-equal? (cron-dows c) '(1 2 3 4 5))
  (check-equal? (cron-dows (parse-cron "0 0 * * 7")) '(0))
  (check-equal? (cron-dows (parse-cron "0 0 * * SUN-TUE")) '(0 1 2))
  (check-equal? (cron-minutes (parse-cron "5/20 * * * *")) '(5 25 45))
  (check-exn #rx"five fields" (λ () (parse-cron "* * * *")))
  (check-exn #rx"out of range" (λ () (parse-cron "60 * * * *")))
  (check-exn #rx"must ascend" (λ () (parse-cron "10-5 * * * *"))))

(spec-test 'TM.schedule "the next and previous ticks of an hourly schedule (trace R5)"
  (define hourly (parse-cron "0 * * * *"))
  (check-equal? (cron-next hourly (at 2026 1 5 10 20)) (at 2026 1 5 11 0))
  (check-equal? (cron-prev hourly (at 2026 1 5 10 20)) (at 2026 1 5 10 0))
  (check-equal? (cron-next hourly (at 2026 1 5 11 0)) (at 2026 1 5 12 0))
  (check-equal? (cron-prev hourly (at 2026 1 5 11 0)) (at 2026 1 5 11 0))
  (check-equal? (cron-ticks-between hourly (at 2026 1 5 10 50) (+ (at 2026 1 5 13 10) 0))
                (list (at 2026 1 5 11 0) (at 2026 1 5 12 0) (at 2026 1 5 13 0))))

(spec-test 'TM.schedule "weekday mornings, month ends, and the day-field rule"
  (define weekday (parse-cron "0 6 * * 1-5"))
  ;; 2026-10-30 is a Friday; the next tick is Monday 2026-11-02 06:00
  (check-equal? (cron-next weekday (at 2026 10 30 7 0)) (at 2026 11 2 6 0))
  (check-equal? (cron-next (parse-cron "59 23 31 12 *") (at 2026 1 1 0 0)) (at 2026 12 31 23 59))
  ;; both day fields restricted: the 13th or any Friday
  (define c (parse-cron "0 0 13 * 5"))
  (check-equal? (cron-next c (at 2026 11 1 0 0)) (at 2026 11 6 0 0))
  (check-true (cron-matches? c (at 2026 11 13 0 0)))
  ;; an impossible day has no ticks
  (check-false (cron-next (parse-cron "0 0 30 2 *") (at 2026 1 1 0 0))))

(spec-test 'TM.schedule "property: next is the first matching minute after t, prev the last at or before"
  (define exprs (map parse-cron '("*/7 * * * *" "0 */3 * * *" "30 2 * * 1,3" "15 10 1-7 * *")))
  (check-property
   (make-config #:tests (trials))
   (property ([t (gen:integer-in (at 2026 1 1 0 0) (at 2027 1 1 0 0))] [i (gen:integer-in 0 3)])
     (define c (list-ref exprs i))
     (define n (cron-next c t))
     (define p (cron-prev c t))
     (check-true (> n t))
     (check-true (cron-matches? c n))
     (check-true (<= p t))
     (check-true (cron-matches? c p))
     (check-equal? (cron-prev c n) n)
     (check-equal? (cron-next c p) n))))
