#lang racket/base
;; ES.time: the time domain, durations, occurrence intervals, and clocks.
;;
;; The time domain 𝕋 is the extended reals in seconds: any real number that
;; is not NaN, plus -inf.0 and +inf.0 for the unbounded ends (a high-water
;; mark that has seen nothing starts at -inf.0). Exact numbers are
;; recommended; every operation here preserves exactness. See
;; docs/decisions/0001-time-domain.md.
(require racket/contract
         racket/math
         racket/format)

(provide
 -inf-time +inf-time
 clocks
 (contract-out
  [time? (-> any/c boolean?)]
  [duration? (-> any/c boolean?)]
  [clock? (-> any/c boolean?)]
  [milliseconds (-> real? duration?)]
  [seconds (-> real? duration?)]
  [minutes (-> real? duration?)]
  [hours (-> real? duration?)]
  [days (-> real? duration?)]
  [weeks (-> real? duration?)]
  [time-max (-> time? time? ... time?)]
  [time-min (-> time? time? ... time?)]
  [struct interval ([start time?] [end time?])]
  [make-point (-> time? interval?)]
  [interval-point? (-> interval? boolean?)]
  [interval-length (-> interval? duration?)]
  [interval-contains? (-> interval? time? boolean?)]
  [interval-hull (-> interval? interval? ... interval?)]
  [interval-intersect (-> interval? interval? (or/c interval? #f))]
  [time->string (-> time? string?)]
  [utc (->* (exact-integer? (integer-in 1 12) (integer-in 1 31))
            ((integer-in 0 23) (integer-in 0 59) (and/c real? (>=/c 0) (</c 61)))
            time?)]
  [utc-fields (-> (and/c time? rational?) (values exact-integer? exact-integer? exact-integer?
                                                  exact-integer? exact-integer? real?
                                                  exact-integer?))]
  [iso8601->seconds (-> string? time?)]
  [seconds->iso8601 (-> (and/c time? rational?) string?)]
  [days-from-civil (-> exact-integer? exact-integer? exact-integer? exact-integer?)]
  [civil-from-days (-> exact-integer? (values exact-integer? exact-integer? exact-integer?))]))

;; ---------------------------------------------------------------------------
;; Time points and durations

(define -inf-time -inf.0)
(define +inf-time +inf.0)

(define (time? v) (and (real? v) (not (nan? v))))
(define (duration? v) (and (time? v) (>= v 0)))

(define (milliseconds n) (/ n 1000))
(define (seconds n) n)
(define (minutes n) (* n 60))
(define (hours n) (* n 3600))
(define (days n) (* n 86400))
(define (weeks n) (* n 604800))

(define (time-max t . ts) (for/fold ([m t]) ([u (in-list ts)]) (if (> u m) u m)))
(define (time-min t . ts) (for/fold ([m t]) ([u (in-list ts)]) (if (< u m) u m)))

;; ---------------------------------------------------------------------------
;; Clocks (TM.clock): every timed operator declares one of these.

(define clocks '(event-time processing-time hybrid))
(define (clock? v) (and (memq v clocks) #t))

;; ---------------------------------------------------------------------------
;; Occurrence intervals [start, end] with start <= end. A point is the
;; degenerate interval start = end.

(struct interval (start end)
  #:transparent
  #:guard (λ (s e name)
            (unless (<= s e)
              (raise-arguments-error name "interval start must not exceed its end"
                                     "start" s "end" e))
            (values s e)))

(define (make-point t) (interval t t))
(define (interval-point? i) (= (interval-start i) (interval-end i)))
(define (interval-length i)
  (define s (interval-start i))
  (define e (interval-end i))
  (if (= s e) 0 (- e s)))
(define (interval-contains? i t) (<= (interval-start i) t (interval-end i)))
(define (interval-hull i . is)
  (interval (apply time-min (interval-start i) (map interval-start is))
            (apply time-max (interval-end i) (map interval-end is))))
(define (interval-intersect a b)
  (define s (time-max (interval-start a) (interval-start b)))
  (define e (time-min (interval-end a) (interval-end b)))
  (and (<= s e) (interval s e)))

(define (time->string t)
  (cond [(eqv? t -inf.0) "-inf"]
        [(eqv? t +inf.0) "+inf"]
        [(and (exact? t) (not (integer? t))) (~r t #:precision 3)]
        [else (~a t)]))

;; ---------------------------------------------------------------------------
;; UTC calendar arithmetic, used for seconds-since-the-epoch time domains and
;; by calendar schedules (TM.schedule). Algorithms from H. Hinnant,
;; "chrono-Compatible Low-Level Date Algorithms".

(define (floor/ a b) (floor (/ a b)))

(define (days-from-civil y m d)
  (define y* (if (<= m 2) (- y 1) y))
  (define era (floor/ y* 400))
  (define yoe (- y* (* era 400)))
  (define doy (+ (quotient (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (- d 1)))
  (define doe (+ (* yoe 365) (quotient yoe 4) (- (quotient yoe 100)) doy))
  (+ (* era 146097) doe -719468))

(define (civil-from-days z0)
  (define z (+ z0 719468))
  (define era (floor/ z 146097))
  (define doe (- z (* era 146097)))
  (define yoe (quotient (- doe (quotient doe 1460) (- (quotient doe 36524)) (quotient doe 146096))
                        365))
  (define doy (- doe (+ (* 365 yoe) (quotient yoe 4) (- (quotient yoe 100)))))
  (define mp (quotient (+ (* 5 doy) 2) 153))
  (define d (+ (- doy (quotient (+ (* 153 mp) 2) 5)) 1))
  (define m (if (< mp 10) (+ mp 3) (- mp 9)))
  (values (+ yoe (* era 400) (if (<= m 2) 1 0)) m d))

(define (utc y mo d [h 0] [mi 0] [s 0])
  (+ (* 86400 (days-from-civil y mo d)) (* 3600 h) (* 60 mi) s))

;; utc-fields : seconds -> (values year month day hour minute second weekday)
;; weekday is 0 for Sunday through 6 for Saturday.
(define (utc-fields t)
  (define whole (floor t))
  (define day (floor/ whole 86400))
  (define secs (- t (* day 86400)))
  (define-values (y m d) (civil-from-days (inexact->exact day)))
  (define h (inexact->exact (floor (/ secs 3600))))
  (define mi (inexact->exact (floor (/ (- secs (* h 3600)) 60))))
  (define s (- secs (* h 3600) (* mi 60)))
  (values y m d h mi s (modulo (+ (inexact->exact day) 4) 7)))

(define iso-rx #px"^(-?\\d{4,})-(\\d{2})-(\\d{2})(?:[T ](\\d{2}):(\\d{2})(?::(\\d{2})(?:\\.(\\d+))?)?)?(Z|[+-]00:?00)?$")

(define (iso8601->seconds str)
  (define m (regexp-match iso-rx str))
  (unless m
    (raise-arguments-error 'iso8601->seconds "not a UTC ISO-8601 timestamp" "string" str))
  (define (num i [default 0]) (if (list-ref m i) (string->number (list-ref m i)) default))
  (define frac (if (list-ref m 7)
                   (/ (string->number (list-ref m 7)) (expt 10 (string-length (list-ref m 7))))
                   0))
  (utc (num 1) (num 2) (num 3) (num 4) (num 5) (+ (num 6) frac)))

(define (pad2 n) (~r n #:min-width 2 #:pad-string "0"))

(define (seconds->iso8601 t)
  (define-values (y m d h mi s _wd) (utc-fields t))
  (define whole-s (floor s))
  (define frac (- s whole-s))
  (string-append (~a y) "-" (pad2 m) "-" (pad2 d) "T" (pad2 h) ":" (pad2 mi) ":"
                 (pad2 (inexact->exact whole-s))
                 (if (zero? frac) "" (substring (~r frac #:precision '(= 3)) 1))
                 "Z"))
