#lang racket/base
;; The deadline scheduler (spec §17, "No polling loop").
;;
;; Each operator instance reports the next moment it needs attention without
;; new data. A single min-heap of deadlines wakes instances when they are due.
;; An instance arms a deadline only if it is earlier than the one it already
;; holds, so the heap grows with deadlines, not with arrivals. A wake-up that
;; turns out to be early is harmless, because evaluation always re-checks the
;; condition against the current time.
;;
;; The heap is an immutable pairing heap so that the whole scheduler can be
;; saved in a checkpoint by reference. Entries with equal times pop in the
;; order they were armed (FIFO), which keeps runs deterministic.
(require racket/contract)

;; ---------------------------------------------------------------------------
;; A deadline is a time t (due when the limit reaches t) or (strictly-after t)
;; (due only once the limit exceeds t). Event-time decisions such as "emit
;; once W > te(m) + Δ" (PT.absence) arm strict deadlines.

(struct strictly-after (time) #:transparent)
(define (deadline-time d) (if (strictly-after? d) (strictly-after-time d) d))
(define (deadline-strict? d) (strictly-after? d))
;; (t, strict?) ordered lexicographically, non-strict first
(define (deadline<? a b)
  (or (< (deadline-time a) (deadline-time b))
      (and (= (deadline-time a) (deadline-time b)) (not (deadline-strict? a)) (deadline-strict? b))))
(define (deadline-due? d limit)
  (if (deadline-strict? d) (< (deadline-time d) limit) (<= (deadline-time d) limit)))

;; ---------------------------------------------------------------------------
;; Immutable pairing heap of entries #(deadline seq token), ordered by
;; (deadline, seq).

(struct ph (entry subs))   ; subs: list of ph

(define (entry<? a b)
  (or (deadline<? (vector-ref a 0) (vector-ref b 0))
      (and (equal? (vector-ref a 0) (vector-ref b 0))
           (< (vector-ref a 1) (vector-ref b 1)))))

(define (ph-merge a b)
  (cond [(not a) b]
        [(not b) a]
        [(entry<? (ph-entry a) (ph-entry b)) (ph (ph-entry a) (cons b (ph-subs a)))]
        [else (ph (ph-entry b) (cons a (ph-subs b)))]))

(define (ph-merge-pairs subs)
  (cond [(null? subs) #f]
        [(null? (cdr subs)) (car subs)]
        [else (ph-merge (ph-merge (car subs) (cadr subs)) (ph-merge-pairs (cddr subs)))]))

(define (ph-insert h e) (ph-merge (ph e '()) h))
(define (ph-pop h) (ph-merge-pairs (ph-subs h)))

;; ---------------------------------------------------------------------------
;; Deadlines: the heap, the armed deadline per token, and a sequence counter.

(struct deadlines (heap armed seq size))

(define empty-deadlines (deadlines #f (hash) 0 0))

;; Arm `token` at deadline t, unless it already holds an earlier (or equal) one.
(define (deadlines-arm d token t)
  (define held (hash-ref (deadlines-armed d) token #f))
  (if (and held (not (deadline<? t held)))
      d
      (deadlines (ph-insert (deadlines-heap d) (vector t (deadlines-seq d) token))
                 (hash-set (deadlines-armed d) token t)
                 (add1 (deadlines-seq d))
                 (add1 (deadlines-size d)))))

(define (deadlines-armed-at d token) (hash-ref (deadlines-armed d) token #f))

;; The time of the earliest live deadline, skipping superseded entries.
(define (deadlines-next d)
  (define h (deadlines-heap d))
  (cond [(not h) #f]
        [else (define e (ph-entry h))
              (if (equal? (hash-ref (deadlines-armed d) (vector-ref e 2) #f) (vector-ref e 0))
                  (deadline-time (vector-ref e 0))
                  (deadlines-next (deadlines (ph-pop h) (deadlines-armed d) (deadlines-seq d)
                                             (sub1 (deadlines-size d)))))]))

;; Pop the earliest live deadline due at `limit`.
;; Returns (values token time d') or (values #f #f d') when none is due.
;; An entry is live when it is the deadline its token currently holds;
;; superseded entries are discarded without waking anyone.
(define (deadlines-pop-due d limit)
  (let loop ([d d])
    (define h (deadlines-heap d))
    (cond
      [(not h) (values #f #f d)]
      [else
       (define e (ph-entry h))
       (define t (vector-ref e 0))
       (define token (vector-ref e 2))
       (define live? (equal? (hash-ref (deadlines-armed d) token #f) t))
       (cond
         [(not live?)
          (loop (deadlines (ph-pop h) (deadlines-armed d) (deadlines-seq d) (sub1 (deadlines-size d))))]
         [(not (deadline-due? t limit)) (values #f #f d)]
         [else (values token (deadline-time t) (deadlines (ph-pop h)
                                          (hash-remove (deadlines-armed d) token)
                                          (deadlines-seq d)
                                          (sub1 (deadlines-size d))))])])))

;; Drop every pending deadline (used when timers are suspended and resumed).
(define (deadlines-tokens d) (hash-keys (deadlines-armed d)))

(define deadline/c (or/c real? strictly-after?))

(provide
 empty-deadlines
 deadline/c
 (contract-out
  [struct strictly-after ([time real?])]
  [deadline-time (-> deadline/c real?)]
  [deadline-due? (-> deadline/c real? boolean?)]
  [deadlines? (-> any/c boolean?)]
  [deadlines-size (-> deadlines? exact-nonnegative-integer?)]
  [deadlines-arm (-> deadlines? any/c deadline/c deadlines?)]
  [deadlines-armed-at (-> deadlines? any/c (or/c #f deadline/c))]
  [deadlines-next (-> deadlines? (or/c #f real?))]
  [deadlines-pop-due (-> deadlines? real? (values any/c (or/c #f real?) deadlines?))]
  [deadlines-tokens (-> deadlines? list?)]))

;; ===========================================================================
;; TM.schedule: five-field cron expressions, read in UTC.
;;
;;   minute (0-59)  hour (0-23)  day-of-month (1-31)  month (1-12 or JAN-DEC)
;;   day-of-week (0-7 or SUN-SAT; 0 and 7 are Sunday)
;;
;; Each field is a comma-separated list of `*`, `n`, `a-b`, with an optional
;; step `/s`. When both day fields are restricted, a day matches if either
;; matches (the traditional cron rule); otherwise the restricted one decides.
;; See docs/decisions/0006-calendar-schedules.md.
(require racket/string
         racket/list
         "time.rkt")

(struct cron (text minutes hours doms months dows dom-any? dow-any?) #:transparent)

(define month-names '("JAN" "FEB" "MAR" "APR" "MAY" "JUN" "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"))
(define dow-names '("SUN" "MON" "TUE" "WED" "THU" "FRI" "SAT"))

(define (cron-error text msg)
  (raise-arguments-error 'parse-cron msg "expression" text))

(define (parse-value text s lo hi names)
  (define u (string-upcase s))
  (define named (and names (for/first ([n names] [i (in-naturals)] #:when (equal? n u)) i)))
  (define v (or (and named (+ named (if (eq? names month-names) 1 0))) (string->number s)))
  (unless (and (exact-integer? v) (<= lo v hi))
    (cron-error text (format "~a is out of range ~a-~a" s lo hi)))
  v)

(define (parse-field text field lo hi names)
  (define vals
    (for*/list ([item (in-list (string-split field ","))]
                [v (in-list
                    (let* ([parts (string-split item "/" #:trim? #f)]
                           [range (car parts)]
                           [step (if (= (length parts) 2)
                                     (let ([n (string->number (cadr parts))])
                                       (unless (exact-positive-integer? n) (cron-error text "bad step"))
                                       n)
                                     1)]
                           [bounds (cond [(equal? range "*") (list lo hi)]
                                         [(regexp-match #rx"^([^-]+)-([^-]+)$" range)
                                          => (λ (m) (list (parse-value text (cadr m) lo hi names)
                                                          (parse-value text (caddr m) lo hi names)))]
                                         [else (define v (parse-value text range lo hi names))
                                               (list v (if (= step 1) v hi))])])
                      (when (> (length parts) 2) (cron-error text "bad step"))
                      (when (> (car bounds) (cadr bounds)) (cron-error text "a range must ascend"))
                      (for/list ([v (in-range (car bounds) (add1 (cadr bounds)) step)]) v)))])
      v))
  (sort (remove-duplicates vals) <))

(define (parse-cron text)
  (define fields (string-split text))
  (unless (= (length fields) 5)
    (cron-error text "a cron expression has five fields: minute hour day-of-month month day-of-week"))
  (define-values (mi h dom mo dow) (apply values fields))
  (cron text
        (parse-field text mi 0 59 #f)
        (parse-field text h 0 23 #f)
        (parse-field text dom 1 31 #f)
        (parse-field text mo 1 12 month-names)
        (remove-duplicates (map (λ (d) (if (= d 7) 0 d)) (parse-field text dow 0 7 dow-names)))
        (equal? dom "*")
        (equal? dow "*")))

(define (cron-day-matches? c d wd)
  (define dom? (memv d (cron-doms c)))
  (define dow? (memv wd (cron-dows c)))
  (cond [(and (cron-dom-any? c) (cron-dow-any? c)) #t]
        [(cron-dom-any? c) (and dow? #t)]
        [(cron-dow-any? c) (and dom? #t)]
        [else (and (or dom? dow?) #t)]))

(define (cron-matches? c t)
  (define-values (y mo d h mi s wd) (utc-fields t))
  (and (memv mo (cron-months c)) (cron-day-matches? c d wd)
       (memv h (cron-hours c)) (memv mi (cron-minutes c)) #t))

(define search-limit 200000)

(define (start-of-day y mo d) (utc y mo d))
(define (start-of-next-month y mo) (if (= mo 12) (utc (add1 y) 1 1) (utc y (add1 mo) 1)))

;; The first tick strictly after t, or #f if none exists (for example 30 February).
(define (cron-next c t)
  (let loop ([m (+ (* 60 (floor (/ t 60))) 60)] [i 0])
    (cond
      [(> i search-limit) #f]
      [else
       (define-values (y mo d h mi s wd) (utc-fields m))
       (cond
         [(not (memv mo (cron-months c))) (loop (start-of-next-month y mo) (add1 i))]
         [(not (cron-day-matches? c d wd)) (loop (+ (start-of-day y mo d) 86400) (add1 i))]
         [(not (memv h (cron-hours c))) (loop (+ (start-of-day y mo d) (* 3600 (add1 h))) (add1 i))]
         [(not (memv mi (cron-minutes c))) (loop (+ m 60) (add1 i))]
         [else m])])))

;; The last tick at or before t, or #f if none exists.
(define (cron-prev c t)
  (let loop ([m (* 60 (floor (/ t 60)))] [i 0])
    (cond
      [(> i search-limit) #f]
      [else
       (define-values (y mo d h mi s wd) (utc-fields m))
       (cond
         [(not (memv mo (cron-months c))) (loop (- (utc y mo 1) 60) (add1 i))]
         [(not (cron-day-matches? c d wd)) (loop (- (start-of-day y mo d) 60) (add1 i))]
         [(not (memv h (cron-hours c))) (loop (- (+ (start-of-day y mo d) (* 3600 h)) 60) (add1 i))]
         [(not (memv mi (cron-minutes c))) (loop (- m 60) (add1 i))]
         [else m])])))

;; The ticks in (a, b], in order.
(define (cron-ticks-between c a b)
  (let loop ([t (cron-next c a)] [acc '()])
    (if (and t (<= t b)) (loop (cron-next c t) (cons t acc)) (reverse acc))))

(provide
 (contract-out
  [struct cron ([text string?] [minutes list?] [hours list?] [doms list?] [months list?] [dows list?]
                [dom-any? boolean?] [dow-any? boolean?])]
  [parse-cron (-> string? cron?)]
  [cron-matches? (-> cron? real? boolean?)]
  [cron-next (-> cron? real? (or/c #f real?))]
  [cron-prev (-> cron? real? (or/c #f real?))]
  [cron-ticks-between (-> cron? real? real? (listof real?))]))
