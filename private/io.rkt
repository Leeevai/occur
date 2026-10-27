#lang racket/base
;; Reading events from data files and printing run results.
;;
;; JSON Lines: one object per line. The reserved keys are
;;   "type"  the event type (optional when the source declares one type)
;;   "id"    the event id (optional; fresh ids are assigned otherwise)
;;   "t"     a point time, or "ts" and "te" for an interval
;;   "n"     the multiplicity (default 1)
;;   "at"    the processing (arrival) time (default te)
;; A line {"retract": id, "at": t} retracts the earlier event with that id.
;; Every other key is a payload attribute, decoded by the type's schema:
;; a Time given as an ISO-8601 string becomes seconds, a Location is an
;; object {"x": .., "y": ..}, records are objects, and lists are arrays.
(require racket/contract
         racket/list
         racket/string
         racket/format
         json
         "time.rkt"
         "event.rkt"
         "operator.rkt")

(struct arrival (at event) #:transparent)   ; event: an event or a retraction

(define reserved '(type id t ts te n at))

(define (decode-value d v)
  (cond
    [(eq? v (json-null)) v]
    [(and (eq? d 'Time) (string? v)) (iso8601->seconds v)]
    [(and (eq? d 'Location) (hash? v)) (location (hash-ref v 'x) (hash-ref v 'y))]
    [(and (record-domain? d) (hash? v))
     (for/hasheq ([f (record-domain-fields d)] #:when (hash-has-key? v (car f)))
       (values (car f) (decode-value (cdr f) (hash-ref v (car f)))))]
    [(and (list-domain? d) (list? v)) (map (λ (x) (decode-value (list-domain-element d) x)) v)]
    [(and (eq? d 'Integer) (real? v) (integer? v)) (inexact->exact v)]
    [else v]))

(define (json->time v)
  (cond [(string? v) (iso8601->seconds v)]
        [(real? v) v]
        [else (error 'read-jsonl "not a time: ~e" v)]))

;; jsexpr->arrival : hash types -> arrival
(define (jsexpr->arrival h types #:where [where "?"])
  (cond
    [(hash-ref h 'retract #f)
     => (λ (id) (arrival (json->time (or (hash-ref h 'at #f) 0)) (retraction-of id)))]
    [else (jsexpr->event-arrival h types where)]))

;; ⊖e for the earlier event with the given id. Retraction propagates by id,
;; so the stub carries only the id.
(define (retraction-of id)
  (retraction (make-event 'Retracted '() #:id id #:at 0)))

(define (jsexpr->event-arrival h types where)
  (define type-name
    (cond [(hash-ref h 'type #f) => string->symbol]
          [(= (length types) 1) (event-type-name (car types))]
          [else (error 'read-jsonl "~a: missing \"type\" and the source declares several types" where)]))
  (define t (or (for/first ([ty types] #:when (eq? (event-type-name ty) type-name)) ty)
                (error 'read-jsonl "~a: type ~a is not declared by the source" where type-name)))
  (define ts (json->time (or (hash-ref h 'ts #f) (hash-ref h 't #f)
                             (error 'read-jsonl "~a: an event needs \"t\" or \"ts\"" where))))
  (define te (json->time (or (hash-ref h 'te #f) (hash-ref h 't #f) ts)))
  (define payload
    (for/hasheq ([(k v) (in-hash h)] #:unless (memq k reserved))
      (values k (decode-value (or (event-type-domain t k) 'Any) v))))
  (cond [(payload-problem t payload)
         => (λ (p) (error 'read-jsonl "~a: ~a event: ~a" where type-name p))])
  (define e (make-event type-name payload
                        #:id (hash-ref h 'id #f) #:ts ts #:te te
                        #:n (let ([n (hash-ref h 'n 1)]) (inexact->exact n))))
  (arrival (if (hash-ref h 'at #f) (json->time (hash-ref h 'at)) te) e))

(define (read-jsonl-arrivals path types)
  (call-with-input-file path
    (λ (in)
      (for/list ([line (in-lines in)] [i (in-naturals 1)]
                 #:unless (string=? (string-trim line) ""))
        (jsexpr->arrival (string->jsexpr line) types #:where (format "~a:~a" path i))))))

;; ---------------------------------------------------------------------------
;; Printing

(define (value->string v)
  (cond [(string? v) (~s v)]
        [(location? v) (format "(~a, ~a)" (location-x v) (location-y v))]
        [(real? v) (time->string v)]
        [(symbol? v) (symbol->string v)]
        [else (~s v)]))

(define (event->string e)
  (string-append
   (format "~a [~a, ~a]" (event-type e) (time->string (event-ts e)) (time->string (event-te e)))
   (if (= (event-n e) 1) "" (format " n=~a" (event-n e)))
   (apply string-append
          (for/list ([k (sort (hash-keys (event-payload e)) symbol<?)])
            (format " ~a=~a" k (value->string (hash-ref (event-payload e) k)))))))

(define (item->string item)
  (cond [(event? item) (event->string item)]
        [(retraction? item) (string-append "⊖ " (event->string (retraction-event item)))]
        [else (~a item)]))

(provide
 (contract-out
  [struct arrival ([at time?] [event (or/c event? retraction?)])]
  [retraction-of (-> any/c retraction?)]
  [jsexpr->arrival (->* (hash? (listof event-type?)) (#:where string?) arrival?)]
  [read-jsonl-arrivals (-> path-string? (listof event-type?) (listof arrival?))]
  [event->string (-> event? string?)]
  [item->string (-> any/c string?)]
  [value->string (-> any/c string?)]))

;; Validate an event built by a surface constructor against its type.
(define (checked-event t e where)
  (cond [(payload-problem t (event-payload e))
         => (λ (p) (raise-arguments-error (event-type-name t)
                                          (format "~a: payload not in D_~a: ~a" where (event-type-name t) p)))]
        [else e]))

(provide
 (contract-out
  [checked-event (-> event-type? event? string? event?)]))

;; A directory in JSON Lines: {"id": entity, "status": "active" | "deprecated" | "removed"}
(define (read-jsonl-directory path)
  (call-with-input-file path
    (λ (in)
      (for/list ([line (in-lines in)] #:unless (string=? (string-trim line) ""))
        (define h (string->jsexpr line))
        (cons (hash-ref h 'id) (hash-ref h 'status "active"))))))

(provide
 (contract-out
  [read-jsonl-directory (-> path-string? (listof pair?))]))
