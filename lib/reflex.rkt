#lang occur
;; Reflex (spec §16) as a library of occur forms.
;;
;; Reflex is a stateful trigger engine: it turns data arrivals into triggers,
;; "run job X for device k over this data, because of reason R". This file
;; gives its vocabulary in occur's public forms only: nothing here requires
;; a private module, and the runtime has no case for Reflex
;; (tests/reflex-purity.rkt checks the first).
(require (for-syntax racket/base syntax/parse))
(provide DataArrival SensorData Trigger
         data-arrival sensor-write
         define-devices push-devices
         accumulate window silence cron
         reflex-rule
         define-reflex)

;; ---------------------------------------------------------------------------
;; §16.1 Inputs

;; Each storage write produces one DataArrival: a reference payload listing,
;; per sensor written, the owning device, the sensor id, its measurement
;; type, the sample count n, the interval [first_ts, last_ts], and the
;; newest value (ES.reference).
(event-type DataArrival
  [writes : (Listof (Record [owner : String] [sensor : String] [type : String] [n : Integer]
                            [first_ts : Time] [last_ts : Time] [newest : Any]))])

;; One entry of a DataArrival's listing.
(define (sensor-write #:owner owner #:sensor sensor #:type type #:n n
                      #:first first-ts #:last [last-ts first-ts] #:newest [newest #f])
  (hasheq 'owner owner 'sensor sensor 'type type 'n n 'first_ts first-ts 'last_ts last-ts 'newest newest))

;; The DataArrival of a storage write committed at t.
(define (data-arrival t . writes) (DataArrival #:at t [writes writes]))

;; The device directory (CX.entity). A device is watched (it can report, and
;; is activated before it sends anything, CX.activation), deprecated (its
;; state is dropped, and its arrivals are ignored and counted), or deleted
;; (its state is dropped; if it reports again it is activated afresh).
;; Its statuses are the directory's active, inactive, and removed.
(define (device-status s)
  (case s
    [(watched) 'active]
    [(deprecated) 'inactive]
    [(deleted) 'removed]
    [else (raise-arguments-error 'devices "unknown device status; expected watched, deprecated, or deleted"
                                 "status" s)]))

;; (define-devices name [device status] ...): the directory, in force from
;; the start.
(define-syntax-rule (define-devices name [device status] ...)
  (directory name #:from (data (list (cons device (device-status 'status)) ...))))

;; (push-devices 'name entries): a script step replacing the directory, or
;; with #:mode 'patch changing the devices listed; entries are
;; (device . status) pairs.
(define (push-devices name entries #:mode [mode 'replace] #:at [t #f])
  (directory-push name (for/list ([e (in-list entries)]) (cons (car e) (device-status (cdr e))))
                  #:mode mode #:at t))

;; Normalization splits a DataArrival into one batch event per sensor
;; written (ES.batch): n samples over [first_ts, last_ts], a reference to the
;; sensor's samples with the newest value as its summary.
(event-type SensorData [owner : String] [sensor : String] [type : String] #:derived)

;; ---------------------------------------------------------------------------
;; §16.2 Conditions
;;
;; A rule has one of four conditions. Each is a name for the core condition
;; that §16.2 gives it, with Reflex's parameter names.

;; accumulate(N, S) = PT.accumulate over WN.since-last, on the watched kinds:
;; ready once the contributions since the last trigger reach N samples
;; (AG.multiplicity) and span S seconds (AG.span). A binding may override
;; both.
(define-condition accumulate #:as pt:accumulate
  #:rename ([#:min-samples #:count] [#:min-span-s #:span]))

;; window(w, o, all?) = PT.window-close: WN.tumbling(w, o) anchored at the
;; first contribution, closed by TM.keyed-watermark (mode all when
;; require_all, else any), the windows that closed together coalesced into
;; one output (OUT.coalesce).
(define-condition window #:as pt:window-close
  #:rename ([#:window-s #:size] [#:offset-s #:offset]
            [#:require-all #:mode (λ (all?) (if all? 'all 'any))]))

;; silence(Δ, recovery?) = PT.silence on the hybrid clock (the newest sample
;; time + Δ, compared with now), with the device's activation as the
;; initiator ACT (CX.activation), so silence is counted from watch start;
;; with #:emit-recovery, + PT.recovery, the first contribution after a
;; silence; together they are edge-triggered (OUT.edge). A binding may
;; override Δ.
(define-condition silence #:as pt:silence
  #:rename ([#:after-s #:after] [#:emit-recovery #:recovery]))

;; cron(expr) = PT.schedule: WN.calendar(expr, processing time, UTC) with
;; WN.empty, so an empty period still fires; ticks missed together are
;; coalesced into one output (OUT.coalesce). expr has the five fields of
;; cron, read in UTC.
(define-condition cron #:as pt:schedule #:rename ())

;; ---------------------------------------------------------------------------
;; §16.2 Rules
;;
;; (reflex-rule id #:condition condition option ...)              RL.template
;; A rule template with Reflex's modifiers, which become the template's:
;;   #:watch (type ...)               the measurement types it watches (all
;;                                    when omitted)
;;   #:max-delay-s Δd                 PT.flush on the processing clock: an
;;                                    accumulation not ready Δd after its first
;;                                    contribution fires anyway (max_delay)
;;   #:reset-on-max-delay #f          the max_delay output keeps its
;;                                    contributions counting (CON.retain); #t,
;;                                    the default, consumes them
;;   #:min-interval-s Δc              OUT.rate-limit(Δc, defer): what is capped
;;                                    is carried into the next output
;;   #:on-update continue | restart   RL.carry on a semantic edit
;; and the template's own #:target, #:params, #:applies-to, #:id, and
;; #:in-source. Rule sources and bindings are the core rule-source and bind
;; (RL.source, RL.binding, RL.fan-out with #:also).

(define-syntax (reflex-rule stx)
  (syntax-parse stx
    [(_ name:id (~alt (~once (~seq #:condition c:expr))
                      (~optional (~seq #:watch (w:expr ...)))
                      (~optional (~seq #:max-delay-s d:expr))
                      (~optional (~seq #:reset-on-max-delay r:boolean))
                      (~optional (~seq #:min-interval-s ci:expr))
                      (~optional (~seq #:on-update u:id))
                      (~optional (~seq #:target t:expr))
                      (~optional (~seq #:params ps))
                      (~optional (~seq #:applies-to a))
                      (~optional (~seq #:id i:expr))
                      (~optional (~and #:in-source in)))
        ...)
     (when (and (attribute r) (not (attribute d)))
       (raise-syntax-error 'reflex-rule "#:reset-on-max-delay needs #:max-delay-s" stx (attribute r)))
     (define (opt kw v) (if v (list kw v) '()))
     #`(rule-template name #:condition c
         #,@(if (attribute w) (list #'#:watch #'(w ...)) '())
         #,@(opt #'#:flush (attribute d))
         #,@(if (and (attribute r) (not (syntax-e (attribute r)))) (list #'#:retain #'(max_delay)) '())
         #,@(if (attribute ci) (list #'#:rate-limit #`(#,(attribute ci) defer)) '())
         #,@(opt #'#:on-update (attribute u))
         #,@(opt #'#:target (attribute t))
         #,@(opt #'#:params (attribute ps))
         #,@(opt #'#:applies-to (attribute a))
         #,@(opt #'#:id (attribute i))
         #,@(if (attribute in) (list #'#:in-source) '()))]))

;; ---------------------------------------------------------------------------
;; §16.3 Outputs

;; A trigger: DR.derive with DR.summary. Its identity is the deduplication id
;; of DR.output-identity, with the emission time added for config triggers.
(event-type Trigger
  [target : Any] [device : Any] [binding-key : Any] [reason : Any]
  [interval : Any] [samples : Integer] [windows : Integer] [sources : Any]
  [params : Any] [emitted-at : Time] [identity : String]
  #:derived)

;; ---------------------------------------------------------------------------
;; The program (§16.1–§16.3)
;;
;; (define-reflex #:arrivals name maybe-from #:devices directory
;;                #:contributions name #:triggers name)
;; declares, in the enclosing module, the DataArrival source `arrivals`
;; (with #:from data, its data; otherwise a script feeds it), the stream of
;; merged per-sensor contributions, and the trigger stream:
;;
;;   arrivals → split per sensor → key by owning device → skew guard
;;            → device directory → high-water-mark merge     = contributions
;;   contributions → rules → Trigger, delivered by device    = triggers
;;
;; Every operator is keyed by device and none reads another device's state
;; (ES.keyed), so the program scales by partition.

(define-syntax (define-reflex stx)
  (syntax-parse stx
    [(_ (~alt (~once (~seq #:arrivals arrivals:id)) (~optional (~seq #:from data))
              (~once (~seq #:devices devices:id)) (~once (~seq #:contributions contributions:id))
              (~once (~seq #:triggers triggers:id)))
        ...)
     #`(begin
         (source arrivals #:types (DataArrival) #,@(if (attribute data) (list #'#:from #'data) '()))
         (define-stream contributions (from arrivals)
           ;; OP.split: one batch event per sensor written (ES.batch), a
           ;; reference to its samples with the newest value (ES.reference)
           (split SensorData #:for ([w writes])
                  [owner (hash-ref w 'owner)] [sensor (hash-ref w 'sensor)] [type (hash-ref w 'type)]
                  #:ts (hash-ref w 'first_ts) #:te (hash-ref w 'last_ts) #:n (hash-ref w 'n)
                  #:reference (hash-ref w 'sensor) #:summary ([newest (hash-ref w 'newest)]))
           ;; ST.key-by the device that owns the sensor, not the one that sent
           ;; the write; sensors are its sub-streams, types their kinds
           (key-by owner #:substream sensor #:kind type)
           ;; TM.skew-guard: drop and count data stamped over an hour ahead
           (skew-guard (hours 1))
           ;; CX.entity: watched devices are active from the start; a
           ;; deprecated device's arrivals are ignored and counted
           (context (entity devices))
           ;; ST.monotonic-merge per sensor: its contributions are all the
           ;; conditions see, so replays and duplicate writes converge
           (monotonic-merge))
         (define-stream triggers (from contributions)
           ;; the rule instances in force on each device (§14)
           (rules)
           ;; DR.derive with DR.summary: the trigger's fields
           (map Trigger [target target] [device key] [binding-key binding-key] [reason reason]
                [interval window] [samples samples] [windows windows] [sources sources]
                [params params] [emitted-at emitted-at] [identity identity])
           ;; DR.ordered: delivered grouped by device
           (key-by device)))]))

