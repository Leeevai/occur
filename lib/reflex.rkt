#lang occur
;; Reflex (spec §16) as a library of occur forms.
;;
;; Reflex is a stateful trigger engine: it turns data arrivals into triggers,
;; "run job X for device k over this data, because of reason R". This file
;; gives its vocabulary in occur's public forms only: nothing here requires
;; a private module, and the runtime has no case for Reflex
;; (tests/reflex-purity.rkt checks the first).
(provide DataArrival SensorData Trigger
         sensor-write)

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

;; Normalization splits a DataArrival into one batch event per sensor
;; written (ES.batch): n samples over [first_ts, last_ts], a reference to the
;; sensor's samples with the newest value as its summary.
(event-type SensorData [owner : String] [sensor : String] [type : String] #:derived)

;; ---------------------------------------------------------------------------
;; §16.3 Outputs

;; A trigger: DR.derive with DR.summary. Its identity is the deduplication id
;; of DR.output-identity, with the emission time added for config triggers.
(event-type Trigger
  [target : Any] [device : Any] [binding-key : Any] [reason : Any]
  [interval : Any] [samples : Integer] [windows : Integer] [sources : Any]
  [params : Any] [emitted-at : Time] [identity : String]
  #:derived)
