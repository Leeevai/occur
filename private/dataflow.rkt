#lang racket/base
;; The operator graph and the deterministic scripted runner.
;;
;; A program is a graph of sources and nodes. Each node runs an operator
;; (private/operator.rkt) partitioned by the operator's key function. The
;; runner executes a finite, ordered script of steps against the program.
;; Processing time is simulated from the script and never read from the wall
;; clock, so every run is reproducible (docs/decisions/0003-runner-order.md).
(require racket/contract
         racket/list
         racket/match
         "time.rkt"
         "event.rkt"
         "operator.rkt"
         "scheduler.rkt")

;; ===========================================================================
;; Sources and nodes

;; watermark strategy: 'none (explicit watermark steps only), 'ascending
;; (W = max te seen), (list 'bounded-delay d) (W = max te seen − d), or
;; (list 'heartbeat T) (W = te of the newest event of type T, a source that
;; emits heartbeats while idle, TM.idle).
(struct source (name types watermark data meta) #:transparent)

(define (make-source name
                     #:types [types '()]
                     #:watermark [watermark 'none]
                     #:data [data #f]
                     #:meta [meta (hasheq)])
  (source name types watermark data meta))

;; An input reference: an upstream source, node, or node name, and the
;; upstream output port it reads.
(struct input-ref (upstream port) #:transparent)

(struct node (name op inputs sink? recursive) #:transparent)

(define (->input-ref x)
  (cond [(input-ref? x) x]
        [(and (pair? x) (exact-nonnegative-integer? (cdr x))) (input-ref (car x) (cdr x))]
        [else (input-ref x 0)]))

(define (make-node name op inputs #:sink? [sink? #f] #:recursive [recursive #f])
  (unless (= (length inputs) (operator-arity op))
    (raise-arguments-error 'make-node "the number of inputs does not match the operator's arity"
                           "node" name "arity" (operator-arity op) "inputs" (length inputs)))
  (node name op (map ->input-ref inputs) sink? recursive))

(define (upstream-name u)
  (cond [(source? u) (source-name u)] [(node? u) (node-name u)] [else u]))

;; ===========================================================================
;; Programs

(struct program (sources nodes order inputs downstream) #:transparent)

(define (make-program roots)
  ;; Collect every source and node reachable from the roots.
  (define sources (make-hasheq))
  (define nodes (make-hasheq))
  (define (claim! table name v)
    (define old (hash-ref table name #f))
    (when (and old (not (eq? old v)))
      (raise-arguments-error 'make-program "two different definitions share a name" "name" name))
    (hash-set! table name v))
  (let walk ([todo roots])
    (unless (null? todo)
      (define x (car todo))
      (cond
        [(source? x)
         (unless (hash-ref sources (source-name x) #f) (claim! sources (source-name x) x))
         (walk (cdr todo))]
        [(node? x)
         (cond [(eq? (hash-ref nodes (node-name x) #f) x) (walk (cdr todo))]
               [else (claim! nodes (node-name x) x)
                     (walk (append (for/list ([r (node-inputs x)]
                                              #:unless (symbol? (input-ref-upstream r)))
                                     (input-ref-upstream r))
                                   (cdr todo)))])]
        [else (walk (cdr todo))])))
  (for ([n (in-hash-keys nodes)]) (when (hash-ref sources n #f)
                                    (raise-arguments-error 'make-program
                                                           "a source and a node share a name"
                                                           "name" n)))
  ;; Resolve inputs to names and check that they exist.
  (define inputs
    (for/hasheq ([(name n) (in-hash nodes)])
      (values name
              (for/list ([r (node-inputs n)])
                (define up (upstream-name (input-ref-upstream r)))
                (unless (or (hash-ref sources up #f) (hash-ref nodes up #f))
                  (raise-arguments-error 'make-program "unknown input" "node" name "input" up))
                (cons up (input-ref-port r))))))
  ;; Downstream edges: (upstream . port) -> list of (node . input-port), in
  ;; node-name order for determinism.
  (define node-names (sort (hash-keys nodes) symbol<?))
  (define downstream
    (for*/fold ([h (hash)]) ([name (in-list node-names)]
                             [(r i) (in-indexed (hash-ref inputs name))])
      (hash-update h r (λ (l) (append l (list (cons name i)))) '())))
  ;; DR.cycle-guard: every cycle must pass through a node declared recursive.
  (define succ
    (for*/fold ([h (hasheq)]) ([(name rs) (in-hash inputs)] [r (in-list rs)]
                               #:when (hash-ref nodes (car r) #f))
      (hash-update h (car r) (λ (l) (cons name l)) '())))
  (check-cycles! nodes succ)
  (define order (topological-order node-names inputs nodes))
  (program (for/hasheq ([(k v) (in-hash sources)]) (values k v))
           (for/hasheq ([(k v) (in-hash nodes)]) (values k v))
           order inputs downstream))

(define (in-indexed l) (in-parallel (in-list l) (in-naturals)))

;; Tarjan's strongly connected components; a component with a cycle must
;; contain a recursive node.
(define (check-cycles! nodes succ)
  (define index (make-hasheq)) (define low (make-hasheq)) (define on (make-hasheq))
  (define stack '()) (define counter 0)
  (define (strong v)
    (hash-set! index v counter) (hash-set! low v counter) (set! counter (add1 counter))
    (set! stack (cons v stack)) (hash-set! on v #t)
    (for ([w (in-list (hash-ref succ v '()))])
      (cond [(not (hash-ref index w #f)) (strong w)
                                         (hash-set! low v (min (hash-ref low v) (hash-ref low w)))]
            [(hash-ref on w #f) (hash-set! low v (min (hash-ref low v) (hash-ref index w)))]))
    (when (= (hash-ref low v) (hash-ref index v))
      (define comp
        (let loop ([acc '()])
          (define w (car stack))
          (set! stack (cdr stack)) (hash-remove! on w)
          (if (eq? w v) (cons w acc) (loop (cons w acc)))))
      (define cyclic? (or (> (length comp) 1) (memq v (hash-ref succ v '()))))
      (when (and cyclic? (not (for/or ([c comp]) (node-recursive (hash-ref nodes c)))))
        (raise-arguments-error
         'make-program
         "cycle guard: these nodes consume their own outputs; declare one of them recursive with a termination bound"
         "cycle" (sort comp symbol<?)))))
  (for ([v (in-list (sort (hash-keys nodes) symbol<?))])
    (unless (hash-ref index v #f) (strong v))))

;; Depth-first topological order over node inputs, ignoring back edges.
(define (topological-order names inputs nodes)
  (define state (make-hasheq))
  (define out '())
  (define (visit n)
    (unless (hash-ref state n #f)
      (hash-set! state n 'active)
      (for ([r (hash-ref inputs n)] #:when (hash-ref nodes (car r) #f))
        (unless (eq? (hash-ref state (car r) #f) 'active) (visit (car r))))
      (hash-set! state n 'done)
      (set! out (cons n out))))
  (for-each visit names)
  (reverse out))

;; ===========================================================================
;; Script steps

;; An arrival carries an event, or a retraction ⊖e of an earlier event
;; (TM.retraction).
(struct step:arrive (time source event) #:transparent)
(struct step:advance (time) #:transparent)
(struct step:watermark (source time) #:transparent)
(struct step:control (time target msg config?) #:transparent)
(struct step:suspend (from to) #:transparent)

(struct step:checkpoint () #:transparent)
(struct step:restore () #:transparent)
(struct step:cold-start () #:transparent)

(define (arrive t src e) (step:arrive t src e))
;; TM.checkpoint: snapshot every operator's state, the read position, and
;; the undelivered outputs. (restore) returns to the snapshot and re-reads
;; the input retained since. TM.replay: (cold-start) rebuilds state by
;; re-reading all retained input, holding outputs back (see run-step!).
(define (checkpoint) (step:checkpoint))
(define (restore) (step:restore))
(define (cold-start) (step:cold-start))
(define (advance-to t) (step:advance t))
(define (watermark-to src t) (step:watermark src t))
(define (control msg #:at [t #f] #:to [target 'all] #:config? [config? #f])
  (step:control t target msg config?))
(define (suspend t1 t2) (step:suspend t1 t2))

(define (step-time s)
  (match s
    [(step:arrive t _ _) t]
    [(step:advance t) t]
    [(step:control t _ _ _) t]
    [(step:suspend t1 _) t1]
    [_ #f]))

;; ===========================================================================
;; The simulated processing clock

(struct simulated-clock (start) #:transparent)

;; ===========================================================================
;; Outputs and results

(struct output (at node key item) #:transparent)

(struct run-result (outputs counters reports now watermarks states) #:transparent)

(define (run-result-events r [node #f])
  (for/list ([o (in-list (run-result-outputs r))]
             #:when (event? (output-item o))
             #:when (or (not node) (eq? node (output-node o))))
    (output-item o)))

(define (run-result-counter r node name)
  (hash-ref (hash-ref (run-result-counters r) node (hash)) name 0))

;; ===========================================================================
;; The runner

(struct runner (program
                [now #:mutable]
                [states #:mutable]       ; hasheq node -> (cons global keys)
                [pdl #:mutable]          ; processing-time deadlines, tokens (node . key)
                [edl #:mutable]          ; hasheq node -> deadlines over keys (event time)
                [wm #:mutable]           ; hasheq name -> W, for sources and nodes
                [maxte #:mutable]        ; hasheq source -> max te seen
                [counters #:mutable]     ; hasheq node -> hash name -> n
                [reports #:mutable]      ; reversed list of (node . datum)
                [prov #:mutable]         ; hasheq node -> hash id -> list of events
                [retracted #:mutable]    ; hasheq node -> hash of the ids retracted at its inputs
                [ids #:mutable]          ; box holding the id counter
                [outputs #:mutable]      ; reversed list of output
                [outbox #:mutable]       ; reversed list of output awaiting delivery
                [suspended? #:mutable]
                [seqs #:mutable]         ; hasheq node -> number of events delivered so far
                [log #:mutable]          ; reversed list of the retained input steps
                [relogging? #:mutable]   ; re-reading retained input: do not log again
                [snapshot #:mutable]     ; the last checkpoint, or #f
                [holding? #:mutable]     ; replay: outputs are held back
                [frozen? #:mutable]      ; replay: processing time does not move
                [held #:mutable]         ; reversed list of held outputs
                [held-emissions #:mutable] ; reversed list of #(node key port item) held at their node
                [awaiting-config? #:mutable]
                delivery
                track-provenance?))

(define (initial-states prog)
  (for/hasheq ([(name n) (in-hash (program-nodes prog))])
    (values name (cons ((operator-init-global (node-op n))) (hash)))))

(define (make-runner prog start delivery track-provenance?)
  (runner prog start (initial-states prog) empty-deadlines
          (for/hasheq ([name (in-hash-keys (program-nodes prog))]) (values name empty-deadlines))
          (hasheq) (hasheq) (hasheq) '() (hasheq) (hasheq) (box 0) '() '() #f (hasheq)
          '() #f #f #f #f '() '() #f delivery track-provenance?))

(define (W r name) (hash-ref (runner-wm r) name -inf.0))

(define (node-op* r name) (node-op (hash-ref (program-nodes (runner-program r)) name)))
(define (node-sink?* r name) (node-sink? (hash-ref (program-nodes (runner-program r)) name)))

(define (bump-counters! r name counts)
  (unless (null? counts)
    (set-runner-counters!
     r (hash-update (runner-counters r) name
                    (λ (h) (for/fold ([h h]) ([c (in-list counts)])
                             (hash-update h (car c) (λ (v) (+ v (cdr c))) 0)))
                    (hash)))))

;; Run one operator callback and absorb its effects. `f` receives the
;; context and returns the new per-key state (or #f to drop the key) for
;; key callbacks; global callbacks are handled by `with-global-callback`.
(define (make-node-ctx r name key #:port [port 0] #:clock [clock #f] #:seq [seq 0])
  (make-ctx #:node name #:key key #:port port #:clock clock
            #:now (runner-now r) #:watermark (W r name) #:seq seq))

(define (absorb-ctx! r name c)
  (bump-counters! r name (ctx-count-list c))
  (for ([d (in-list (ctx-report-list c))])
    (set-runner-reports! r (cons (cons name d) (runner-reports r))))
  (ctx-emissions c))

(define (rearm! r name key kstate #:stalled? [stalled? #f])
  (define op (node-op* r name))
  ;; A timer callback that left the state unchanged must not re-arm a
  ;; deadline that is already due: that would wake it again with nothing to
  ;; do, forever (for example at the final watermark).
  (define d ((operator-deadline op) kstate))
  (when (and d (not (and stalled? (deadline-due? d (runner-now r)))))
    (set-runner-pdl! r (deadlines-arm (runner-pdl r) (cons name key) d)))
  (define ed ((operator-event-deadline op) kstate))
  (when (and ed (not (and stalled? (deadline-due? ed (W r name)))))
    (set-runner-edl! r (hash-update (runner-edl r) name (λ (dl) (deadlines-arm dl key ed))))))

(define (set-key-state! r name key kstate)
  (define st (hash-ref (runner-states r) name))
  (set-runner-states! r (hash-set (runner-states r) name
                                  (cons (car st)
                                        (if kstate
                                            (hash-set (cdr st) key kstate)
                                            (hash-remove (cdr st) key))))))

(define (with-ids r thunk)
  (parameterize ([current-id-source (runner-ids r)]) (thunk)))

;; Run a per-key callback: (proc global kstate-or-#f ctx) -> kstate-or-#f.
(define (key-callback! r name key proc #:port [port 0] #:clock [clock #f] #:create? [create? #t]
                       #:seq [seq 0])
  (define op (node-op* r name))
  (define st (hash-ref (runner-states r) name))
  (define g (car st))
  (define c (make-node-ctx r name key #:port port #:clock clock #:seq seq))
  (define existing (hash-ref (cdr st) key #f))
  (define kstate (or existing (and create? (with-ids r (λ () ((operator-init-key op) g key c))))))
  (define new (with-ids r (λ () (proc g kstate c))))
  (set-key-state! r name key new)
  (when new (rearm! r name key new #:stalled? (and clock (eq? new kstate))))
  (propagate! r name key (absorb-ctx! r name c)))

;; Run a global callback: (proc global keys ctx) -> (values global keys).
(define (global-callback! r name proc)
  (define op (node-op* r name))
  (define st (hash-ref (runner-states r) name))
  (define c (make-node-ctx r name #f))
  (define-values (g2 keys2) (with-ids r (λ () (proc (car st) (cdr st) c))))
  (set-runner-states! r (hash-set (runner-states r) name (cons g2 keys2)))
  (for ([(k s) (in-hash keys2)])
    (unless (eq? s (hash-ref (cdr st) k #f)) (rearm! r name k s)))
  (propagate! r name #f (absorb-ctx! r name c))
  (fire-event-timers! r name))

(define (record-provenance! r name e)
  (when (runner-track-provenance? r)
    (define (add idx id) (hash-update idx id (λ (l) (cons e l)) '()))
    (set-runner-prov!
     r (hash-update (runner-prov r) name
                    (λ (idx)
                      (for/fold ([idx (add idx (event-id e))]) ([c (in-list (event-constituents e))])
                        (add idx (event-id c))))
                    (hash)))))

;; Whether e, or an event in its provenance (∈*), was retracted at the inputs
;; of node `name`.
(define (retracted-under? r name e)
  (define ids (hash-ref (runner-retracted r) name #f))
  (and ids
       (let loop ([e e])
         (or (hash-ref ids (event-id e) #f)
             (for/or ([c (in-list (event-constituents e))]) (and (event? c) (loop c)))))))

;; Deliver the emissions of node `name` (made under partition `key`).
;; During a replay, the events of a node that judges or coalesces its own
;; outputs (the rules node) are held at the node, so that they are released
;; per operator instance and judged before any later stage sees them.
(define (holds-emissions? op) (and (or (operator-judge op) (operator-coalesce op)) #t))

(define (propagate! r name key emissions)
  (define prog (runner-program r))
  (define op (node-op* r name))
  (for ([pe (in-list emissions)]
        ;; DR.retract: an event derived from one this node has seen retracted
        ;; is retracted too, so it is never emitted
        #:unless (and (event? (cdr pe)) (retracted-under? r name (cdr pe))
                      (begin (bump-counters! r name (list (cons 'retracted 1))) #t))
        #:unless (and (runner-holding? r) (event? (cdr pe)) (holds-emissions? op)
                      (begin (set-runner-held-emissions! r (cons (vector name key (car pe) (cdr pe))
                                                                 (runner-held-emissions r)))
                             #t)))
    (define port (car pe))
    (define item (cdr pe))
    (when (event? item) (record-provenance! r name item))
;; a sink's outputs are its port-0 emissions; other ports are side outputs
    (when (and (node-sink?* r name) (= port 0) (not (signal? item)))
      (define k (if (and (operator-instance-of op) (event? item)) ((operator-instance-of op) item) key))
      (set-runner-outbox! r (cons (output (runner-now r) name k item) (runner-outbox r))))
    (for ([d (in-list (hash-ref (program-downstream prog) (cons name port) '()))])
      (deliver! r (car d) (cdr d) item))))

(define (deliver! r name port item)
  (define op (node-op* r name))
  (define n (hash-ref (program-nodes (runner-program r)) name))
  (match item
    [(? event? e)
     (cond
       [(and (node-recursive n) (>= (event-level e) (node-recursive n)))
        (bump-counters! r name (list (cons 'recursion-bound 1)))]
       [else
        (define key (if (operator-key op) ((operator-key op) e) #f))
        (define seq (hash-ref (runner-seqs r) name 0))
        (set-runner-seqs! r (hash-set (runner-seqs r) name (add1 seq)))
        (key-callback! r name key (λ (g s c) ((operator-on-event op) g s e c)) #:port port #:seq seq)
        (fire-event-timers! r name)])]
    [(retraction e)
     (set-runner-retracted! r (hash-update (runner-retracted r) name (λ (ids) (hash-set ids (event-id e) #t)) (hash)))
     ;; the operator updates its state (a table deletes the row, a window
     ;; forgets the event), and what the node already derived from e is
     ;; retracted (DR.retract)
     (when (operator-on-retract op)
       (define key (if (operator-key op) ((operator-key op) e) #f))
       (key-callback! r name key (λ (g s c) ((operator-on-retract op) g s e c))
                      #:port port #:create? #f))
     (define idx (hash-ref (runner-prov r) name (hash)))
     (define outs (remove-duplicates (reverse (hash-ref idx (event-id e) '())) eq?))
     (set-runner-prov! r (hash-set (runner-prov r) name (hash-remove idx (event-id e))))
     (propagate! r name #f (for/list ([o (in-list outs)]) (cons 0 (retraction o))))]
    [(signal kind key _ _)
     (define st (hash-ref (runner-states r) name))
     (cond
       [(operator-on-signal op)
        (key-callback! r name key (λ (g s c) ((operator-on-signal op) g s item c))
                       #:create? (and (operator-lifecycle? op) (eq? kind 'activate)))]
       [(and (operator-lifecycle? op) (eq? kind 'activate))
        (unless (hash-ref (cdr st) key #f)
          (key-callback! r name key (λ (g s c) s)))]
       [(and (operator-lifecycle? op) (memq kind '(drop deactivate)))
        (set-key-state! r name key #f)])
     ;; signals continue downstream
     (for ([p (in-range (operator-outputs op))])
       (for ([d (in-list (hash-ref (program-downstream (runner-program r)) (cons name p) '()))])
         (deliver! r (car d) (cdr d) item)))]))

;; Fire the event-time timers of `name` that are due at its watermark.
(define (fire-event-timers! r name)
  (let loop ()
    (define-values (key t dl) (deadlines-pop-due (hash-ref (runner-edl r) name) (W r name)))
    ;; the key of an unpartitioned node is #f, so test the time, not the token
    (when t
      (set-runner-edl! r (hash-set (runner-edl r) name dl))
      (when (hash-has-key? (cdr (hash-ref (runner-states r) name)) key)
        (key-callback! r name key (λ (g s c) ((operator-on-timer (node-op* r name)) g s c))
                       #:clock 'event-time #:create? #f))
      (loop))))

;; Fire processing-time deadlines due at or before `limit`. Outside a
;; suspension each fires at its own time; after a suspension every overdue
;; instance fires once, at the resume time. With #:drain?, deadlines strictly
;; after the limit's instant also fire (the script has ended, so processing
;; time is taken to move on past it).
(define (fire-processing-deadlines! r limit #:drain? [drain? #f])
  ;; suspended, or replaying (TM.replay: timers wait until the replay caught up)
  (unless (or (runner-suspended? r) (runner-frozen? r))
    (let loop ()
      (define-values (token t pdl)
        (let-values ([(tok t d) (deadlines-pop-due (runner-pdl r) limit)])
          (if (or tok (not drain?))
              (values tok t d)
              (deadlines-pop-due (runner-pdl r) (+ limit (/ 1 1000000000))))))
      (when token
        (set-runner-pdl! r pdl)
        ;; outputs of an earlier instant are delivered before time moves on, so
        ;; grouping by key (DR.ordered) never reorders instants
        (when (> t (runner-now r)) (flush-outbox! r))
        (set-runner-now! r (time-max (runner-now r) t))
        (define name (car token))
        (define key (cdr token))
        (when (hash-has-key? (cdr (hash-ref (runner-states r) name)) key)
          (key-callback! r name key (λ (g s c) ((operator-on-timer (node-op* r name)) g s c))
                         #:clock 'processing-time #:create? #f))
        (loop)))))

;; Recompute node watermarks in topological order after a source advanced.
(define (propagate-watermarks! r)
  (define prog (runner-program r))
  (for ([name (in-list (program-order prog))])
    (define ins (for/list ([i (in-list (hash-ref (program-inputs prog) name))]
                           #:unless (back-edge? prog name (car i)))
                  (W r (car i))))
    (define w (if (null? ins) -inf.0 (apply min ins)))
    (when (> w (W r name))
      (set-runner-wm! r (hash-set (runner-wm r) name w))
      (fire-event-timers! r name)
      (define op (node-op* r name))
      (when (operator-on-watermark op)
        (global-callback! r name (λ (g keys c) ((operator-on-watermark op) g keys w c)))))))

(define (back-edge? prog name up)
  (define order (program-order prog))
  (define (pos x) (index-of order x))
  (and (hash-ref (program-nodes prog) up #f)
       (>= (pos up) (pos name))))

(define (advance-source-watermark! r src w)
  (when (> w (W r src))
    (set-runner-wm! r (hash-set (runner-wm r) src w))
    (propagate-watermarks! r)))

(define (flush-outbox! r)
  (define pending (reverse (runner-outbox r)))
  (set-runner-outbox! r '())
  (cond
    [(runner-holding? r) (set-runner-held! r (append (reverse pending) (runner-held r)))]
    [else (deliver-outputs! r pending)]))

(define (deliver-outputs! r pending)
  (define ordered
    (case (runner-delivery r)
      [(by-key)
       ;; DR.ordered: grouped by key in order of first appearance, FIFO within a key
       (define keys (remove-duplicates (map output-key pending)))
       (append* (for/list ([k (in-list keys)])
                  (filter (λ (o) (equal? (output-key o) k)) pending)))]
      [else pending]))
  (set-runner-outputs! r (append (reverse ordered) (runner-outputs r))))

(define (meta-step? s) (or (step:checkpoint? s) (step:restore? s) (step:cold-start? s)))

(define (run-step! r s)
  (cond
    [(step:checkpoint? s) (take-snapshot! r)]
    [(step:restore? s) (restore-snapshot! r)]
    [(step:cold-start? s) (cold-start! r)]
    [else
     (unless (runner-relogging? r) (set-runner-log! r (cons s (runner-log r))))
     (cond
       [(runner-frozen? r) (replay-step! r s)]
       [else (run-input-step! r s)])
     ;; TM.replay: held outputs are released once the configuration has loaded
     (when (and (runner-awaiting-config? r) (step:control? s) (step:control-config? s)
                (not (runner-relogging? r)))
       (release-held! r))]))

(define (run-input-step! r s)
  (define prog (runner-program r))
  (define t (step-time s))
  (when (and t (not (step:suspend? s)))
    (fire-processing-deadlines! r t)
    (when (> t (runner-now r)) (flush-outbox! r))
    (set-runner-now! r (time-max (runner-now r) t)))
  (match s
    [(step:arrive _ src e)
     (unless (hash-ref (program-sources prog) src #f)
       (raise-arguments-error 'run "arrival for an unknown source" "source" src))
     (for ([d (in-list (hash-ref (program-downstream prog) (cons src 0) '()))])
       (deliver! r (car d) (cdr d) e))
     (when (event? e)
       (define strategy (source-watermark (hash-ref (program-sources prog) src)))
       (define maxte (time-max (hash-ref (runner-maxte r) src -inf.0) (event-te e)))
       (set-runner-maxte! r (hash-set (runner-maxte r) src maxte))
       (match strategy
         ['ascending (advance-source-watermark! r src maxte)]
         [(list 'bounded-delay d) (advance-source-watermark! r src (- maxte d))]
         [(list 'heartbeat type) (when (eq? (event-type e) type)
                                   (advance-source-watermark! r src (event-te e)))]
         [_ (void)]))]
    [(step:advance _) (void)]
    [(step:watermark src w)
     (unless (hash-ref (program-sources prog) src #f)
       (raise-arguments-error 'run "watermark for an unknown source" "source" src))
     (advance-source-watermark! r src w)]
    [(step:control _ target msg _)
     (for ([name (in-list (program-order prog))]
           #:when (or (eq? target 'all) (eq? target name)))
       (define op (node-op* r name))
       (when (operator-on-control op)
         (global-callback! r name (λ (g keys c) ((operator-on-control op) g keys msg c)))))]
    [(step:suspend t1 t2)
     (fire-processing-deadlines! r t1)
     (set-runner-now! r (time-max (runner-now r) t1))
     (set-runner-now! r (time-max (runner-now r) t2))])
  (fire-processing-deadlines! r (runner-now r))
  (flush-outbox! r))

;; Generators (operators of arity 0) start with the run: their unkeyed
;; state is created at the start time and their deadlines armed.
(define (start-generators! r)
  (for ([name (in-list (program-order (runner-program r)))])
    (define op (node-op* r name))
    (when (or (zero? (operator-arity op)) (and (operator-eager? op) (not (operator-key op))))
      (key-callback! r name #f (λ (g s c) s))))
  (flush-outbox! r))

;; ===========================================================================
;; TM.checkpoint and TM.replay

(struct snap (now states pdl edl wm maxte counters prov retracted ids outbox seqs suspended? position) #:transparent)

(define (take-snapshot! r)
  (flush-outbox! r)
  (set-runner-snapshot!
   r (snap (runner-now r) (runner-states r) (runner-pdl r) (runner-edl r) (runner-wm r) (runner-maxte r)
           (runner-counters r) (runner-prov r) (runner-retracted r) (unbox (runner-ids r)) (runner-outbox r) (runner-seqs r)
           (runner-suspended? r) (length (runner-log r)))))

;; Restore the snapshot, then re-read the input retained since it.
(define (restore-snapshot! r)
  (define sn (runner-snapshot r))
  (unless sn (raise-arguments-error 'run "restore needs an earlier checkpoint"))
  (set-runner-now! r (snap-now sn)) (set-runner-states! r (snap-states sn)) (set-runner-pdl! r (snap-pdl sn))
  (set-runner-edl! r (snap-edl sn)) (set-runner-wm! r (snap-wm sn)) (set-runner-maxte! r (snap-maxte sn))
  (set-runner-counters! r (snap-counters sn)) (set-runner-prov! r (snap-prov sn))
  (set-runner-retracted! r (snap-retracted sn))
  (set-box! (runner-ids r) (snap-ids sn)) (set-runner-outbox! r (snap-outbox sn)) (set-runner-seqs! r (snap-seqs sn))
  (set-runner-suspended?! r (snap-suspended? sn))
  (define since (reverse (take (runner-log r) (- (length (runner-log r)) (snap-position sn)))))
  (set-runner-relogging?! r #t)
  (for ([s (in-list since)]) (run-step! r s))
  (set-runner-relogging?! r #f))

;; Rebuild state from all retained input with processing time frozen and
;; outputs held; processing-time timers wait until the replay has caught up.
(define (cold-start! r)
  (flush-outbox! r)
  (define prog (runner-program r))
  (set-runner-states! r (initial-states prog)) (set-runner-pdl! r empty-deadlines)
  (set-runner-edl! r (for/hasheq ([name (in-hash-keys (program-nodes prog))]) (values name empty-deadlines)))
  (set-runner-wm! r (hasheq)) (set-runner-maxte! r (hasheq)) (set-runner-prov! r (hasheq)) (set-runner-seqs! r (hasheq))
  (set-runner-retracted! r (hasheq))
  (set-runner-outbox! r '()) (set-runner-suspended?! r #f)
  (set-runner-holding?! r #t)
  (set-runner-frozen?! r #t)
  (start-generators! r)
  (set-runner-relogging?! r #t)
  (for ([s (in-list (reverse (runner-log r)))]) (run-step! r s))
  (set-runner-relogging?! r #f)
  ;; caught up: timers resume; outputs wait for the configuration, if any
  (set-runner-frozen?! r #f)
  (fire-processing-deadlines! r (runner-now r))
  (flush-outbox! r)
  (if (for/or ([n (in-hash-values (program-nodes prog))]) (operator-config? (node-op n)))
      (set-runner-awaiting-config?! r #t)
      (release-held! r)))

;; A replayed step: data and configuration are re-read at the frozen
;; processing time; clock steps are skipped.
(define (replay-step! r s)
  (match s
    [(step:arrive _ src e) (run-input-step! r (step:arrive (runner-now r) src e))]
    [(step:control _ target msg cfg) (run-input-step! r (step:control (runner-now r) target msg cfg))]
    [(step:watermark _ _) (run-input-step! r s)]
    [_ (void)]))

;; Release what the replay held: coalesced to one per operator instance,
;; each judged against the instances in force (an output of a vanished
;; instance is dropped and counted). Sink outputs are delivered; the
;; emissions held at their node then continue downstream.
(define (release-held! r)
  (define held (reverse (runner-held r)))
  (define emissions (reverse (runner-held-emissions r)))
  (set-runner-held! r '())
  (set-runner-held-emissions! r '())
  (set-runner-holding?! r #f)
  (set-runner-awaiting-config?! r #f)
  (define (merge+judge name items)
    (define op (node-op* r name))
    (define merged (if (null? (cdr items)) (car items) ((or (operator-coalesce op) default-coalesce) items)))
    (define st (hash-ref (runner-states r) name))
    (cond
      [(or (not (operator-judge op)) ((operator-judge op) (car st) (cdr st) merged)) merged]
      [else (bump-counters! r name (list (cons 'dropped-after-replay 1))) #f]))
  ;; sink outputs of the other nodes, one per (node, key)
  (define events (filter (λ (o) (event? (output-item o))) held))
  (deliver-outputs!
   r (for*/list ([g (in-list (group-by (λ (o) (cons (output-node o) (output-key o))) events))]
                 [merged (in-value (merge+judge (output-node (car g)) (map output-item g)))]
                 #:when merged)
       (output (runner-now r) (output-node (car g)) (output-key (car g)) merged)))
  ;; emissions held at their node, one per (node, instance, port)
  (define (instance-of v)
    (define op (node-op* r (vector-ref v 0)))
    (if (operator-instance-of op) ((operator-instance-of op) (vector-ref v 3)) (vector-ref v 1)))
  (for* ([g (in-list (group-by (λ (v) (list (vector-ref v 0) (instance-of v) (vector-ref v 2))) emissions))]
         [v (in-value (car g))]
         [merged (in-value (merge+judge (vector-ref v 0) (map (λ (x) (vector-ref x 3)) g)))]
         #:when merged)
    (propagate! r (vector-ref v 0) (vector-ref v 1) (list (cons (vector-ref v 2) merged))))
  (flush-outbox! r))

;; OUT.coalesce for held outputs without an operator-specific merge.
(define (default-coalesce es)
  (define latest (last es))
  (event-update latest
                #:payload (hash-set (event-payload latest) 'merged (length es))
                #:ts (apply time-min (map event-ts es)) #:te (apply time-max (map event-te es))
                #:n (apply + (map event-n es)) #:constituents es))

(define (run prog steps
             #:clock [clock (simulated-clock 0)]
             #:final-watermark? [final-watermark? #t]
             #:delivery [delivery 'by-key]
             #:track-provenance? [track-provenance? #t])
  (define r (make-runner prog (simulated-clock-start clock) delivery track-provenance?))
  (start-generators! r)
  (for ([s (in-list steps)]) (run-step! r s))
  (when final-watermark?
    (for ([src (in-list (sort (hash-keys (program-sources prog)) symbol<?))])
      (advance-source-watermark! r src +inf.0))
    (fire-processing-deadlines! r (runner-now r))
    (flush-outbox! r))
  ;; deadlines waiting for processing time to move past the last instant
  (fire-processing-deadlines! r (runner-now r) #:drain? #t)
  (flush-outbox! r)
  (run-result (reverse (runner-outputs r)) (runner-counters r) (reverse (runner-reports r))
              (runner-now r) (runner-wm r) (runner-states r)))

;; ===========================================================================
;; Applying one operator to finite input histories (ES.operator)
;;
;; op : H₁(t) × … × Hₙ(t) → O(t). Each history is a list of events, fed in
;; order at processing time te(e) (or the given arrival times), followed by
;; the final watermark.

(define (run-operator op histories
                      #:arrivals [arrivals #f]
                      #:watermark [watermark 'ascending]
                      #:final-watermark? [final-watermark? #t])
  (define srcs (for/list ([i (in-range (length histories))])
                 (make-source (string->symbol (format "in~a" i)) #:watermark watermark)))
  (define n (make-node 'op op srcs #:sink? #t))
  (define prog (make-program (list n)))
  (define steps
    (sort (for*/list ([(h i) (in-indexed histories)] [(e j) (in-indexed h)])
            (list (if arrivals (list-ref (list-ref arrivals i) j) (event-te e)) i j
                  (arrive (if arrivals (list-ref (list-ref arrivals i) j) (event-te e))
                          (source-name (list-ref srcs i)) e)))
          (λ (a b) (or (< (car a) (car b))
                       (and (= (car a) (car b))
                            (or (< (cadr a) (cadr b))
                                (and (= (cadr a) (cadr b)) (< (caddr a) (caddr b)))))))))
  (run prog (map cadddr steps) #:final-watermark? final-watermark?))

(provide
 (contract-out
  [struct source ([name symbol?] [types (listof any/c)] [watermark any/c] [data any/c] [meta hash?])]
  [make-source (->* (symbol?) (#:types list? #:watermark any/c #:data any/c #:meta hash?) source?)]
  [struct input-ref ([upstream any/c] [port exact-nonnegative-integer?])]
  [struct node ([name symbol?] [op operator?] [inputs (listof input-ref?)] [sink? boolean?]
                [recursive (or/c #f exact-positive-integer?)])]
  [make-node (->* (symbol? operator? list?)
                  (#:sink? boolean? #:recursive (or/c #f exact-positive-integer?))
                  node?)]
  [struct program ([sources hash?] [nodes hash?] [order (listof symbol?)] [inputs hash?]
                   [downstream hash?])]
  [make-program (-> list? program?)]
  [arrive (-> time? symbol? (or/c event? retraction?) step:arrive?)]
  [advance-to (-> time? step:advance?)]
  [watermark-to (-> symbol? time? step:watermark?)]
  [control (->* (any/c) (#:at (or/c #f time?) #:to symbol? #:config? boolean?) step:control?)]
  [suspend (-> time? time? step:suspend?)]
  [checkpoint (-> step:checkpoint?)]
  [restore (-> step:restore?)]
  [cold-start (-> step:cold-start?)]
  [struct step:checkpoint ()]
  [struct step:restore ()]
  [struct step:cold-start ()]
  [struct step:arrive ([time time?] [source symbol?] [event (or/c event? retraction?)])]
  [struct step:advance ([time time?])]
  [struct step:watermark ([source symbol?] [time time?])]
  [struct step:control ([time (or/c #f time?)] [target symbol?] [msg any/c] [config? boolean?])]
  [struct step:suspend ([from time?] [to time?])]
  [struct simulated-clock ([start time?])]
  [struct output ([at time?] [node symbol?] [key any/c] [item item?])]
  [struct run-result ([outputs (listof output?)] [counters hash?] [reports list?] [now time?]
                      [watermarks hash?] [states hash?])]
  [run-result-events (->* (run-result?) ((or/c #f symbol?)) (listof event?))]
  [run-result-counter (-> run-result? symbol? symbol? exact-integer?)]
  [run (->* (program? list?)
            (#:clock simulated-clock? #:final-watermark? boolean? #:delivery (or/c 'by-key 'emission)
             #:track-provenance? boolean?)
            run-result?)]
  [run-operator (->* (operator? (listof (listof event?)))
                     (#:arrivals (or/c #f (listof (listof time?))) #:watermark any/c
                      #:final-watermark? boolean?)
                     run-result?)]))
