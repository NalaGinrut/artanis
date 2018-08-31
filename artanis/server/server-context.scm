
(define-box-type ready-queue)

(define (new-ready-queue)
  (make-box-type ready-queue (new-queue)))

(::define (ready-queue-empty? rq)
  (:anno: (ready-queue) -> boolean)
  (queue-empty? (unbox-type rq)))

(::define (ready-queue-in! rq v)
  (:anno: (ready-queue) -> ready-queue)
  (queue-in! (unbox-type rq) v))

(::define (ready-queue-out! rq)
  (:anno: (ready-queue) -> ANY)
  (queue-out! (unbox-type rq)))

(::define (ready-queue-length rq)
  (:anno: (ready-queue) -> int)
  (queue-length (unbox-type rq)))

(define (current-work-table server)
  (ragnarok-server-work-table server))

;; A table contains continuations
(define-record-type work-table
  (fields
   content ; the continuation
   mutex)) ; a mutex for lock


;; NOTE: Any methods in protocol shouldn't be bound to Ragnarok.
;;       We have to make sure the developers could implement their
;;       own server-core.
(define-record-type ragnarok-protocol
  (fields name    ; the name of the protocol (in symbol)
          open    ;
          read    ; server -> client -> ANY
          write   ; server -> client -> response -> str/bv -> ANY
          close)) ; server -> client -> ANY

(define-record-type redirector
  (fields
   reader      ; the registered reader
   writer      ; the registered writer
   type        ; proxy or protocols based on websocket
   remote-port ; remote port or #f
   count       ; transfered bytes, maybe useful
   mutex))     ; a mutex for locking

;; A redirectors table holds all the redirectors as the value, and the
;; client port descriptor is the key.
;; NOTE: There're 2 kinds of redirector types:
;; 1. 'proxy
;;    For redirecting to the remote socket port.
;;    The content field will be the remote socket port.
;; 2. protocol in symbol, e.g, 'echo, 'ping
;;    For regular usage of websocket. If the http-read has detected the
;;    current client was bound to a websocket, then http-read won't read
;;    its body.
;;    The body reading will be delayed to the handler, users have to
;;    use :websocket command to read the parsed body according to the
;;    registered protocol parser.
;;    The content field is #f.
(define (make-redirectors-table) (make-hash-table))

(::define (get-the-redirector-of-websocket server client)
  (:anno: (ragnarok-server ragnarok-client) -> redirector)
  (hashq-ref
   (ragnarok-server-services server)
   client))

(define (register-redirector! server client reader writer type port)
  (hashq-set! (ragnarok-server-services server)
              client
              (make-redirector
               reader
               writer
               type
               port
               0
               (make-mutex))))

(define (remove-redirector! server client)
  (hashq-remove! (ragnarok-server-services server)
                 client))

(define (is-proxy? redirector)
  (eq? (redirector-type redirector) 'proxy))

(define-record-type task
  (fields
   client ; connecting client: <port, opt>
   (mutable touch-time) ; refresh when the connection is handled each time
   (mutable timeout) ; timeout of task
   (mutable keepalive?) ; if keep it alive
   (mutable kont) ; delimited continuation
   (mutable prio))) ; priority

(define (is-task-timeout? task)
  (let ((start-time (task-touch-time task))
        (timeout (task-timeout task)))
    (if (zero? timeout)
        #f ; timeout = 0 means disable long live connection, then no timeout at all.
        (>= (- (current-time) start-time) timeout))))

(define (update-task-time! task)
  (task-touch-time-set! task (current-time)))

(define-box-type ragnarok-client)
(define (new-ragnarok-client v)
  (DEBUG "make ragnarok client ~a~%" v)
  (make-box-type ragnarok-client v))

(::define (oneshot-mention! c)
  (:anno: (ragnarok-client) -> ANY)
  (when (not (port-closed? (client-sockport c)))
    (let* ((fd (client-sockport-descriptor c))
           (epfd (ragnarok-server-epfd (current-server)))
           (event (make-epoll-event fd (gen-oneshot-event))))
      (epoll-ctl epfd EPOLL_CTL_MOD fd event))))

;; for emacs:
;; (put '::define 'scheme-indent-function 1)

;; NOTE: The remote connection wrapped in Guile socket port.
(::define (client-sockport c)
  (:anno: (ragnarok-client) -> socket-port)
  ;;(DEBUG "client-sockport: ~a~%" (unbox-type c))
  (car (unbox-type c)))

(::define (client-sockport-descriptor c)
  (:anno: (ragnarok-client) -> int)
  (port->fdes (client-sockport c)))

(::define (client-details c)
  (:anno: (ragnarok-client) -> vector)
  (cdr (unbox-type c)))

(::define (client-fam c)
  (:anno: (ragnarok-client) -> int)
  (sockaddr:fam (client-details c)))

;; NOTE: It's actually sin_addr.s_addr, which is the remote IP.
(::define (client-addr c)
  (:anno: (ragnarok-client) -> int)
  (sockaddr:addr (client-details c)))

(::define (address->ip addr)
  (:anno: (int) -> string)
  (inet-ntop (get-family) addr))

(::define (client-ip c)
  (:anno: (ragnarok-client) -> string)
  (address->ip (client-addr c)))

;; NOTE: Different from listenning-port
;; NOTE: This is socket port, say, ip:port, don't be confused with Guile port.
(::define (client-connecting-port c)
  (:anno: (ragnarok-client) -> int)
  (sockaddr:port (client-details c)))

(::define (remove-from-work-table! wt client peer-shutdown?)
  (:anno: (work-table ragnarok-client boolean) -> ANY)
  (DEBUG "Removed task ~a~%" (client-sockport client))
  (hashv-remove! (work-table-content wt) (client-sockport-descriptor client))
  (close (client-sockport client)))

(::define (add-a-task-to-work-table! wt client task)
  (:anno: (work-table ragnarok-client task) -> ANY)
  (DEBUG "Add new task ~a == ~a~%"
         (client-sockport client) (client-sockport (task-client task)))
  (hashv-set! (work-table-content wt) (client-sockport-descriptor client) task))

(::define (get-task-from-work-table wt client)
  (:anno: (work-table ragnarok-client) -> task)
  (hashv-ref (work-table-content wt) (client-sockport-descriptor client)))

(::define (restore-working-client wt fd)
  (:anno: (work-table int) -> ragnarok-client)
  (and=> (hashv-ref (work-table-content wt) fd) task-client))

;; This is a table to record client and proto pairs (CP pairs), client is the
;; key while protocol name is the value.
;; NOTE: The CP pairs should be recorded in http-open handler,
(define *proto-conn-table* (make-hash-table))

(define (specified-proto? client)
  (and=> (hashv-ref *proto-conn-table* (client-sockport-descriptor client))
         lookup-protocol))

(define (register-proto! client protoname)
  (hashv-set! *proto-conn-table* (client-sockport-descriptor client) protoname))


;; NOTE: We need this null-task as a placeholder to let task scheduling loop
;;       work smoothly.
(define (the-null-task)
  (make-task
   "The null task client"
   #f
   (lambda () (DEBUG "A NULL-Task was called. The work table seems empty~%"))
   "The null task prio"))

;; NOTE: We can't put them in env.scm, since it uses things imported
;;       from utils.scm. But it's OK and it's better the keep them private.
;; NOTE: These parameters should only be used by these functions:
;;       1. request handler
;;          Since it's bound before calling the handler.
;;          It'll be exception when it's called outside the handler.
;;       2. call-with-abort
;;          These parameters will be unbound when it's aborted in to the
;;          scheduler. So we could use them to pass task/proto/server/client
;;          into the scheduler.
;;       3. (artanis route)
;;          It is used within the handler, so it's fine to use the parameters.
;;       4. All hooks related to request
;;          They are actually called within the handler.
(define current-task (make-parameter (the-null-task)))
(define current-proto (make-parameter (did-not-specify-parameter 'proto)))
(define current-server (make-parameter (did-not-specify-parameter 'server)))
(define current-client (make-parameter (did-not-specify-parameter 'client)))
