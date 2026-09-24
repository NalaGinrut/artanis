;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2016-2026
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
;;  Artanis is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License and GNU
;;  Lesser General Public License published by the Free Software
;;  Foundation, either version 3 of the License, or (at your option)
;;  any later version.

;;  Artanis is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License and GNU Lesser General Public License
;;  for more details.

;;  You should have received a copy of the GNU General Public License
;;  and GNU Lesser General Public License along with this program.
;;  If not, see <http://www.gnu.org/licenses/>.

(define-module (artanis server server-context)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis server epoll)
  #:use-module (ice-9 threads)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (init-wakeup-fd!
            wakeup-fd
            wake-up-task!
            take-woken-clients!
            client-live-fd
            notify-task-done!

            task-busy-begin!
            task-busy?
            task-abandoned?
            task-abandon!
            task-busy-end!
            take-woken-items!
            take-expired-busy-clients!
            deadline-after
            deadline-passed?

            idle-watch!
            take-idle-expired!

            make-ragnarok-engine
            ragnarok-engine?
            ragnarok-engine-name
            ragnarok-engine-breaker
            ragnarok-engine-runner
            ragnarok-engine-collector
            ragnarok-engine-loader

            make-ragnarok-server
            ragnarok-server?
            ragnarok-server-epfd
            ragnarok-server-listen-socket
            ragnarok-server-work-table ; get work-table
            ragnarok-server-ready-queue
            ragnarok-server-event-set
            ragnarok-server-services
            current-work-table ; get work-table from specified server

            high-prio-task-add!
            high-prio-task-remove!
            is-high-prio-task?

            new-ready-queue
            ready-queue?
            ready-queue-empty?
            ready-queue-in!
            ready-queue-out!

            make-work-table
            work-table?
            work-table-content
            work-table-mutex

            make-ragnarok-protocol
            ragnarok-protocol?
            ragnarok-protocol-name
            ragnarok-protocol-open
            ragnarok-protocol-read
            ragnarok-protocol-write
            ragnarok-protocol-close

            make-redirector
            redirector?
            redirector-reader
            redirector-writer
            redirector-type
            redirector-remote-port
            redirector-count
            redirector-mutex
            register-redirector!
            remove-redirector!
            get-the-redirector-of-websocket
            is-proxy?

            make-task
            task?
            task-client
            task-timeout task-timeout-set!
            task-touch-time task-touch-time-set!
            task-keepalive? task-keepalive?-set!
            task-kont task-kont-set!
            task-prio task-prio-set!
            is-task-timeout?
            update-task-time!

            new-ragnarok-client
            ragnarok-client?
            oneshot-mention/fd!
            oneshot-mention!
            client-sockport
            client-sockport-descriptor
            client-connecting-port
            client-ip
            address->ip

            remove-from-work-table!
            add-a-task-to-work-table!
            get-task-from-work-table
            restore-working-client

            specified-proto?
            proto-conn-state
            register-proto!
            unregister-proto!

            current-task
            current-proto
            current-server
            current-client))

(define-record-type ragnarok-engine
  (fields
   name
   breaker
   runner
   collector
   loader))

(define-record-type ragnarok-server
  (fields
   epfd
   listen-socket
   work-table  ; a table contains continuations
   ready-queue ; a queue contains connect socket
   event-set
   services))  ; a table to hold all redirectors (int -> redirector)

;; ========== Cross-thread wake-up ==========
;; Other threads (e.g. runner workers) can't resume a Ragnarok task directly,
;; since the work-table and the ready-queue belong to the server thread. They
;; push the client to be resumed into *woken-clients*, then signal the wake-up
;; eventfd, which is registered in epoll. So the server thread returns from
;; epoll_wait, drains *woken-clients*, and puts their tasks into ready-queue.
;; The eventfd is only a doorbell, *woken-clients* tells who to wake up.
;; NOTE: We push the client rather than its fd, so that the server thread can
;;       check the task of the fd still belongs to the same client (eq?), in
;;       case the fd has been closed and reused by another connection.
;; NOTE: Only for the single-threaded server core (server.workers = 1), since
;;       the eventfd and *woken-clients* are global rather than per server.

(define *woken-mutex* (make-mutex))
(define *woken-clients* '())
(define *wakeup-fd* #f)

(define *server-epfd* #f) ; for detaching a busy connection, see defer-close!

(define (init-wakeup-fd! epfd)
  (set! *server-epfd* epfd)
  (when (not *wakeup-fd*)
    (set! *wakeup-fd* (eventfd-create)))
  *wakeup-fd*)

(define (wakeup-fd) *wakeup-fd*)

;; Each item is (client . done?). done? is #t when it's the completion of a
;; busy operation (see Busy tasks below), so Ragnarok must end the busy state.

(define (push-woken-item! client done?)
  (with-mutex *woken-mutex*
    (set! *woken-clients* (cons (cons client done?) *woken-clients*)))
  (eventfd-signal! *wakeup-fd*))

;; Safe to be called from any thread, including the server thread.
(define (wake-up-task! client)
  (push-woken-item! client #f))

;; Called by the other thread when the busy operation of the client is done.
;; Safe to be called from any thread.
(define (notify-task-done! client)
  (push-woken-item! client #t))

;; Called from the server thread when epoll reports the eventfd readable.
;; Returns the (client . done?) items pushed since last call, oldest first.
(define (take-woken-items!)
  (eventfd-drain! *wakeup-fd*)
  (with-mutex *woken-mutex*
    (let ((items *woken-clients*))
      (set! *woken-clients* '())
      (reverse items))))

(define (take-woken-clients!)
  (map car (take-woken-items!)))
;; =========== end Cross-thread wake-up ===========

;; ========== Busy tasks ==========
;; A task is busy when it's waiting for an operation running in another thread,
;; e.g. a runner. The operation runs outside the server core, but its state is
;; owned by Ragnarok, so that the task can always come back to Ragnarok:
;;  1. A busy task isn't idle, it's waiting for the server itself, so the task
;;     timeout doesn't apply. The operation may have its own deadline instead:
;;     when it has passed, Ragnarok resumes the task to let it give up waiting.
;;  2. If the task has to be closed while busy (peer shutdown, exception,
;;     timeout, etc.), the connection is detached from epoll and the
;;     work-table, and shut down, but its port is NOT closed until the
;;     operation is done, since the other thread may still hold the fd
;;     (deferred close). The task is marked as abandoned, so an operation not
;;     started yet can be skipped.
;;  3. When the operation is done, the other thread calls notify-task-done!,
;;     and Ragnarok ends the busy state: it either resumes the task, or
;;     finishes the deferred close if the task has gone.
;; Keyed by client, which is unique for each connection.
;; NOTE: Only abandoned? is read by other threads, so it's protected by
;;       *busy-mutex*. The rest is touched by the server thread only.

;; Deadlines use the monotonic internal clock, since current-time only has
;; 1-second resolution.
(define (deadline-after seconds)
  (+ (get-internal-real-time)
     (inexact->exact (round (* seconds internal-time-units-per-second)))))

(define (deadline-passed? deadline)
  (>= (get-internal-real-time) deadline))

(define-record-type busy
  (fields (mutable count)       ; in-flight operations
          (mutable deadline)    ; #f or the time to resume the task
          (mutable abandoned?)  ; the task has gone
          (mutable closing?)))  ; the close of connection is deferred

(define *busy-mutex* (make-mutex))
(define *busy-tasks* (make-hash-table)) ; client -> busy

;; Server thread only.
(define (task-busy-begin! client deadline)
  (let ((b (hashq-ref *busy-tasks* client)))
    (cond
     (b (busy-count-set! b (1+ (busy-count b)))
        (busy-deadline-set! b deadline))
     (else
      (hashq-set! *busy-tasks* client (make-busy 1 deadline #f #f))))))

(define (task-busy? client)
  (and (hashq-ref *busy-tasks* client) #t))

;; Safe to be called from any thread.
(define (task-abandoned? client)
  (with-mutex *busy-mutex*
    (and=> (hashq-ref *busy-tasks* client) busy-abandoned?)))

;; Server thread only. The task gives up waiting for the operation, e.g. its
;; deadline has passed. The operation will be skipped if it's not started.
(define (task-abandon! client)
  (and=> (hashq-ref *busy-tasks* client)
         (lambda (b)
           (with-mutex *busy-mutex*
             (busy-abandoned?-set! b #t))
           (busy-deadline-set! b #f))))

;; Server thread only. Called by remove-from-work-table! when the task is
;; closed while it's busy.
(define (defer-close! client)
  (let ((b (hashq-ref *busy-tasks* client))
        (fd (client-live-fd client)))
    (DEBUG "Defer closing busy client ~a~%" fd)
    (task-abandon! client)
    (busy-closing?-set! b #t)
    ;; Keep the fd open so that its number can't be reused while the other
    ;; thread may still hold it, but shut the connection down now: the peer
    ;; sees the end of the response immediately, and the other thread gets
    ;; EPIPE if it's still writing, which ends it early.
    (false-if-exception (shutdown (car (unbox-type client)) 2))
    ;; Some close paths don't remove the fd from epoll. It must be removed,
    ;; otherwise its next event would be regarded as an orphan fd and closed.
    (when (and fd *server-epfd*)
      (false-if-exception (epoll-ctl *server-epfd* EPOLL_CTL_DEL fd #f)))))

;; Server thread only. Called by Ragnarok on notify-task-done!.
;; Returns #t if the task is still alive and should be resumed.
(define (task-busy-end! client)
  (let ((b (hashq-ref *busy-tasks* client)))
    (cond
     ((not b) #t) ; not a busy operation, just a wake-up
     ((> (busy-count b) 1)
      (busy-count-set! b (1- (busy-count b)))
      (not (busy-closing? b)))
     (else
      (hashq-remove! *busy-tasks* client)
      (cond
       ((busy-closing? b)
        (DEBUG "Finish the deferred close of client ~a~%" (client-live-fd client))
        (high-prio-task-remove! client)
        (let ((port (car (unbox-type client))))
          (false-if-exception (close port)))
        #f)
       (else #t))))))

;; Server thread only. Returns the alive busy clients whose deadline has
;; passed. Their deadline is cleared, so each of them is returned only once.
(define (take-expired-busy-clients!)
  (hash-fold
   (lambda (client b acc)
     (let ((deadline (busy-deadline b)))
       (cond
        ((and deadline (not (busy-closing? b)) (deadline-passed? deadline))
         (busy-deadline-set! b #f)
         (cons client acc))
        (else acc))))
   '()
   *busy-tasks*))
;; =========== end Busy tasks ===========
;; ========== Idle connections ==========
;; A long-lived connection (e.g. WebSocket) is idle when its task has been
;; waiting for the peer longer than its timeout. The task timeout is only
;; checked when the task is resumed, so an idle task must be resumed by the
;; server to get its 408 (then the protocol closes it, e.g. a WebSocket close
;; 1001).
;; Lazy re-queue: there's one queue per timeout T, each item is
;; (client . time-enqueued). Items are enqueued in time order, so only the
;; head of each queue needs to be checked in each round. When an item is due
;; (enqueued T ago), the task is expired if it's idle for T now, otherwise the
;; item is enqueued again with the current time. So a task expires between T
;; and 2T after its last activity.
;; An item whose task has gone (or whose fd now belongs to another connection)
;; is just dropped, nothing has to be done when a connection is closed.
;; NOTE: Only for the single-threaded server core (server.workers = 1).
;; NOTE: Server thread only.

(define *idle-queues* (make-hash-table)) ; timeout -> queue

;; Watch the task of client, timeout is its task timeout in seconds (> 0).
(define (idle-watch! client timeout)
  (let ((q (or (hashv-ref *idle-queues* timeout)
               (let ((q (new-queue)))
                 (hashv-set! *idle-queues* timeout q)
                 q))))
    (queue-in! q (cons client (current-time)))))

;; Returns the clients whose task is idle for its timeout. They're removed
;; from the watch: the resumed task is closed by its timeout.
(define (take-idle-expired! wt)
  (let ((now (current-time))
        (tasks (work-table-content wt)))
    (define (alive-task client)
      (let* ((fd (client-live-fd client))
             (task (and fd (hashv-ref tasks fd))))
        (and task (eq? (task-client task) client) task)))
    (hash-fold
     (lambda (timeout q acc)
       (let lp ((acc acc))
         (cond
          ((queue-empty? q) acc)
          ;; The head isn't due yet, neither is the rest.
          ((< (- now (cdr (queue-head q))) timeout) acc)
          (else
           (let* ((client (car (queue-out! q)))
                  (task (alive-task client)))
             (cond
              ((not task) (lp acc))
              ;; A busy task is waiting for the server, not idle.
              ((and (not (task-busy? client)) (is-task-timeout? task))
               (lp (cons client acc)))
              (else
               (queue-in! q (cons client now))
               (lp acc))))))))
     '()
     *idle-queues*)))
;; =========== end Idle connections ===========

(define *high-prio-mutex* (make-mutex))
(define *high-prio-table* (make-hash-table))

(::define (high-prio-task-remove! client)
  (:anno: (ragnarok-client) -> ANY)
  (with-mutex
   *high-prio-mutex*
   (hash-remove! *high-prio-table* client)))

(::define (high-prio-task-add! client)
  (:anno: (ragnarok-client) -> ANY)
  (with-mutex
   *high-prio-mutex*
   (hash-set! *high-prio-table* client #t)))

(::define (is-high-prio-task? client)
  (:anno: (ragnarok-client) -> boolean)
  (hash-ref *high-prio-table* client))

(define-box-type ready-queue)

(define (new-ready-queue)
  (make-box-type ready-queue (new-queue)))

(::define (ready-queue-empty? rq)
  (:anno: (ready-queue) -> boolean)
  (queue-empty? (unbox-type rq)))

(define *ready-queue-mutex* (make-mutex))

(::define (ready-queue-in! rq v)
  (:anno: (ready-queue ragnarok-client) -> ready-queue)
  (with-mutex
   *ready-queue-mutex*
   (if (is-high-prio-task? v)
       (stack-push! (unbox-type rq) v)
       (queue-in! (unbox-type rq) v))))

(::define (ready-queue-out! rq)
  (:anno: (ready-queue) -> ANY)
  (with-mutex
   *ready-queue-mutex*
   (queue-out! (unbox-type rq))))

(::define (ready-queue-length rq)
  (:anno: (ready-queue) -> int)
  (with-mutex
   *ready-queue-mutex*
   (queue-length (unbox-type rq))))

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
  (hash-ref
   (ragnarok-server-services server)
   client))

(define (register-redirector! server client reader writer type port)
  (hash-set! (ragnarok-server-services server)
             client
             (make-redirector
              reader
              writer
              type
              port
              0
              (make-mutex))))

(define (remove-redirector! server client)
  (hash-remove! (ragnarok-server-services server)
                client))

(define (is-proxy? redirector)
  (eq? (redirector-type redirector) 'proxy))

(define-record-type task
  (fields
   client ; connecting client: <port, opt>
   (mutable touch-time) ; refresh when the connectikon is handled each time
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

(define (fd-closed? fd)
  (null? (fdes->ports fd)))

(::define (oneshot-mention/fd! fd)
  (:anno: (int) -> ANY)
  (DEBUG "oneshot-mention/fd! ~a~%" fd)
  (let* ((epfd (ragnarok-server-epfd (current-server)))
         (event (make-epoll-event fd (gen-oneshot-event))))
    (epoll-ctl epfd EPOLL_CTL_MOD fd event)))

(::define (oneshot-mention! c)
  (:anno: (ragnarok-client) -> ANY)
  (DEBUG "oneshot-mention! ~a~%" c)
  (let ((fd (client-sockport-descriptor c)))
    (oneshot-mention/fd! fd)))

;; for emacs:
;; (put '::define 'scheme-indent-function 1)

;; NOTE: The remote connection wrapped in Guile socket port.
(::define (client-sockport c)
  (:anno: (ragnarok-client) -> socket-port)
  (DEBUG "client-sockport: ~a~%" (unbox-type c))
  (let ((port (car (unbox-type c))))
    (cond
     ((port-closed? port)
      (cond
       ((preparing-quit?) port)
       (else
        (throw 'artanis-err 410 client-sockport
               "The client was closed suddenly!"))))
     (else port))))

;; Return the fd of the client, or #f if its port has been closed. Unlike
;; client-sockport-descriptor, it never throws.
(define (client-live-fd c)
  (let ((port (car (unbox-type c))))
    (and (not (port-closed? port))
         (port->fdes port))))

(::define (client-sockport-descriptor c)
  (:anno: (ragnarok-client) -> int)
  (let ((port (client-sockport c)))
    (let ((sock-port (client-sockport c)))
      (and (not (port-closed? sock-port))
           (port->fdes sock-port)))))

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
  ;; Before the fd could be closed and reused.
  (unregister-proto! client)
  (if (task-busy? client)
      ;; Another thread may still use the port, see Busy tasks.
      (defer-close! client)
      (close (client-sockport client))))

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

;; This is a table to record the protocol of a connection which is switched
;; from HTTP, e.g. WebSocket after the handshake. The key is the fd, the value
;; is (protocol . state), where state is the per-connection state owned by that
;; protocol.
;; NOTE: The entry is removed in remove-from-work-table!, every close path goes
;;       through it. So the entry can never survive its connection, otherwise
;;       a reused fd would be served by the wrong protocol.
;; NOTE: Only for the single-threaded server core (server.workers = 1).
(define *proto-conn-table* (make-hash-table))

;; Returns the protocol record, or #f for a plain HTTP connection.
(define (specified-proto? client)
  (and=> (hashv-ref *proto-conn-table* (client-sockport-descriptor client))
         car))

(define (proto-conn-state client)
  (and=> (hashv-ref *proto-conn-table* (client-sockport-descriptor client))
         cdr))

(define* (register-proto! client protoname #:optional (state #f))
  (let ((proto (lookup-protocol protoname)))
    (when (not proto)
      (throw 'artanis-err 500 'register-proto!
             "Protocol `~a' isn't registered!" protoname))
    (hashv-set! *proto-conn-table* (client-sockport-descriptor client)
                (cons proto state))))

(define (unregister-proto! client)
  (hashv-remove! *proto-conn-table* (client-sockport-descriptor client)))


;; NOTE: We need this null-task as a placeholder to let task scheduling loop
;;       work smoothly.
(define (the-null-task)
  (make-task
   #f
   #f
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
