;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2016,2017
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
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

;; ===============================================================
;; Ragnarok is the name of generic server core of Artanis.
;;
;; NOTE: The `protocol' here doesn't mean you can define your own
;;       protocol. It must be always HTTP content-compatible protocol,
;;       say, HTTPS, HTTP/2, etc. It is only for server-core.
;; NOTE: Artanis provides arbitrary protocol parsing by websocket
;;       over HTTP. Don't mess up with `protocol' !!!
;; ===============================================================

(define-module (artanis server ragnarok)
  #:use-module (artanis env)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis server epoll)
  #:use-module (artanis server server-context)
  #:use-module (artanis server scheduler)
  #:use-module (artanis server aio)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (system repl error-handling)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 suspendable-ports)
  #:use-module (rnrs bytevectors)
  #:export (establish-http-gateway
            get-task-breaker
            protocol-service-open))

;; default socket should be nonblock
(define (make-listen-socket family addr port)
  (let ((sock (socket family SOCK_STREAM 0))
        (multi-server? (get-conf '(server multi))))
    ;; Causes the port to be released immediately after the socket is closed.
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (when (eq? 'edge (get-conf '(server trigger)))
          ;; nonblock if edge trigger
          (fcntl sock F_SETFL (logior O_NONBLOCK (fcntl sock F_GETFL 0))))
    ;; NOTE: Since Linux-3.9
    ;;       SO_REUSEPORT allows multiple sockets on the same host to bind to
    ;;       the same port, and is intended to improve the performance of multithreaded
    ;;       network server applications running on top of multicore systems.
    (when multi-server? (setsockopt sock SOL_SOCKET SO_REUSEPORT 1))
    (bind sock family addr port)
    (listen sock (get-conf '(server backlog)))
    (DEBUG "Listen socket is ~a~%" sock)
    sock))

(define (generate-work-table)
  (DEBUG "generating work-table~%")
  (make-work-table (make-hash-table) (make-mutex)))

(define *error-event* (logior EPOLLRDHUP EPOLLHUP))
(define *read-event* EPOLLIN)
(define (gen-read-event) (logior *read-event* (get-trigger)))
(define *rw-event* (logior EPOLLIN EPOLLOUT))
(define (gen-rw-event) (logior *error-event* (get-trigger) *rw-event*))
(define *write-event* EPOLLOUT)
(define (gen-write-event) (logior *error-event* (get-trigger) *write-event*))

;; Ragnarok server will establish a http-gateway, and support various protocols
;; based on websocket.
;; NOTE: Different from other ragnarok interfaces, ragnarok-open is not going to
;;       call `open' method of protocol instance. ragnarok-open is only used for
;;       establishing http-gateway.
;; NOTE: Since http service always opens, it's no need to call http-open.
(define* (ragnarok-open #:key
                        (host (get-conf '(host name)))
                        (family (get-family))
                        (addr (get-addr))
                        (port (get-conf '(host port))))
  (let* ((listen-socket (make-listen-socket family addr port))
         (listen-fd (port->fdes listen-socket))
         (listen-event (make-epoll-event listen-fd (gen-read-event)))
         (event-set (make-epoll-event-set))
         (epfd (epoll-create1 0))
         (services (make-hash-table))
         (ready-q (new-ready-queue))
         (wt (generate-work-table)))
    (DEBUG "Prepare for regnarok-open~%")
    (epoll-ctl epfd EPOLL_CTL_ADD listen-fd listen-event)
    (DEBUG "Added listenning port to epoll~%")
    (make-ragnarok-server epfd listen-socket wt ready-q event-set services)))

(define (with-stack-and-prompt thunk)
  (call-with-prompt
   'serve-one-request
   (lambda () (start-stack #t (thunk)))
   (lambda (k proc)
     (DEBUG "Preparing stack for running ...~%")
     (with-stack-and-prompt (lambda () (proc k))))))

;; NOTE: make sure `client' is connect socket
(define (is-a-continuable-work? server client)
  (DEBUG "Is continuable work ~a~%" (client-sockport client))
  (let* ((wt (current-work-table server))
         (task (get-task-from-work-table wt client)))
    (cond
     (task ; if task exists in work-table
      (DEBUG "Task ~a exists!~%" (client-sockport (task-client task)))
      (DEBUG "Continuable work ~a~%" (client-sockport client))
      ;; NOTE: Don't remove the task from work-table here, even it'll be obsoleted later.
      ;;       If there's new task caused by scheduling, then it'll be overwritten to the
      ;;       work-table.
      ;;       And if we remove it here, then we've lost necessary information to release
      ;;       it later.
      task) ; and return this task for later application (restore the execution)
     (else #f)))) ; not a continuable work since it's not in the work-table

(define (run-task task)
  (call-with-prompt
   'serve-one-request
   (task-kont task)
   ragnarok-scheduler))

(define (print-work-table server)
  (let ((wt (work-table-content (current-work-table server))))
    (display "PRINT WORK TABLE\n")
    (hash-for-each
     (lambda (k v)
       (format #t "~a  :  ~a~%" k (client-sockport (task-client v))))
     wt)))

;; NOTE: We will call `accept' if it's listenning socket. Logically, it's
;;       impossible to encounter listenning socket again while polling.
(::define (fill-ready-queue-from-service proto server)
  ;; should be client list but we don't check internally.
  (:anno: (ragnarok-protocol ragnarok-server) -> ready-queue)
  (define (is-listenning-fd? e)
    (DEBUG "listenning-port? ~a~%" e)
    (let ((listen-socket (ragnarok-server-listen-socket server)))
      (DEBUG "listen-socket: ~a = ~a~%" (car e) listen-socket)
      (= (car e) (port->fdes listen-socket))))
  (DEBUG "Start to fill ready queue~%")
  (print-work-table server)
  (let ((epfd (ragnarok-server-epfd server))
        (events (ragnarok-server-event-set server))
        (timeout (get-conf '(server polltimeout)))
        (rq (ragnarok-server-ready-queue server)))
    (for-each
     (lambda (e)
       (DEBUG "Checking event ~a~%" e)
       (let ((client
              (cond
               ((is-listenning-fd? e)
                (DEBUG "New connection ~a~%" e)
                (new-ragnarok-client (accept (ragnarok-server-listen-socket server))))
               ((is-peer-shutdown? e)
                (DEBUG "Peer shutdown ~a~%" e)
                (parameterize ((must-close-connection? #t))
                  (ragnarok-close
                   proto
                   server
                   (restore-working-client (current-work-table server) (car e))
                   #:peer-shutdown? #t)
                  (DEBUG "Closed ~a~%" e)
                  #f))
               (else
                (DEBUG "Restore working client ~a~%" e)
                (pk "restore client"(restore-working-client (current-work-table server) (car e)))))))
         (cond
          (client
           (DEBUG "start~%")
           (DEBUG "now get client ~a~%" (client-ip client))
           (DEBUG "end~%")
           (ready-queue-in! rq client))
          (else
           (DEBUG "The client ~a was shutdown~%" e)))))
     (pk "epoll-wait"(epoll-wait epfd events timeout)))))

(define (handle-request handler request body)
  (define (request-error-handler k . e)
    ;; NOTE: We don't have to do any work here but just throw 500 exception.
    ;;       When this handler is called, it must hit the pass-keys, which
    ;;       should be silient and throw 500.
    (values (build-response #:code 500) #f '()))
  (DEBUG "handle request~%")
  (call-with-error-handling
   (lambda ()
     (call-with-values
         (lambda ()
           (with-stack-and-prompt
            (lambda ()
              (DEBUG "prepare the handler~%")
              (handler request body))))
       (lambda (response body)
         (call-with-values
             (lambda ()
               (DEBUG "Ragnarok: In handler~%")
               ;; TODO: optimize sanitize-response to be faster
               (sanitize-response request response body))
           (lambda (response body)
             (DEBUG "Ragnarok: Sanitized~%")
             ;; NOTE: we return '() as the state here to keep it compatible with
             ;;       the continuation of Guile built-in server, although it's
             ;;       useless in Ragnarok.
             (values response body '()))))))
   ;; pass-keys should be the keys which will not be described in details, since
   ;; the server developers doesn't care about them. If exception key hits pass-keys,
   ;; post-error handler will be called.
   #:pass-keys '(quit interrupt)
   ;; Programs can call `batch-mode?' to see if they are running as part of a
   ;; script or if they are running interactively. REPL implementations ensure that
   ;; `batch-mode?' returns #f during their extent.
   #:on-error (if (batch-mode?) 'backtrace 'debug)
   #:post-error request-error-handler))

(define-syntax-rule (protocol-service-open)
  ((protocol-open proto) server client))

(define (is-listenning-socket? server client)
 (let ((listen-socket (ragnarok-server-listen-socket server)))
   (= (port->fdes listen-socket) (client-sockport-decriptor client))))

(define (register-connecting-socket epfd conn-port)
  (DEBUG "AAAAAAAA: register ~a as RW event~%" conn-port)
  (epoll-ctl epfd EPOLL_CTL_ADD (port->fdes conn-port)
             (make-epoll-event (port->fdes conn-port) (gen-rw-event))
             #:keep-alive? #t)
  (DEBUG "register End~%"))

(::define (serve-one-request handler proto server client)
  (:anno: (proc ragnarok-protocol ragnarok-server ragnarok-client) -> ANY)
  (DEBUG "START:  serve ~a~%" (client-sockport client))
  (cond
   ((is-a-continuable-work? server client)
    => (lambda (task)
         (DEBUG "Ragnarok: continue request~%")
         (parameterize ((current-proto proto)
                        (current-server server)
                        (current-client client)
                        (current-task task))
           (run-task task))))
   ((not (is-listenning-socket? server client))
    (DEBUG "Ragnarok: new request ~a~%" (client-sockport client))
    ;; The ready socket is listenning socket, so we need to call
    ;; `accept' on it to get a connecting socket. And create a
    ;; new task with this new connecting socket.
    (letrec ((epfd (ragnarok-server-epfd server))
             (kont (lambda ()
                     (DEBUG "Ragnarok: new task from ~a <-> ~a ~%"
                            (client-sockport client) (client-sockport (current-client)))
                     (call-with-values
                         (lambda ()
                           (DEBUG "Ragnarok: start to read client ~a~%" client)
                           (ragnarok-read proto server client))
                       (lambda (request body)
                         (call-with-values
                             (lambda ()
                               (handle-request handler request body))
                           (lambda (response body _)
                             ;; NOTE: We provide the 3rd parameter here to keep it
                             ;;       compatible with the continuation of Guile built-in
                             ;;       server, although it's useless in Ragnarok.
                             (DEBUG "Ragnarok: write client~%")
                             (ragnarok-write proto server client response body)
                             (cond
                              ((not (keep-alive? response))
                               (ragnarok-close proto server client)
                               (values))
                              (else
                               (DEBUG "Client ~a keep alive~%" (client-sockport client))
                               (break-task)
                               (DEBUG "Continue from keep-alive connection!~%")
                               (kont)))))))))
             (conn (client-sockport client))
             (prio #t) ; TODO: we don't have prio yet
          (bufsize (get-conf '(server bufsize)))
          (wt (current-work-table server)))
      ;; Register new connection socket to epoll event set, or it can't be
      ;; raised again.
      ;; NOTE: We don't add it again if the client has already been kept alive.
      (register-connecting-socket epfd conn)
      ;; NOTE: The block-buffer is NOT a blocking I/O. They're in totally
      ;;       different concept. block-buffer is a mechanism to hold data in an
      ;;       allocated memeory block. Blocking I/O means the I/O operation
      ;;       has to stop before the data is fully received.
      ;; NOTE: For blocking I/O, buffering is always meaningless. We're using
      ;;       non-blocking I/O, so we need block-buffer.
      (setvbuf conn 'block)
      (setsockopt conn SOL_SOCKET SO_SNDBUF bufsize)
      (setsockopt conn SOL_SOCKET SO_KEEPALIVE 1)
      ;; If `edge' mode, then set new connection port to non-blocking
      (when (eq? 'edge (get-conf '(server trigger)))
        (fcntl conn F_SETFL (logior O_NONBLOCK (fcntl conn F_GETFL 0))))
      ;; NOTE: Each new task will be treated just like it's aborted to be scheduled,
      ;;       and will be added to the work-table. It means new task will not run
      ;;       immediately.
      (parameterize ((current-server server)
                     (current-client client)
                     (current-proto proto)
                     (current-task (make-task client kont prio)))
        (run-task (current-task)))))
   (else
    ;; NOTE: If we come here, it means the ready socket is NEITHER:
    ;; 1. A continuable task
    ;;    The connecting socket which was registered to work-table
    ;; 2. The listenning socket
    ;;    It means a new request is on-site. It requires this
    ;;    listenning socket to be `accept' to get connecting
    ;;    socket for the actually task.
    (DEBUG "Ragnarok: Impossible to be here, maybe a BUG?~%")
    (throw 'artanis-err 500 serve-one-request "Can't be here!~%"))))

(define (guile-builtin-server-run handler)
  (apply (@ (web server) run-server) handler 'http
         (list #:port (get-conf '(host port)))))

(define (get-one-request-from-clients proto server)
  (DEBUG "Prepare to get one request from clients, proto is ~a~%" proto)
  (let ((rq (ragnarok-server-ready-queue server)))
    (cond
     ((ready-queue-empty? rq)
      ;; if the queue is empty, filling the queue with new requests
      ;; with one epoll query.
      (DEBUG "ready queue is empty~%")
      (fill-ready-queue-from-service proto server)
      (DEBUG "fill ready queue from service~%")
      (get-one-request-from-clients proto server))
     (else
      (DEBUG "ready queue is NOT empty ~a, get one!~%" (unbox-type rq))
      (ready-queue-out! rq)))))

(define (ragnarok-http-gateway-run handler)
  (define-syntax-rule (detect-client-protocol client)
    (cond
     ((specified-proto? client) => identity)
     (else 'http)))
  (DEBUG "Enter ragnarok-http-gateway-run~%")
  (let ((http (lookup-protocol 'http))
        (server (ragnarok-open)))
    ;; *** NOTE: There's no current-server and current-client here, they're bound
    ;;           in serve-one-request.
    (DEBUG "Prepare for main-loop~%")
    (when (not http)
      (throw 'artanis-err 500 "BUG: There should be `http' protocol at least!"))
    (let main-loop((client (get-one-request-from-clients http server)))
      (let ((proto-name (detect-client-protocol client)))
        (DEBUG "Enter main-loop, protocol is ~a~%" proto-name)
        (call-with-sigint
         ;; handle C-c to break the server loop properly
         (lambda ()
           (DEBUG "Prepare to serve one request ~a~%" (client-sockport client))
           (serve-one-request handler http server client)
           (DEBUG "Serve one done~%"))
         (lambda ()
           ;; NOTE: The remote connection will be handled gracefully in ragnarok-close
           ;; NOTE: The parameters will be lost when exception raised here, so we can't
           ;;       use any of current-task/server/client/proto in the exception handler
           (DEBUG "Prepare to close connection ~a~%" (client-ip client))
           (ragnarok-close http server client)))
        (DEBUG "main-loop again~%")
        (main-loop (get-one-request-from-clients http server))))))

;; NOTE: we don't schedule guile-engine, although it provides naive mechanism for scheduling.
(define (new-guile-engine)
  (define info "You're using `guile' engine, which has no scheduler yet!")
  (make-ragnarok-engine
   'guile
   (lambda () (DEBUG info))
   (lambda _ (DEBUG info))
   guile-builtin-server-run))

(define (new-ragnarok-engine)
  (make-ragnarok-engine
   'ragnarok
   break-task
   run-task
   ragnarok-http-gateway-run))

(define-syntax-rule (get-task-breaker)
  (ragnarok-engine-breaker
   (lookup-server-engine
    (get-conf '(server engine)))))

;; (server-core-name . server-engine)
(define *server-cores*
  `((ragnarok . ,(new-ragnarok-engine))
    (guile    . ,(new-guile-engine))))

(define (lookup-server-engine engine-name)
  (DEBUG "Loading server engine '~a' ...~%" engine-name)
  (assoc-ref *server-cores* engine-name))

;; '(server engine) specified server core, so if it's `ragnarok', Artanis will
;; use (artanis server ragnarok). If it's `guile' then Artanis will use
;; (web server), say, Guile built-in server.
(define (establish-http-gateway handler)
  (let* ((engine (lookup-server-engine (get-conf '(server engine))))
         (loader (ragnarok-engine-loader engine)))
    (cond
     (loader
      (cond
       ;; NOTE: Guile inner engine is not ready for non-blocking yet,
       ;;       but we provide the possibility for the future customized engine
       ;;       if users don't like rangarok (how is that possible!)
       ((and (eq? (get-conf '(server trigger)) 'edge)
             (not (eq? (get-conf '(server engine)) 'guile)))
        (DEBUG "Using Non-Blocking I/O~%")
        ;; enable global suspendable ports for non-blocking
        ;; NOTE: only for ragnarok engine, not for Guile built-in engine.         
        (install-suspendable-ports!)
        (DEBUG "Installed suspendable ports~%")
        (parameterize ((current-read-waiter async-read-waiter)
                       (current-write-waiter async-write-waiter))
          (DEBUG "Starting `~a' engine loader ...~%" (ragnarok-engine-name engine))
          (loader handler)))
       (else
        (DEBUG "Not using Non-Blocking I/O~%")
        ;; not for non-blocking I/O
        (DEBUG "Starting ~a loader ...~%" engine)
        (loader handler))))
     (else (error establish-http-gateway
                  "Invalid `server.engine' in artanis.conf" (ragnarok-engine-name engine))))))

(::define (ragnarok-read proto server client)
  (:anno: (ragnarok-protocol ragnarok-server ragnarok-client) -> ANY)
  (DEBUG "ragnarok-read ~a~%" (client-ip client))
  ((ragnarok-protocol-read proto) server client))

(::define (ragnarok-write proto server client response body)
  ;; FIXME:
  ;; Since body could be string/bv, and we haven't supported multi-types yet,
  ;; then we just use ANY type here to ingore the body type.
  (:anno: (ragnarok-protocol ragnarok-server ragnarok-client <response> ANY) -> ANY)
  (DEBUG "ragnarok-write ~a~%" (client-ip client))
  ((ragnarok-protocol-write proto) server client response body))

;; NOTE: The parameters will be lost when exception raised here, so we can't
;;       use any of current-task/server/client/proto in this function.
(::define (ragnarok-close proto server client #:key (peer-shutdown? #f))
  (:anno: (ragnarok-protocol ragnarok-server ragnarok-client boolean) -> ANY)
  (DEBUG "ragnarok-close ~a~%" (client-ip client))
  ((ragnarok-protocol-close proto) server client peer-shutdown?))
