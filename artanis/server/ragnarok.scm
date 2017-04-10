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
  #:use-module (artanis aio)
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
(define (make-listen-fd family addr port)
  (let ((sock (socket family SOCK_STREAM 0)))
    ;; Causes the port to be released immediately after the socket is closed.
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (when (eq? 'edge (get-conf '(server trigger)))
          ;; nonblock if edge trigger
          (fcntl sock F_SETFL (logior O_NONBLOCK (fcntl sock F_GETFL 0))))
    (bind sock family addr port)
    (listen sock (get-conf '(server backlog)))
    (port->fdes sock)))

(define (generate-work-tables)
  (define (new-work-table)
    (make-work-table (make-hash-table) (make-mutex)))
  (let ((n (get-conf '(server workers))))
    (DEBUG "generating ~a work-tables~%" n)
    (map (lambda (i) (new-work-table)) (iota n))))

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
  (let* ((listen-fd (make-listen-fd family addr port))
         (listen-event (make-epoll-event listen-fd (gen-read-event)))
         (event-set (make-epoll-event-set))
         (epfd (epoll-create1 0))
         (services (make-hash-table))
         (ready-q (new-ready-queue))
         (wt (generate-work-tables)))
    (sigaction SIGPIPE SIG_IGN) ; FIXME: should we remove the related threads?
    (DEBUG "Prepare for regnarok-open~%")
    (epoll-ctl epfd EPOLL_CTL_ADD listen-fd listen-event)
    (DEBUG "Added listenning port to epoll~%")
    (make-ragnarok-server epfd listen-fd wt ready-q event-set services)))

(define (with-stack-and-prompt thunk)
  (call-with-prompt
   'serve-one-request
   (lambda () (start-stack #t (thunk)))
   (lambda (k proc)
     (DEBUG "Preparing stack for running ...~%")
     (with-stack-and-prompt (lambda () (proc k))))))

;; NOTE: make sure `client' is connect socket
(define (is-a-continuable-work? server client)
  (let* ((wt (current-work-table server))
         (task (get-task-from-work-table wt client)))
    (cond
     (task ; if task exists in work-table
      (DEBUG "Continuable work ~a" (client-ip client))
      (remove-from-work-table! wt client) ; then remove it from work-table
      task) ; and return this task for later application (restore the execution)
     (else #f)))) ; not a continuable work since it's not in the work-table

(define (run-task task)
  (call-with-prompt
   'serve-one-request
   (lambda ()
     ((task-kont task)))
   ragnarok-scheduler))

;; NOTE: We will call `accept' if it's listenning socket. Logically, it's
;;       impossible to encounter listenning socket again while polling.
(::define (fill-ready-queue-from-service proto server)
  ;; should be client list but we don't check internally.
  (:anno: (ragnarok-protocol ragnarok-server) -> ready-queue)
  (define (listenning-port? e)
    (let ((listen-socket (ragnarok-server-listen-socket server)))
      (= (car e) (port->fdes listen-socket))))
  (let ((epfd (ragnarok-server-epfd server))
        (events (ragnarok-server-event-set server))
        (timeout (get-conf '(server polltimeout)))
        (rq (ragnarok-server-ready-queue server)))
    (for-each
     (lambda (e)
       (let ((client
              (cond
               ((listenning-port? e)
                (DEBUG "New connection, fd is ~a~%"
                       (port->fdes (ragnarok-server-listen-socket server)))
                (make-ragnarok-client (accept (car e))))
               ((is-peer-shutdown? e)
                (DEBUG "Peer shutdown ~a~%" e)
                (parameterize ((must-close-connection? #t))
                  (ragnarok-close
                   proto
                   server
                   (restore-working-client server e))))
               (else (restore-working-client server e)))))
         (ready-queue-in! rq client)))
     (epoll-wait epfd events timeout))))

(define (handle-request handler request body)
  (define (request-error-handler k . e)
    ;; NOTE: We don't have to do any work here but just throw 500 exception.
    ;;       When this handler is called, it must hit the pass-keys, which
    ;;       should be silient and throw 500.
    (values (build-response #:code 500) #f '()))
  (call-with-error-handling
   (lambda ()
     (call-with-values
         (lambda ()
           (with-stack-and-prompt
            (lambda ()
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

(::define (serve-one-request handler proto server client)
  (:anno: (proc protocol ragnarok-server ragnarok-client) -> ANY)
  (define (try-to-serve-one-request)
    (call-with-values
        (lambda ()
          (DEBUG "Ragnarok: start to read client~%")
          (ragnarok-read proto server client))
      (lambda (request body)
        (DEBUG "Ragnarok: finish read client~%")
        (call-with-values
            (lambda ()
              (DEBUG "Ragnarok: new request ready~%")
              (parameterize ((current-proto proto)
                             (current-server server)
                             (current-client client))
                (cond
                 ((is-a-continuable-work? server client)
                  => (lambda (task)
                       (DEBUG "Ragnarok: continue request~%")
                       (parameterize ((current-task task))
                         (run-task task))))
                 ((not (is-listenning-socket? server client))
                  (DEBUG "Ragnarok: new request~%")
                  ;; The ready socket is listenning socket, so we need to call
                  ;; `accept' on it to get a connecting socket. And create a
                  ;; new task with this new connecting socket.
                  (let ((kont (lambda ()
                                (handle-request handler request body)))
                        (conn (client-sockport client))
                        (prio #t) ; TODO: we don't have prio yet
                        (bufsize (get-conf '(server bufsize))))
                    ;; NOTE: The block-buffer is NOT a blocking I/O. They're in totally
                    ;;       different concept. block-buffer is a mechanism to hold data in an
                    ;;       allocated memeory block. Blocking I/O means the I/O operation
                    ;;       has to stop before the data is fully received.
                    ;; NOTE: For blocking I/O, buffering is always meaningless. We're using
                    ;;       non-blocking I/O, so we need block-buffer.
                    (setvbuf conn 'block)
                    (setsockopt conn SOL_SOCKET SO_SNDBUF bufsize)
                    (parameterize ((current-task (make-task conn kont prio)))
                      (DEBUG "Run new task ~a" (client-ip client))
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
                  (throw 'artanis-err 500 try-to-serve-one-request "Can't be here!~%")))))
          (lambda (response body)
            (DEBUG "Ragnarok: write client~%")
            (ragnarok-write proto server client response body))))))

  (DEBUG "Ragnarok: enter~%")
  ;; NOTE: delimited here to limit the continuation capturing granularity.
  (call-with-prompt
   'serve-one-request
   try-to-serve-one-request
   ragnarok-scheduler))

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
      (get-one-request-from-clients proto server))
     (else
      (DEBUG "ready queue is NOT empty, get one!~%")
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
      (let ((proto (detect-client-protocol client)))
        (DEBUG "Enter main-loop, protocol is ~a~%" (ragnarok-protocol-name proto))
        (call-with-sigint
         ;; handle C-c to break the server loop properly
         (lambda ()
           (DEBUG "Prepare to serve one request ~a~%" (client-ip client))
           (serve-one-request handler http server client))
         (lambda ()
           ;; NOTE: The remote connection will be handled gracefully in ragnarok-close
           ;; NOTE: The parameters will be lost when exception raised here, so we can't
           ;;       use any of current-task/server/client/proto in the exception handler
           (DEBUG "Prepare to close connection ~a~%" (client-ip client))
           (ragnarok-close http server client)
           (values)))
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
  (:anno: (protocol ragnarok-server ragnarok-client) -> ANY)
  (DEBUG "ragnarok-read ~a~%" (client-ip client))
  ((ragnarok-protocol-read proto) server client))

(::define (ragnarok-write proto server client response body)
  ;; FIXME:
  ;; Since body could be string/bv, and we haven't supported multi-types yet,
  ;; then we just use ANY type here to ingore the body type.
  (:anno: (protocol ragnarok-server ragnarok-client response ANY) -> ANY)
  (DEBUG "ragnarok-write ~a~%" (client-ip client))
  ((ragnarok-protocol-write proto) server client response body))

;; NOTE: The parameters will be lost when exception raised here, so we can't
;;       use any of current-task/server/client/proto in this function.
(::define (ragnarok-close proto server client)
  (:anno: (protocol ragnarok-server ragnarok-client) -> ANY)
  (DEBUG "ragnarok-close ~a~%" (client-ip client))
  ((ragnarok-protocol-close proto) server client))
