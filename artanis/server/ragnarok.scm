;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2016,2017,2018
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
  #:use-module (artanis page)
  #:use-module (artanis server epoll)
  #:use-module (artanis server server-context)
  #:use-module (artanis server scheduler)
  #:use-module (artanis server aio)
  #:use-module (artanis websocket named-pipe)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (system repl error-handling)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 suspendable-ports)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 format)
  ;; FIXME: Do we still need threads and mutex? Maybe someone who wants to
  ;;        write their own server core still needs it. So we keep it.
  #:use-module (ice-9 threads)
  #:use-module (rnrs bytevectors)
  #:export (establish-http-gateway
            get-task-breaker
            protocol-service-open
            get-resources-collector))

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

(define (resources-collector)
  (define (remove-timemout-connections)
    (let* ((server (current-server))
           (wt (work-table-content (current-work-table server)))
           (http (current-proto)))
      (hash-for-each
       (lambda (_ t)
         (catch 'resources-collector
           (lambda ()
             (when (is-task-timeout? t)
               (format (artanis-current-output)
                       "Collecting task ~a~%" t)
               (throw 'resources-collector 408 remove-timemout-connections
                      "The request is timeout!")))
           (lambda e
             (call-with-values
                 (lambda ()
                   (parameterize ((resources-collecting? #t))
                     (apply (make-unstop-exception-handler (exception-from-server)) e)))
               (lambda (r b s)
                 (catch #t
                   (lambda ()
                     (ragnarok-write http server (task-client t) r b #f)
                     (ragnarok-close http server (task-client t) #f))
                   (lambda _
                     ;; ignore any error since there's no resource to handle.
                     (remove-named-pipe-if-the-connection-is-websocket! (task-client t))
                     (close-current-task! server (task-client t))
                     (close (client-sockport (task-client t))))))))))
       wt)))
  ;; TODO: add more collectors
  (when (>= (get-conf '(server timeout)) 0)
    ;; If timeout is 0 then don't check timeout connections
    (remove-timemout-connections)))

;; print-work-table is only used for debugging
(define (print-work-table server)
  (let ((wt (work-table-content (current-work-table server))))
    (display "PRINT WORK TABLE\n")
    (hash-for-each
     (lambda (k v)
       (format #t "~a  :  ~a~%" k (client-sockport (task-client v))))
     wt)))

(define (no-available-port-to-allocate? e)
  (and (eq? (car e) 'system-error)
       (let ((errno (system-error-errno e)))
         (or (eqv? errno EADDRNOTAVAIL) ; no available port for connecting socket
             (eqv? errno EMFILE))))) ; Too many opened files

;; NOTE: We will call `accept' if it's listenning socket. Logically, it's
;;       impossible to encounter listenning socket again while polling.
(::define (fill-ready-queue-from-service proto server)
  ;; should be client list but we don't check internally.
  (:anno: (ragnarok-protocol ragnarok-server) -> ready-queue)
  (define (accept-them-all listen-socket)
    (let lp ((ret '()))
      (let ((fd (catch #t
                  (lambda ()
                    (accept listen-socket))
                  (lambda e
                    (cond
                     ((no-available-port-to-allocate? e)
                      (format (artanis-current-output)
                              "Ragnarok can't accept request. ~a~%"
                              (strerror (system-error-errno e)))
                      (format (artanis-current-output)
                              "Start resource collecting......")
                      (parameterize ((current-proto proto)
                                     (current-server server))
                        ;; When there's no available port for new requests,
                        ;; we call resources-collector to recycle timeout
                        ;; requests.
                        (resources-collector))
                      (format (artanis-current-output) "done~%")
                      #f)
                     (else
                      (DEBUG "Get ~a new connections.~%" (length ret))
                      #f))))))
        (if fd
            (lp (cons (new-ragnarok-client fd) ret))
            ret))))
  (define (is-listenning-fd? e)
    (DEBUG "listenning-port? ~a~%" e)
    (let ((listen-socket (ragnarok-server-listen-socket server)))
      (DEBUG "Is listen-socket? ~a ?= ~a~%" (car e) listen-socket)
      (= (car e) (port->fdes listen-socket))))
  ;;(DEBUG "Start to fill ready queue~%")
  ;;(print-work-table server)
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
                (DEBUG "New connection from listening socket ~a~%" e)
                (accept-them-all (ragnarok-server-listen-socket server)))
               ((is-peer-shutdown? e)
                => (lambda (s)
                     (DEBUG "Connecting socket ~a was shutdown!~%" e)
                     (parameterize ((must-close-connection? #t)
                                    (half-closed? s))
                       (ragnarok-close
                        proto
                        server
                        (restore-working-client (current-work-table server) (car e))
                        #t)
                       (if (half-closed?)
                           (DEBUG "Half-closed ~a~%" e)
                           (DEBUG "Full-closed ~a~%" e))
                       #f)))
               ((restore-working-client (current-work-table server) (car e))
                => (lambda (client)
                     (DEBUG "Restore working client ~a~%" e)
                     client))
               ((exists-in-epoll? (ragnarok-server-epfd server) (car e))
                (DEBUG "The fd ~a in still in epoll but not task for it, just ignore!"
                       (car e)))
               (else
                (DEBUG "The fd ~a is neither in epoll, nor task for it, just ignore!"
                       (car e))))))
         (cond
          ((list? client)
           (DEBUG "New coming connections: ~a~%" (length client))
           (for-each (lambda (c) (ready-queue-in! rq c)) client))
          ((ragnarok-client? client)
           (DEBUG "Restored client ~a~%" (client-ip client))
           (ready-queue-in! rq client))
          (else
           (DEBUG "The client ~a is ready to shutdown~%" e)))))
     (epoll-wait epfd events timeout))))

(define (handle-request handler request request-body)
  (define (request-error-handler k . e)
    ;; NOTE: We don't have to do any work here but just throw 500 exception.
    ;;       When this handler is called, it must hit the pass-keys, which
    ;;       should be silient and throw 500.
    (throw 'artanis-err 500 handle-request "Internal ERROR ~a ~a!" k e))
  (DEBUG "handle request~%")
  (call-with-error-handling
   (lambda ()
     (DEBUG "prepare the handler~%")
     (handler request request-body))
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
  (DEBUG "Register ~a as RW event~%" conn-port)
  (epoll-ctl epfd EPOLL_CTL_ADD (port->fdes conn-port)
             (make-epoll-event (port->fdes conn-port) (gen-rw-event)))
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
           (when (is-task-timeout? task)
             (DEBUG "Peer ~a timeout!~%" client)
             (throw 'artanis-err 408 serve-one-request
                    "The request is timeout!"))
           (update-task-time! task)
           (run-task task))))
   ((not (is-listenning-socket? server client))
    (DEBUG "Ragnarok: new request ~a~%" (client-sockport client))
    ;; The ready socket is listenning socket, so we need to call
    ;; `accept' on it to get a connecting socket. And create a
    ;; new task with this new connecting socket.
    (letrec ((epfd (ragnarok-server-epfd server))
             (kont (lambda ()
                     (DEBUG "Ragnarok: new task from ~a~%" (client-sockport client))
                     (call-with-values
                         (lambda ()
                           (DEBUG "Ragnarok: start to read client ~a~%" client)
                           (ragnarok-read proto server client))
                       (lambda (request body)
                         (call-with-values
                             (lambda ()
                               (let ((task (current-task)))
                                 (when (and (allow-long-live-connection?)
                                            (request-keep-alive? request))
                                   (task-keepalive?-set! task #t)))
                               (handle-request handler request body))
                           (lambda (response body request-status)
                             ;; NOTE: We provide the 3rd parameter here to keep it
                             ;;       compatible with the continuation of Guile built-in
                             ;;       server, although it's useless in Ragnarok.
                             (DEBUG "Ragnarok: write client~%")
                             (ragnarok-write proto server client response body
                                             (eq? 'HEAD (request-method request)))
                             (let ((keepalive? (and (allow-long-live-connection?)
                                                    (or (response-keep-alive? response)
                                                        (task-keepalive? (current-task))))))
                               (cond
                                ((or (eq? request-status 'exception)
                                     (not keepalive?))
                                 (parameterize ((must-close-connection? #t))
                                   (ragnarok-close proto server client #f)))
                                (else
                                 (DEBUG "Client ~a keep alive, status: ~a~%"
                                        (client-sockport client) request-status)
                                 (kont))))))))))
             (conn (client-sockport client))
             (prio #t) ; TODO: we don't have prio yet
             (bufsize (get-conf '(server bufsize)))
             (wt (current-work-table server))
             (start-time (current-time))
             (timeout (get-conf '(server timeout))))
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
                     (current-task (make-task client start-time timeout #f kont prio)))
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
  ((@ (web server) run-server)
   handler
   'http
   (list #:port (get-conf '(host port))
         #:family (get-family)
         #:addr (get-addr))))

(define (fibers-server-run handler)
  ((@ (web server) run-server)
   handler
   'fibers
   (list #:port (get-conf '(host port))
         #:family (get-family)
         #:addr (get-addr))))

(define (get-one-request-from-clients proto server)
  ;;(DEBUG "Prepare to get one request from clients, proto is ~a~%" proto)
  (let ((rq (ragnarok-server-ready-queue server)))
    (cond
     ((ready-queue-empty? rq)
      ;; if the queue is empty, filling the queue with new requests
      ;; with one epoll query.
      ;;(DEBUG "ready queue is empty~%")
      (fill-ready-queue-from-service proto server)
      (when (null? rq)
        (format (artanis-current-output)
                "Start resource collecting......")
        (parameterize ((current-proto proto)
                       (current-server server))
          ;; When there's no any request, the resources-collector will be triggered.
          ;; This is necessary, since the timeouted connections can get 408 timely.
          (resources-collector))
        (format (artanis-current-output) "done~%"))
      ;;(DEBUG "fill ready queue from service~%")
      (get-one-request-from-clients proto server))
     (else
      ;;(DEBUG "ready queue is NOT empty ~a, get one!~%" (unbox-type rq))
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
      (throw 'artanis-err 500 ragnarok-http-gateway-run
             "BUG: There should be `http' protocol at least!"))
    (let main-loop((client (get-one-request-from-clients http server)))
      (let ((proto-name (detect-client-protocol client)))
        (DEBUG "Enter main-loop, protocol is ~a~%" proto-name)
        (catch #t
          (lambda ()
            (call-with-sigint
             ;; handle C-c to break the server loop properly
             (lambda ()
               (DEBUG "Prepare to serve one request ~a~%" (client-sockport client))
               (catch 'uri-error
                 ;; NOTE: We make it very strict according to RFC 3986.
                 ;;       For example:
                 ;;       =======================3.3 Path============================
                 ;;       If a URI does not contain an authority component,
                 ;;       then the path cannot begin with two slash characters ("//").
                 ;;       ===========================================================
                 ;;       It means we will not accept the URL like:
                 ;;       http://localhost:3000//hello
                 ;;       And GNU Artanis will reply a warning page to tell
                 ;;       the client what's wrong with the URL.
                 ;;       Most mainstream server just try to make the client
                 ;;       happier even if the URL is wrong. I don't think
                 ;;       it's a good way to go. Make it strict to avoid
                 ;;       potential security risk is better.
                 (lambda ()
                   (catch 'artanis-err
                     (lambda ()
                       ;;(print-work-table server)
                       (serve-one-request handler http server client))
                     (lambda e
                       (cond
                        ((eqv? ETIMEDOUT (system-error-errno e))
                         ;; NOTE: eqv? is necessary since system-error-errno on a
                         ;;       non-system-erro will be non-integer, so don't use
                         ;;       = to check.
                         (main-loop (get-one-request-from-clients http server)))
                        (else
                         (call-with-values
                             (lambda ()
                               (apply (make-unstop-exception-handler (exception-from-server)) e))
                           (lambda (r b s)
                             (catch #t
                               (lambda ()
                                 ;; NOTE: If we come here, then it's out of the task prompt,
                                 ;;       we must hold every kind of exception here to prevent
                                 ;;       the server down. Since it's out of the valid server
                                 ;;       continuation, we have no way to manage exceptions here
                                 ;;       but only ignore them.
                                 (parameterize ((out-of-task-prompt? #t))
                                   (ragnarok-write http server client r b #f)
                                   (ragnarok-close http server client #f)))
                               (lambda e
                                 (DEBUG "An error occured outside of the task prmpt, ")
                                 (DEBUG "we have no choice but ignore it.~%")
                                 ;; NOTE: The error task must be removed here.
                                 (remove-named-pipe-if-the-connection-is-websocket! client)
                                 (close-current-task! server client)
                                 (close (client-sockport client)))))))))))
                 (lambda (k . e)
                   (call-with-values
                       (lambda ()
                         (let* ((reason (apply format #f e))
                                (warn-page (get-syspage "warn-the-client.tpl"))
                                (reason-page (string->bytevector
                                              (tpl->html warn-page (the-environment))
                                              (get-conf '(server charset))))
                                (response (artanis-sys-response
                                           400 (client-sockport client) reason-page)))
                           (values response reason-page #f)))
                     (lambda (r b s)
                       (cond
                        ((eqv? (cadr e) 408)
                         (format (artanis-current-output)
                                 "Client `~a(~a)` was timeout, closed by server!"
                                 (client-sockport client) (client-ip client))
                         (ragnarok-close http server client #f))
                        (else
                         (ragnarok-write http server client r b #f)
                         (ragnarok-close http server client #f)))))))
               (DEBUG "Serve one done~%"))
             (lambda ()
               ;; NOTE: The remote connection will be handled gracefully in ragnarok-close
               ;; NOTE: The parameters will be lost when exception raised here, so we can't
               ;;       use any of current-task/server/client/proto in the exception handler
               (DEBUG "Prepare to close connection ~a~%" (client-ip client))
               (ragnarok-close http server client #f))))
          (lambda e
            (format (artanis-current-output)
                    "Error: ~a~%" (or (and=> (system-error-errno e) strerror) e))
            (cond
             ((out-of-system-resources? e)
              (parameterize ((current-server server))
                (remove-named-pipe-if-the-connection-is-websocket! client)
                (close-current-task! server client)
                (close (client-sockport client))
                (resources-collector)))
             (else
              (format (artanis-current-output)
                      "Ingore it to avoid Ragnarok crash.~%")))))
        (DEBUG "main-loop again~%")
        (main-loop (get-one-request-from-clients http server))))))

;; NOTE: we don't schedule guile-engine, although it provides naive mechanism for scheduling.
(define (new-guile-engine)
  (define info "You're using `guile' engine, which has no scheduler yet!")
  (make-ragnarok-engine
   'guile
   (lambda () (DEBUG info))
   (lambda _ (DEBUG info))
   (lambda () (DEBUG "The `guile' engine doesn't have resources collector!"))
   guile-builtin-server-run))

(define (new-fibers-engine)
  (define info "You're using `fiber' engine, which has a managed scheduler, which is out of Artanis control!")
  (make-ragnarok-engine
   'fibers
   (lambda () (DEBUG info))
   (lambda _ (DEBUG info))
   (lambda () (DEBUG "The `fiber' engine doesn't have resources collector!"))
   fibers-server-run))

(define (new-ragnarok-engine)
  (make-ragnarok-engine
   'ragnarok
   break-task
   run-task
   resources-collector
   ragnarok-http-gateway-run))

(define-syntax-rule (get-task-breaker)
  (ragnarok-engine-breaker
   (lookup-server-engine
    (get-conf '(server engine)))))

(define-syntax-rule (get-resources-collector)
  (ragnarok-engine-collector
   (lookup-server-engine
    (get-conf '(server engine)))))

;; (server-core-name . server-engine)
(define *server-cores*
  `((ragnarok . ,(new-ragnarok-engine))
    (guile    . ,(new-guile-engine))
    (fibers   . ,(new-fibers-engine))))

(define (lookup-server-engine engine-name)
  (DEBUG "Loading server engine '~a' ...~%" engine-name)
  (let ((engine (assoc-ref *server-cores* engine-name)))
    (when (not engine)
      (format #t "Unsupported server engine: ~a~%" engine-name)
      (format #t "Artanis only support these engines: ~{~a~^,~}~%"
              (map car *server-cores*))
      (format #t
              "Feel free to discuss in artanis@gnu.org if you have new core to add!~%")
      (exit -1))
    engine))

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
             (eq? (get-conf '(server engine)) 'ragnarok))
        (DEBUG "Using Non-Blocking I/O~%")
        ;; enable global suspendable ports for non-blocking
        ;; NOTE: only for ragnarok engine, not for Guile built-in engine.
        (install-suspendable-ports!)
        (DEBUG "Installed suspendable ports~%")
        (parameterize ((current-read-waiter async-read-waiter)
                       (current-write-waiter async-write-waiter)
                       (current-encoder (if (get-conf '(db encodeparams))
                                            uri-encode
                                            identity)))
          (DEBUG "Starting `~a' engine loader ...~%" (ragnarok-engine-name engine))
          (loader handler)))
       ((eq? (get-conf '(server engine)) 'fibers)
        (DEBUG "Using managed scheduler from Fibers, out of Artanis' control~%")
        (parameterize ((current-encoder (if (get-conf '(db encodeparams))
                                            uri-encode
                                            identity)))
          (DEBUG "Starting ~a loading ...~%" (ragnarok-engine-name engine))
          (loader handler)))
       (else
        (DEBUG "Not using Non-Blocking I/O~%")
        ;; not for non-blocking I/O
        (DEBUG "Starting ~a loader ...~%" (ragnarok-engine-name engine))
        (loader handler))))
     (else (error establish-http-gateway
                  "Invalid `server.engine' in artanis.conf" (ragnarok-engine-name engine))))))

(define (make-io-exception-handler type)
  (lambda e
    (DEBUG "ragnarok-~a exception: ~a~%" type e)
    (cond
     ((io-exception:peer-is-shutdown? e)
      ;; NOTE: peer has been shutdown for reasons, we just let them be checked by epoll
      ;;       in next round loop, and close the connection by Ragnarok.
      ;; WARN: Don't call (close-task) directly, since we'll lose the chance to remove
      ;;       it from epoll. Although epoll will remove closed fd automatically, it won't
      ;;       remove the fd until it's closed completely, so does half-closed connection!
      ;;       Be careful.
      (DEBUG "Peer is shutdown, just schedule and wait for closing it later ~%")
      (break-task)
      (DEBUG "Peer ~a is still open, we close it explicitly at the end!"
             (client-sockport (current-client)))
      (close-task))
     ((io-exception:out-of-memory? e)
      ;; NOTE: out of memory, and throw 503 to let client try again. We can't just schedule
      ;;       for next round loop, since some data read from request would be lost because
      ;;       of lack of memory. The safe way is to tell the client that server is busy
      ;;       so try again.
      ;; FIXME: Is it proper to call (gc) here?
      (gc)
      (throw 'artanis-err 503 make-io-exception-handler
             "Ragnarok-~a: we run out of RAMs!~%" type))
     ((out-of-system-resources? e)
      => (lambda (reason)
           (let ((shout (format #f
                                "Out of system resources, maybe you should ~a!" reason)))
             (display (WARN-TEXT shout) (artanis-current-output))
             (resources-collector)
             (break-task))))
     (else
      (DEBUG "Not an I/O exception, throw it to upper level~%")
      ;; not a exception should be handled here, re-throw it to the next level
      (apply throw e)))))

(::define (ragnarok-read proto server client)
  (:anno: (ragnarok-protocol ragnarok-server ragnarok-client) -> ANY)
  (DEBUG "ragnarok-read ~a~%" (client-ip client))
  (catch #t
    (lambda ()
      ((ragnarok-protocol-read proto) server client))
    (make-io-exception-handler 'read)))

(::define (ragnarok-write proto server client response body method-is-head?)
  (:anno: (ragnarok-protocol ragnarok-server ragnarok-client <response> ANY boolean) -> ANY)
  (DEBUG "ragnarok-write ~a~%" (client-ip client))
  (catch #t
    (lambda ()
      ((ragnarok-protocol-write proto) server client response body method-is-head?))
    (make-io-exception-handler 'write)))

;; NOTE: The parameters will be lost when exception raised here, so we can't
;;       use any of current-task/server/client/proto in this function.
(::define (ragnarok-close proto server client peer-shutdown?)
  (:anno: (ragnarok-protocol ragnarok-server ragnarok-client boolean) -> ANY)
  (DEBUG "ragnarok-close ~a~%" (client-ip client))
  ((ragnarok-protocol-close proto) server client peer-shutdown?))
