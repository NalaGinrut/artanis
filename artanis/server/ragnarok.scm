;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2016
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
;; ===============================================================

(define-module (artanis server ragnarok)
  #:use-module (artanis env)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis server epoll)
  #:use-module (artanis server server-context)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (system repl error-handling)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 threads)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:export (establish-http-gateway
            get-task-breaker))

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

(define (generate-work-table)
  (define (new-work-table)
    (make-work-table (make-hash-table) (make-mutex)))
  (let ((n (get-conf '(server workers))))
    (map (lambda (i) (new-work-table)) (iota n))))

;; NOTE: We need this null-task as a placeholder to let task scheduling loop
;;       work smoothly.
(define (the-null-task)
  (DEBUG "A NULL-Task was called. The work table seems empty~%"))

(define current-task (make-parameter the-null-task))
(define current-proto (make-parameter (did-not-specify-parameter 'proto)))
(define current-server (make-parameter (did-not-specify-parameter 'server)))
(define current-client (make-parameter (did-not-specify-parameter 'client)))

(define *proto-conn-table* (make-hash-table))

(define (specified-proto? client)
  (and=> (hashv-ref *proto-conn-table* (client-sockport-decriptor client))
         lookup-protocol))

(define (register-proto! client protoname)
  (hashv-set! *proto-conn-table* (client-sockport-decriptor client) protoname))

(define (get-trigger)
  (case (get-conf '(server trigger))
    ((edge) EPOLLET)
    ((level) 0)
    (else (throw 'artanis-err 500 "Invalid (server trigger)!"
                 (get-conf '(server trigger))))))

(define (get-family)
  (case (get-conf '(host family))
    ((ipv4) AF_INET)
    ((ipv6) AF_INET6)
    (else (throw 'artanis-err 500 "Invalid (host family)!"
                 (get-conf '(host family))))))
        
(define (get-addr)
  (let ((host (get-conf '(host addr)))
        (family (get-family)))
    (inet-pton family host)))

(define *error-event* (logior EPOLLRDHUP EPOLLHUP))
(define *read-event* EPOLLIN)
(define (gen-read-event) (logior *read-event* (get-trigger)))
(define *rw-event* (logior EPOLLIN EPOLLOUT))
(define (gen-rw-event) (logior *error-event* (get-trigger) *rw-event*))
(define *write-event* EPOLLOUT)
(define (gen-write-event) (logior *error-event* (get-trigger) *write-event*))

(define (make-ragnarok-server epfd fd work-table work-queue event-set)
  ;; TODO: should there be a global var to hold ragnarok instance?
  #t
  )

;; Ragnarok server will establish a http-gateway, and support various protocols
;; based on websocket.
(define* (ragnarok-open #:key
                        (host (get-conf '(host name)))
                        (family (get-family))
                        (addr (get-addr))
                        (port (get-conf '(host port))))
  (let* ((fd (make-listen-fd family addr port))
         (listen-event (make-epoll-event fd (gen-read-event)))
         (event-set (make-epoll-event-set))
         (epfd (epoll-create1 0)))
    (sigaction SIGPIPE SIG_IGN) ; FIXME: should we remove the related threads?
    (epoll-ctl epfd EPOLL_CTL_ADD fd listen-event)
    (make-ragnarok-server epfd fd (generate-work-tables) (new-queue) event-set)))

(define (with-stack-and-prompt thunk)
  (call-with-prompt
   'serve-one-request
   (lambda () (start-stack #t (thunk)))
   (lambda (k proc)
     (with-stack-and-prompt (lambda () (proc k))))))

;; NOTE: make sure `client' is connect socket
(define (is-a-continuable-work? server client)
  (let* ((wt (ragnarok-server-work-table server))
         (task (get-task-from-work-table wt client)))
    (cond
     (task ; if task exists in work-table
      (remove-from-work-table! wt client) ; then remove it from work-table
      task) ; and return this task for later application (restore the execution)
     (else #f)))) ; not a continuable work since it's not in the work-table

(define (break-task)
  (abort-to-prompt
   'serve-one-request
   (current-proto)
   (current-server)
   (current-client)))

(define (run-task task)
  (call-with-prompt
   'serve-one-request
   (lambda ()
     ((task-kont task)))
   ragnarok-scheduler))

(::define (get-clients-from-service server)
  ;; should be client list but we don't check internally.
  (:anno: (ragnarok-server) -> list)
  (let ((epfd (ragnarok-server-epfd server))
        (events (ragnarok-server-event-set server))
        (timeout (get-conf '(server polltimeout))))
    (map make-ragnarok-client (epoll-wait epfd events timeout))))

(define (handle-request proto client handler request body)
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

(::define (serve-one-request handler proto server client)
  (:anno: (proc protocol ragnarok-server ragnarok-client) -> ANY)
  (define-syntax-rule (protocol-service-open)
    ((protocol-open proto) server client))
  (define (try-to-serve-one-request)
    (call-with-values
        (lambda ()
          (when (not (protocol-service-is-opened? client proto))
            (protocol-service-open))
          (ragnarok-read proto server client))
      (lambda (client request body)
        (DEBUG "Ragnarok: read client~%")
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
                 (else
                  (DEBUG "Ragnarok: handle request~%")
                  (let ((kont (lambda ()
                                (handle-request proto client handler request body)))
                        (conn (client-connecting-port client))
                        (prio #t)) ; TODO: we don't have prio yet
                    (parameterize ((current-task (make-task conn kont prio)))
                      (run-task (current-task))))))))
          (lambda (response body)
            (DEBUG "Ragnarok: write client~%")
            (ragnarok-write proto server client response body))))))

  (DEBUG "Ragnarok: enter~%")
  ;; NOTE: delimited here to limit the continuation capturing granularity.
  (call-with-prompt
   'serve-one-request
   (lambda ()
     (catch #t
       try-to-serve-one-request
       (lambda (k . e)
         (let ((E (get-errno e)))
           (cond
            ((or (= E EAGAIN) (= E EWOULDBLOCK))
             (DEBUG "Ragnarok: EAGAIN, break to sleep~%")
             ;; NOTE: Any capturing should aborted to 'serve-one-request, or you
             ;;       will miss the scheduler, sometimes cause unknown crash too.
             (abort-to-prompt 'serve-one-request proto server client))
            (else (apply throw k e)))))))
   ragnarok-scheduler))

(define (guile-builtin-server-run handler)
  (apply (@ (web server) run-server) handler 'http
         (list #:port (get-conf '(host port)))))

(define (get-one-request-from-clients server)
  (let ((rq (ragnarok-server-ready-queue server)))
    (cond
     ((queue-empty? rq)
      ;; if the queue is empty, filling the queue with new requests
      ;; with one epoll query.
      (let ((el (get-clients-from-service server)))
        ;; TODO: el should compute priority
        (for-each (lambda (e) (queue-in! rq e)) el)
        (get-one-request-from-clients server)))
     (else (queue-out! rq)))))

(define (ragnarok-http-gateway-run handler)
  (let ((http (lookup-protocol 'http))
        (server (ragnarok-open)))
    ;; handle C-c to break the server loop properly
    (call-with-sigint
     (lambda ()
       (let main-loop((client (get-one-request-from-clients server)))
         (let ((proto (cond
                       ((specified-proto? client) => identity)
                       (else http))))
           (serve-one-request handler proto server client)
           (main-loop (get-one-request-from-clients server)))))
     (lambda ()
       ;; NOTE: The remote connection will be handled gracefully in ragnarok-close
       (ragnarok-close http server)
       (values)))))

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

(define *server-cores*
  `((ragnarok . ,(new-ragnaork-engine))
    (guile    . ,(new-guile-engine))))

(define (get-ragnarok-engine-loader name)
  (ragnarok-engine-loader (assoc-ref *server-cores* name)))

(define (lookup-server-engine engine-name)
  (assoc-ref *server-cores* engine-name))

;; '(server engine) specified server core, so if it's `ragnarok', Artanis will
;; use (artanis server ragnarok). If it's `guile' then Artanis will use
;; (web server), say, Guile built-in server.
(define (establish-http-gateway handler)
  (let* ((engine (lookup-server-engine (get-conf '(server engine))))
         (loader (get-ragnarok-engine-loader engine)))
    (cond
     (loader (loader handler))
     (else (error establish-http-gateway
                  "Invalid `server.engine' in artanis.conf" (ragnaork-engine-name engine))))))

(::define (ragnarok-read proto server client)
  (:anno: (protocol ragnarok-server ragnarok-client) -> ANY)
  ((protocol-read proto) server client))

(::define (ragnarok-write proto server client response body)
  ;; FIXME:
  ;; Since body could be string/bv, and we haven't supported multi-types yet,
  ;; then we just use ANY here to ingore the body type.
  (:anno: (protocol ragnarok-server ragnarok-client response ANY) -> ANY)
  ((protocol-write proto) server client response body))

(define (ragnarok-clean-current-conn-fd server)
  (let ((conn (task-conn (current-task)))
        (epfd (ragnarok-server-epfd server)))
    ;; NOTE:
    ;; In kernel versions before 2.6.9, the EPOLL_CTL_DEL operation required a
    ;; non-null pointer in event, even though this argument is ignored. Since
    ;; Linux 2.6.9, event can be specified as NULL when using EPOLL_CTL_DEL.
    ;; Applications that need to be portable to kernels before 2.6.9 should
    ;; specify a non-null pointer in event.
    ;; So, Artanis isn't compatible with Linux 2.6.9 and before.
    (epoll-ctl epfd EPOLL_CTL_DEL conn %null-pointer)
    ;; Close the connection gracefully
    (shutdown conn 2) ; Stop both recv and trans
    (close conn))) ; deallocate the File Descriptor

;; clean task from work-table
(define (ragnarok-clean-current-task)
  (define-syntax-rule (do-clean-task)
    (%remove-from-work-table! (task-conn (current-task))))
  ;; NOTE: current task is the head of work-table
  (let ((workder (get-conf '(server workers))))
    (cond
     ((= 1 workers) (do-clean-task))
     ((> workers 1) (with-mutex (work-table-mutex wq) (do-clean-task)))
     (else (throw 'artanis-err 500 "Invalid (server workers) !" workers)))))

(::define (ragnarok-close proto server)
  (:anno: (protocol ragnarok-server) -> ANY)
  (ragnarok-clean-current-conn-fd server)
  (ragnarok-clean-current-task)
  ((protocol-close proto) server))
