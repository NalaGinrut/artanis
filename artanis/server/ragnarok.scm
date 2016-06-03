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
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis server)
  #:use-module (artanis server epoll)
  #:use-module (artanis server server-context)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 threads)
  #:use-module (rnrs bytevectors)
  #:use-module (foreign system)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:export (establish-http-gateway))

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

(define (%remove-from-work-table! conn)
  (hashv-remove! (work-table-content (current-worktable)) conn))

(define (%add-current-task-to-work-table! wt conn)
  (hashv-set! wt conn))

(define (current-worktable)
  (ragnarok-server-work-table ragnarok))

(define (current-task)
  (let ((wq (work-table-content (current-worktable)))
        (worker (current-worker)))
    (table-head (list-ref wq (1+ worker)))))

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
  (call-with-prompt (default-prompt-tag)
                    (lambda () (start-stack #t (thunk)))
                    (lambda (k proc)
                      (with-stack-and-prompt (lambda () (proc k))))))

;; NOTE: make sure `client' is connect socket
(define (is-continuable-work? server client)
  (let* ((wt (ragnarok-server-work-table server))
         (task (hashv-ref wt (car client))))
    (cond
     (task
      (hashv-remove! wt (car client))
      task)
     (else #f))))

(define (run-task task)
  ((task-kont task)))

(define (get-requests-from-client server)
  (let ((epfd (ragnarok-server-epfd server))
        (events (ragnarok-server-event-set server))
        (timeout (get-conf '(server polltimeout))))
    (epoll-wait epfd events timeout)))

(define (serve-one-request handler proto server client)
  (DEBUG "Ragnarok: enter~%")
  (call-with-prompt
   'serve-one-request
   (lambda ()
     (catch #t
            (lambda ()
              (call-with-values
                  (lambda () (ragnarok-read proto client))
                (lambda (client request body)
                  (DEBUG "Ragnarok: read client~%")
                  (call-with-values
                      (lambda ()
                        (DEBUG "Ragnarok: new request ready~%")
                        (cond
                         ((is-continuable-work? server client)
                          => (lambda (task)
                               (DEBUG "Ragnarok: continue request~%")
                               (run-task task)))
                         (else
                          (DEBUG "Ragnarok: handle request~%")
                          (handle-request handler request body))))
                    (lambda (response body)
                      (DEBUG "Ragnarok: write client~%")
                      (write-client proto server client response body))))))
            (lambda (k . e)
              (let ((E (get-errno e)))
                (cond
                 ((eq? k 'interrupt)
                  ;; rethrow interrupt signal
                  (throw 'interrupt))
                 ((or (= E EAGAIN) (= E EWOULDBLOCK))
                  (abort-to-prompt 'serve-one-request proto server client))
                 (else (apply throw k e)))))))
   ragnarok-scheduler))

(define (guile-builtin-server-run handler)
  (apply (@ (web server) run-server) handler 'http
         (list #:port (get-conf '(host port)))))

(define (get-one-request-from-client server)
  (let ((rq (ragnarok-server-ready-queue server)))
    (cond
     ((queue-empty? rq)
      (let ((el (get-requests-from-client server)))
        ;; TODO: el should compute priority
        (for-each (lambda (e) (queue-in! rq e)) el)
        (get-one-request-from-client server)))
     (else (queue-out! rq)))))

(define (ragnarok-http-gateway-run handler)
  (let ((http (lookup-protocol 'http))
        (server (ragnarok-open)))
    ;; handle C-c to break the server loop properly
    (call-with-sigint
     (lambda ()
       (let main-loop((client (get-one-request-from-client server)))
         (let ((proto (cond
                       ((specified-proto? client) => identity)
                       (else http))))
           (serve-one-request handler proto server client)
           (main-loop (get-one-request-from-client server)))))
     (lambda ()
       (ragnarok-close http server)
       (values)))))

(define *server-cores*
  `((ragnarok . ,ragnarok-http-gateway-run)
    (guile    . ,guile-builtin-server-run)))
  
;; '(server impl) specified server core, so if it's `ragnarok', Artanis will
;; use (artanis server ragnarok). If it's `guile' then Artanis will use
;; (web server), say, Guile built-in server.
(define (establish-http-gateway handler)
  (let* ((impl (lookup-server-impl (get-conf '(server impl))))
         (loader (assq *server-cores* impl)))
    (cond
     (loader (loader handler))
     (else (error establish-http-gateway "Invalid (server impl) in artanis.conf" impl)))))

;; (define (ragnarok-read rag proto ev)
;;   (call-with-error-handling
;;    (lambda ()
;;      (define-syntax-rule (->port ev)
;;        (let ((events (car ev))
;;              (fd (cdr ev)))
;;          (cond
;;           ((= (logand events *rw-event*) *rw-event*)
;;            (fdes->ports  
;;      ((protocol-read proto) rag (->port ev)))
;;    #:pass-keys '(quit interrupt)
;;    #:on-error (if (batch-mode?) 'backtrace 'debug)
;;    #:post-error (lambda _ (values #f #f #f))))

(define (ragnarok-clean-current-conn-fd)
  (let ((conn (task-conn (current-task)))
        (epfd (ragnarok-server-epfd ragnarok)))
    ;; NOTE:
    ;; In kernel versions before 2.6.9, the EPOLL_CTL_DEL operation required a
    ;; non-null pointer in event, even though this argument is ignored.  Since
    ;; Linux 2.6.9, event can be specified as NULL when  using  EPOLL_CTL_DEL.
    ;; Applications  that  need  to be portable to kernels before 2.6.9 should
    ;; specify a non-null pointer in event.
    ;; So, Artanis doesn't compatible with Linux 2.6.9 and before.
    (epoll-ctl epfd EPOLL_CTL_DEL conn %null-pointer)))

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

(define (ragnarok-close proto client server)
  (ragnarok-clean-current-conn-fd)
  ((protocol-close proto) client server))
