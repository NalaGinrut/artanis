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

(define-module (artanis server http)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis config)
  #:use-module (artanis runner)
  #:use-module (artanis websocket)
  #:use-module (artanis server server-context)
  #:use-module (artanis server epoll)
  #:use-module (artanis server scheduler)
  #:use-module (artanis server ragnarok)
  #:use-module (artanis server request)
  #:use-module ((ice-9 textual-ports) #:select (put-string))
  #:use-module ((web response) #:select (response-transfer-encoding))
  #:use-module ((ice-9 iconv) #:select (string->bytevector))
  #:use-module ((rnrs) #:select (put-bytevector
                                 bytevector?
                                 get-bytevector-n!
                                 bytevector-length make-bytevector))
  #:use-module (ice-9 format)
  #:use-module (ice-9 futures)
  #:use-module ((ice-9 suspendable-ports)
                #:select (current-read-waiter current-write-waiter))
  #:use-module (system foreign)
  #:export (new-http-protocol
            %%raw-close-connection
            http-close))

(define (clean-mmapped-file fd)
  (cond
   ((is-mmapped-file? fd)
    => (lambda (bv)
         (DEBUG "munmap file for ~a~%" fd)
         (munmap (bytevector->pointer bv) (bytevector-length bv))))
   (else #f)))

(define (clean-current-conn-fd server client peer-shutdown?)
  (let ((conn-fd (if (eq? client 'init-now-client)
                     #f
                     (client-sockport-descriptor client))))
    (cond
     ((and (not conn-fd) (preparing-quit?))
      (DEBUG "The socket had closed however we're preparing close!")
      #f)
     (else
      (let ((conn (client-sockport client))
            (epfd (ragnarok-server-epfd server)))
        (DEBUG "Clean connected fd ~a~%" conn-fd)
        ;; NOTE:
        ;; In kernel versions before 2.6.9, the EPOLL_CTL_DEL operation required a
        ;; non-null pointer in event, even though this argument is ignored. Since
        ;; Linux 2.6.9, event can be specified as NULL when using EPOLL_CTL_DEL.
        ;; Applications that need to be portable to kernels before 2.6.9 should
        ;; specify a non-null pointer in event.
        ;; So, Artanis isn't compatible with Linux 2.6.9 and before.
        (epoll-ctl epfd EPOLL_CTL_DEL conn-fd #f) ; #f means %null-pointer here
        ;; Close the connection gracefully
        ;; NOTE: `shutdown' is preferred here to stop receiving data.
        ;;       Then try to send all the rest data.
        ;; NOTE: We can't just close it here, if we do so, then we've lost the information
        ;;       to get fd from port which is the key to remove task from work-table.
        (when (not peer-shutdown?)
          (DEBUG "Peer is not shutdown, let me close it for an end~%")
          (catch #t
            (lambda ()
              (shutdown conn 0)       ; Stop receiving data
              (DEBUG "Shutdown ~a successfully~%" conn)
              (force-output conn)
              (clean-mmapped-file conn-fd)
              (DEBUG "Force-output ~a successfully~%" conn))
            list))
        ;; I don't care if the connection is still alive anymore, so ignore errors.
        (DEBUG "Close connection ~a from ~a.~%" (client-sockport client) (client-ip client)))))))

;; NOTE: Close operation must follow these steps:
;; 1. remove fd from epoll event
;; 2. close fd
;; 3. abort to the main-loop
(define (%%raw-close-connection server client peer-shutdown?)
  (DEBUG "clean current client ~a is closed: ~a~%" (client-sockport client)
         (port-closed? (client-sockport client)))
  (clean-current-conn-fd server client peer-shutdown?)
  ;; ((@@ (artanis server ragnarok) print-work-table) server)
  ;; clean from work-table
  (close-current-task! server client peer-shutdown?))

;; NOTE: HTTP service is established by default, so it's unecessary to do any
;;       openning work.
(define (http-open server client)
  (throw 'artanis-err 500 http-open
         "This method shouldn't be called, it's very likely a bug somewhere!"))

(::define (http-read server client)
  (:anno: (ragnarok-server ragnarok-client) -> (<request> ANY))
  (define (bad-request status port)
    (write-response (build-response #:version '(1 . 1) #:code status
                                    #:headers '((content-length . 0)))
                    port))
  ;; Returns the request and the size of its head, see read-request-head.
  (define (try-to-read-request port)
    (catch #t
      (lambda ()
        (read-request-head port (get-conf '(upload size))))
      (lambda (k . e)
        (case k
          ;; TODO: how about bad-response for proxy?
          ((http-version bad-header bad-header-component bad-request)
           (DEBUG "ERROR in read-request: ~a~%" e)
           (bad-request 400 port)
           (%%raw-close-connection server client #f)
           (simply-quit))
          (else
           (apply throw k e))))))
  ;; upload.size limits the whole request, the head and the body.
  ;; The body is framed by Content-Length or chunked, see
  ;; request-body-framing.
  (define (try-to-read-request-body req head-size)
    (DEBUG "try to read request body ~a~%" req)
    (let ((framing (request-body-framing req))
          (limit (- (get-conf '(upload size)) head-size)))
      (cond
       ((eq? framing 'chunked)
        (read-chunked-body (request-port req) limit))
       ((> framing limit)
        (DEBUG "Entity size is ~a, size limit is ~a~%"
               framing limit)
        ;; 413 Content Too Large (RFC 9110 15.5.14). The body isn't read, so
        ;; the connection is closed after the response.
        (throw 'artanis-err 413 try-to-read-request-body
               "Content is too large!~%"))
       ((zero? framing) #f)
       (else (read-request-body req)))))
  ;; The request must be read within server.readtimeout, see
  ;; io-deadline-set!. When the task waits for the peer, it's resumed by
  ;; the data, or by the deadline.
  (define read-timeout (get-conf '(server readtimeout)))
  (define body? #f)
  (define (arm-deadline!)
    (when (> read-timeout 0)
      (io-deadline-set! client read-timeout)))
  (define (deadline-waiter port)
    (break-task)
    (cond
     ((io-deadline-passed? client)
      (io-deadline-clear! client)
      (throw 'artanis-err 408 http-read
             "The request isn't read in ~a seconds" read-timeout))
     ;; The body may take long, it must just not stop coming.
     ((and body? (char-ready? port))
      (arm-deadline!))))
  (define (read-with-deadline thunk)
    (parameterize ((current-read-waiter deadline-waiter))
      (thunk)))
  (define first-request? (not (task-keepalive? (current-task))))
  (DEBUG "Enter http-read ~a~%" (client-sockport client))
  ;; A new connection must send its request in time. A kept-alive one may
  ;; wait for its next request, until server.timeout.
  (when first-request? (arm-deadline!))
  (let ((port (client-sockport client)))
    (cond
     ((eof-object? (read-with-deadline (lambda () (peek-char port))))
      (DEBUG "http-read: Encountered EOF, closing ~a~%"
             (client-sockport client))
      ;; Close it as peer-shutdown
      (%%raw-close-connection server client #t)
      (simply-quit))
     (else
      (unless first-request? (arm-deadline!))
      (call-with-values
          (lambda () (read-with-deadline (lambda () (try-to-read-request port))))
        (lambda (req head-size)
          (io-deadline-clear! client)
          (let ((ws-state (and (not (eof-object? req))
                               (detect-if-connecting-websocket req port))))
            (cond
             ((eof-object? req)
              ;; Only empty lines before EOF, close it as peer-shutdown.
              (%%raw-close-connection server client #t)
              (simply-quit))
             ((websocket-conn? ws-state)
              ;; The 101 response has been sent. Switch the connection to the
              ;; `websocket' protocol, then Ragnarok will select it for this
              ;; connection from now on. The handshake has no message for the
              ;; handler, so return the first message instead.
              (register-proto! client 'websocket ws-state)
              (let ((ws (specified-proto? client)))
                ((ragnarok-protocol-open ws) server client)
                ((ragnarok-protocol-read ws) server client)))
             ((eq? ws-state 'rejected)
              ;; The HTTP error has been sent.
              (%%raw-close-connection server client #f)
              (simply-quit))
             (else
              (set! body? #t)
              (arm-deadline!)
              (let ((body (read-with-deadline
                           (lambda ()
                             (try-to-read-request-body req head-size)))))
                (io-deadline-clear! client)
                (values req body)))))))))))

;; The end of a chunked body: the last chunk and no trailer (RFC 9112 7.1).
(define *last-chunk* "0\r\n\r\n")

;; Write a body-stream, see response-emit-stream. Each piece is a chunk if the
;; response is chunked, otherwise it's written as it is, and the connection is
;; closed at the end. Each piece is flushed at once.
;; The writes suspend the task when the client doesn't read, so the producer
;; waits for it. If anything goes wrong once the headers are sent, e.g. the
;; client has gone or the producer throws, the connection is closed without
;; the end of the body, so the client knows it's incomplete.
(define (write-body-stream server client res body)
  (define port (response-port res))
  (define chunked? (member '(chunked) (response-transfer-encoding res)))
  (define charset (get-conf '(server charset)))
  (define (send x)
    (let ((bv (cond
               ((bytevector? x) x)
               ((string? x) (string->bytevector x charset))
               (else (throw 'artanis-err 500 send
                            "Expect a string or a bytevector, but got ~a" x)))))
      ;; An empty chunk would end the body.
      (unless (zero? (bytevector-length bv))
        (when chunked?
          (put-string port (number->string (bytevector-length bv) 16))
          (put-string port "\r\n"))
        (put-bytevector port bv)
        (when chunked? (put-string port "\r\n"))
        (force-output port))))
  (catch #t
    (lambda ()
      ;; The status and the headers go out at once, the client knows the
      ;; request is taken before the first piece, which may take a while
      ;; (e.g. the first token of an LLM).
      (force-output port)
      ((body-stream-producer body) send)
      (when chunked?
        (put-string port *last-chunk*))
      (force-output port))
    (lambda (k . e)
      (format (artanis-current-output)
              "The body stream to ~a is broken: ~a ~a~%" (client-ip client) k e)
      (%%raw-close-connection server client #f)
      (simply-quit))))

;; The write waiter of http-write. When a write would block, the client
;; doesn't read what has been sent: the task waits for it, but at most
;; server.writetimeout each time, see io-deadline-set!. It's not refreshed by
;; every byte the client reads, only by a write which has been done, so a
;; client reading a byte now and then holds it for that long at most.
;; Otherwise the connection is dropped: nothing more can be sent, not even an
;; error, the client has a part of the response.
(define (write-deadline-waiter server client)
  (define timeout (get-conf '(server writetimeout)))
  (lambda (port)
    (when (> timeout 0)
      (io-deadline-set! client timeout))
    (break-task)
    (cond
     ((io-deadline-passed? client)
      (io-deadline-clear! client)
      (format (artanis-current-output)
              "Client ~a doesn't read the response in ~a seconds, drop it~%"
              (client-ip client) timeout)
      ;; The data waiting in the port can't be sent, so the flush when the
      ;; port is closed fails at once, instead of waiting for the client.
      (catch #t (lambda () (shutdown port 2)) (const #f))
      (%%raw-close-connection server client #f)
      (simply-quit))
     (else (io-deadline-clear! client)))))

(::define (http-write server client response body method-is-head?)
  (:anno: (ragnarok-server ragnarok-client <response> ANY boolean) -> ANY)
  (parameterize ((current-write-waiter (write-deadline-waiter server client)))
    (%http-write server client response body method-is-head?)))

(define (%http-write server client response body method-is-head?)
  (cond
   (method-is-head?
    (DEBUG "Method is HEAD, so don't write body~%")
    (force-output (response-port (write-response
                                  (response-with-standard-phrase response)
                                  (client-sockport client))))
    (%%raw-close-connection server client #t)
    ;; NOTE: simply-quit here will be more efficient to avoid useless keep-alive connection
    ;;       and just drop the rest steps.
    (simply-quit))
   (else
    (let* ((res (write-response (response-with-standard-phrase response)
                                (client-sockport client)))
           (port (response-port res))) ; return the continued port
      ;; send body to regular HTTP client
      (cond
       ((not body) 'no-body) ; pass
       ((bytevector? body)
        (write-response-body res body)
        (force-output port))
       ((file-sender? body)
        ;; No runner timeout: the runner writes to the client socket itself.
        (call-with-runner (file-sender-thunk body) #:timeout 0))
       ((body-stream? body)
        (write-body-stream server client res body))
       (else
        (throw 'artanis-err 500 http-write
               "Expected a bytevector for body" body)))))))

;; WebSocket connections are closed by the `websocket' protocol, see ws-close
;; in (artanis server websocket), they never come here.
(::define (http-close server client peer-shutdown?)
  (:anno: (ragnarok-server ragnarok-client boolean) -> ANY)
  (DEBUG "http close ~a~%" (client-sockport client))
  (cond
   ((preparing-quit?) #t)
   (else
    (DEBUG "do close connection~%")
    ;; NOTE: Don't use simply-quit here, since there's no valid installed prompt
    ;;       to be aborted from now on.
    (cond
     ((half-closed?)
      (DEBUG "Half-closed connection ~a from ~a~%"
             (client-sockport client) (client-ip client)))
     (else
      (DEBUG "Full-closed connection ~a from ~a~%"
             (client-sockport client) (client-ip client))))))
  (%%raw-close-connection server client peer-shutdown?))

(define (new-http-protocol)
  (make-ragnarok-protocol 'http http-open http-read http-write http-close))
