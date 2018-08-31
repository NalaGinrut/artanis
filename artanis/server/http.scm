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

(define-module (artanis server http)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis config)
  #:use-module (artanis websocket)
  #:use-module (artanis websocket named-pipe)
  #:use-module (artanis server server-context)
  #:use-module (artanis server epoll)
  #:use-module (artanis server scheduler)
  #:use-module (artanis server ragnarok)
  #:use-module ((rnrs) #:select (put-bytevector
                                 bytevector?
                                 get-bytevector-n!
                                 bytevector-length make-bytevector))
  #:use-module (ice-9 format)
  #:use-module (ice-9 futures)
  #:export (new-http-protocol
            %%raw-close-connection))

(define (clean-current-conn-fd server client peer-shutdown?)
  (let ((conn (client-sockport client))
        (conn-fd (client-sockport-descriptor client))
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
          (shutdown conn 0) ; Stop receiving data
          (DEBUG "Shutdown ~a successfully~%" conn)
          (force-output conn)
          (DEBUG "Force-output ~a successfully~%" conn))
        list))
    ;; I don't care if the connection is still alive anymore, so ignore errors.
    (DEBUG "Close connection ~a from ~a.~%" (client-sockport client) (client-ip client))))

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
  (define (try-to-read-request port)
    (catch #t
      (lambda ()
        (read-request port))
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
  (define (try-to-read-request-body req)
    (DEBUG "try to read request body ~a~%" req)
    (let ((content-length (or (request-content-length req) 0))
          (port (request-port req)))
      (cond
       ((> content-length (get-conf '(upload size)))
        (DEBUG "Entity size is ~a, size limit is ~a~%"
               content-length (get-conf '(upload size)))
        (throw 'artanis-err 419 try-to-read-request-body "Entity is too large!~%"))
       ((zero? content-length) #f)
       (else (read-request-body req)))))
  (DEBUG "Enter http-read ~a~%" (client-sockport client))
  (let ((port (client-sockport client)))
    (cond
     ((eof-object? (peek-char port))
      (DEBUG "Encountered EOF, closing ~a~%" (client-sockport client))
      ;; Close it as peer-shutdown
      (%%raw-close-connection server client #t)
      (simply-quit))
     (else
      (let* ((req (try-to-read-request port))
             (need-websocket?
              ;; NOTE: This step includes handshake if it hasn't done it.
              (detect-if-connecting-websocket req server client))
             (body (if need-websocket?
                       #f (try-to-read-request-body req))))
        (cond
         (need-websocket?
          (let ((ip (client-ip client)))
            (DEBUG "Client `~a' is in Websocket mode!~%" ip)
            (DEBUG "The websocket based client ~a is reading...~%" ip)
            (DEBUG "Just return #f body according to Artanis convention~%")
            (DEBUG "[Websocket] Client `~a' is requesting Websocket service~%"
                   (client-ip client)))
          ;; NOTE: Each time the body is the content from client. The content is parsed
          ;;       from the frame in websocket-read. And the payload is parsed by the
          ;;       registered parser. Users don't have to call parser explicitly.
          (values req (websocket-read req server client)))
         (else (values req body))))))))

(::define (http-write server client response body method-is-head?)
  (:anno: (ragnarok-server ragnarok-client <response> ANY boolean) -> ANY)
  (cond
   (method-is-head?
    (DEBUG "Method is HEAD, so don't write body~%")
    (force-output (response-port (write-response response (client-sockport client))))
    (%%raw-close-connection server client #t)
    ;; NOTE: simply-quit here will be more efficient to avoid useless keep-alive connection
    ;;       and just drop the rest steps.
    (simply-quit))
   ((get-the-redirector-of-websocket server client)
    ;; If there's a redirector has been registered by the client, then it means
    ;; the client enabled a special websocket-based protocol other than
    ;; HTTP. And we will not close this client, but treat it as a waiting
    ;; connection.
    => (lambda (redirector)
         (let ((type (redirector-type redirector))
               (ip (client-ip client)))
           (DEBUG "The redirected ~a client ~a is writing...~%" type ip)
           (DEBUG "Just suspended...~%")
           (cond
            ((is-proxy? redirector)
             ;; TODO: auto proxy I/O
             ((redirector-writer redirector) redirector)
             (break-task)
             (http-write server client response body #f))
            (else
             ;; NOTE: For common websocket, http-write will wrap the response body into
             ;;       a websocket frame by websocket-write.
             (DEBUG "Common websocket writing for `~a'~%" (client-ip client))
             (websocket-write type body server client))))))
   (else
    (let* ((res (write-response response (client-sockport client)))
           (port (response-port res))) ; return the continued port
      ;; send body to regular HTTP client
      (cond
       ((not body) 'no-body) ; pass
       ((bytevector? body)
        (write-response-body res body)
        (force-output port))
       ((file-sender? body)
        (let ((fut (make-future (file-sender-thunk body))))
          (let lp ()
            (cond
             ((eq? 'done ((@@ (ice-9 futures) future-state) fut))
              (touch fut)
              (%%raw-close-connection server client #f)
              (simply-quit))
             (else
              (oneshot-mention! client)
              (break-task)
              (lp))))))
       (else
        (throw 'artanis-err 500 http-write
               "Expected a bytevector for body" body)))))))

;; Check if the client in the redirectors table:
;; 1. In the table, emit websocket closing handshake.
;; 2. Not in the table, just close the connection.
(::define (http-close server client peer-shutdown?)
  (:anno: (ragnarok-server ragnarok-client boolean) -> ANY)
  (DEBUG "http close ~a~%" (client-sockport client))
  (cond
   ((get-the-redirector-of-websocket server client)
    => (lambda (redirector)
         (let ((type (redirector-type redirector))
               (ip (client-ip client)))
           (remove-redirector! server client)
           (DEBUG "Closing `~a' client `~a' registered as websocket...~%" type ip)
           ;; NOTE:
           ;; Websocket protocol demands a closing frame when the connection is going to
           ;; close, so there's oneshot writing operation before shutdown. Then we have to
           ;; clean websocket before actual shutdown
           (remove-named-pipe-if-the-connection-is-websocket! client)
           (cond
            ((eq? 'half-write (half-closed?))
             ;; NOTE: We have to give it one more chance to finish the reading.
             ;;       So we can't actually close it here.
             (DEBUG "Half-write websocket ~a from ~a~%"
                    (client-sockport client) (client-ip client))
             (closing-websocket-handshake server client peer-shutdown?))
            ((eq? 'half-read (half-closed?))
             (DEBUG "Half-read websocket ~a from ~a~%"
                    (client-sockport client) (client-ip client)))
            (else
             ;; full-closed
             (DEBUG "Full-closed websocket ~a from ~a~%"
                    (client-sockport client) (client-ip client))
             (%%raw-close-connection server client peer-shutdown?))))))
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
             (client-sockport client) (client-ip client))
      (%%raw-close-connection server client peer-shutdown?))))))

(define (new-http-protocol)
  (make-ragnarok-protocol 'http http-open http-read http-write http-close))
