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

(define-module (artanis server http)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis config)
  #:use-module (artanis websocket)
  #:use-module (artanis server server-context)
  #:use-module (artanis server epoll)
  #:use-module (artanis server scheduler)
  #:use-module ((rnrs) #:select (put-bytevector bytevector? get-bytevector-n!
                                                bytevector-length make-bytevector))
  #:use-module (ice-9 iconv)
  #:export (new-http-protocol))

(define (clean-current-conn-fd server client peer-shutdown?)
  (let ((conn (client-sockport client))
        (conn-fd (client-sockport-decriptor client))
        (epfd (ragnarok-server-epfd server)))
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
      (shutdown conn 1)
      (force-output conn))))

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
         "This method shouldn't be called, it's likely a bug!"))

(::define (http-read server client)
  (:anno: (ragnarok-server ragnarok-client) -> (<request> ANY))
  (define (bad-request status port)
    (write-response (build-response #:version '(1 . 1) #:code status
                                    #:headers '((content-length . 0)))
                    port))
  (define (try-to-read-request-body req)
    (DEBUG "try to read request body ~a~%" req)
    (let ((content-length (or (request-content-length req) 0))
          (port (request-port req)))
      (cond
       ((> content-length (get-conf '(upload size)))
        (throw 'artanis-err 419 try-to-read-request-body "Entity is too large!"))
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
     (with-throw-handler
      #t
      ;; We use with-throw-handler here, it's different from catch which will
      ;; unwind the stack. That means, if you use with-throw-handler, and when
      ;; the exception occurs, the exception will throw to the innomost catch
      ;; context after the error handler of with-throw-handler returns.
      ;; This helps us to avoid unwind and re-throw exception.
      (lambda ()
        (let* ((req (read-request port))
               (is-websock? (detect-if-connecting-websocket req #f))
               (body (if is-websock?
                         #f (try-to-read-request-body req))))
          (when (and is-websock? (get-conf 'debug-mode))
            (DEBUG "websocket mode!~%")
            (let ((ip (client-ip client)))
              (DEBUG "The websocket based client ~a is reading...~%" ip)
              (DEBUG "Just return #f body according to Artanis convention~%")))
            (values req body)))
      (lambda (k . e)
        (apply format (artanis-current-output) (cadr e) (cddr e))
        (case k
          ((artanis-err)
           (bad-request (car e) port))
         (else
          ;; General 400 error
          (bad-request 400 port)))
        (%%raw-close-connection server client #f)
        (simply-quit)))))))

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
           ((redirector-writer redirector) redirector)
           (break-task)
           (http-write server client response body #f))))
   ((string? body)
    (http-write server client response
                (string->bytevector body (get-conf '(server charset)))
                method-is-head?))
   (else
    (let* ((res (write-response response (client-sockport client)))
           (port (response-port res)))  ; return the continued port
      ;; send body to regular HTTP client
      (cond
       ((not body) 'no-body) ; pass
       ((bytevector? body)
        (write-response-body res body)
        (force-output port))
       ((file-sender? body)
        ((file-sender-thunk body)))
       (else
        (throw 'artanis-err 500 "1: Expected a bytevector for body" body)))))))

;; Check if the client in the redirectors table:
;; 1. In the table, just scheduled for next time.
;; 2. Not in the table, just close the connection.
(::define (http-close server client peer-shutdown?)
  (:anno: (ragnarok-server ragnarok-client boolean) -> ANY)
  (DEBUG "http close ~a~%" (client-sockport client))
  (cond
   ((and (not (must-close-connection?))
         (get-the-redirector-of-websocket server client))
    ;; If the protocol has been registered by the client, then it means
    ;; the client enabled a special websocket-based protocol other than
    ;; HTTP. And we will not close this client, but treat it as a waiting
    ;; connection.
    => (lambda (redirector)
         (let ((type (redirector-type redirector))
               (ip (client-ip client)))
           (DEBUG "The ~a client ~a is suspending...~%" type ip)
           (break-task))))
   (else
    (DEBUG "do close connection~%")
    ;; NOTE: Don't use simply-quit here, since there's no valid installed prompt
    ;;       to be aborted from now on.
    (%%raw-close-connection server client peer-shutdown?))))

(define (new-http-protocol)
  (make-ragnarok-protocol 'http http-open http-read http-write http-close))
