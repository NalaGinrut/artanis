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
  #:use-module ((rnrs) #:select (put-bytevector bytevector?))
  #:export (new-http-protocol))

(define (clean-current-conn-fd server client)
  (let ((conn-fd (client-sockport-decriptor client))
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
    ;; FIXME: shutdown or close ?
    ;;(shutdown conn-fd 2) ; Stop both recv and trans
    (close conn-fd))) ; deallocate the File Descriptor

;; NOTE: Close operation must follow these steps:
;; 1. remove fd from epoll event
;; 2. close fd
;; 3. abort to the main-loop
(define (%%raw-close-connection server client)
  (clean-current-conn-fd server client)
  ;; Trigger the `abort' and back the main-loop
  (close-task))

;; NOTE: HTTP service is established by default, so it's unecessary to do any
;;       openning work.
(define (http-open server client)
  (throw 'artanis-err 500 http-open
         "This method shouldn't be called, it's likely a bug!"))

(::define (http-read server client)
  (:anno: (ragnarok-server ragnarok-client) -> (request ANY))
  (define (bad-request port)
    (write-response (build-response #:version '(1 . 1) #:code 400
                                    #:headers '((content-length . 0)))
                    port))
  (DEBUG "Enter http-open ~a~%" (client-ip client))
  (let ((port (client-sockport client)))
   (cond
    ((eof-object? (peek-char port))
     (DEBUG "Encountered EOF, closing ~a~%" (address->ip client))
     (%%raw-close-connection server client))
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
                         #f (read-request-body req))))
          (when (and is-websock? (get-conf 'debug-mode))
            (let ((ip (client-ip client)))
              (DEBUG "The websocket based client ~a is reading...~%" ip)
              (DEBUG "Just return #f body according to Artanis convention~%")))
            (values req body)))
      (lambda (k . e)
        (bad-request port)
        (close-port port)))))))

(define (keep-alive? response)
  (let ((v (response-version response)))
    (and (or (< (response-code response) 400)
             (= (response-code response) 404))
         (case (car v)
           ((1)
            (case (cdr v)
              ((1) (not (memq 'close (response-connection response))))
              ((0) (memq 'keep-alive (response-connection response)))))
           (else #f)))))

(::define (http-write server client response body)
  (:anno: (ragnarok-server rangarok-client response ANY) -> ANY)
  (cond
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
           (break-task))))
   (else
    (let* ((res (write-response response (client-sockport client)))
           (port (response-port res)))  ; return the continued port
      ;; send body to regular HTTP client
      (cond
       ((not body)) ; pass
       ((bytevector? body)
        (write-response-body res body))
       ((thunk? body) (body))
       (else
        (throw 'artanis-err 500 "1: Expected a bytevector for body" body)))
      (cond
       ((must-close-connection?)
        (%%raw-close-connection server client))
       ((keep-alive? res)
        (force-output port)
        (break-task))
       (else
        (%%raw-close-connection server client)))
      (values)))))

;; Check if the client in the redirectors table:
;; 1. In the table, just scheduled for next time.
;; 2. Not in the table, just close the connection.
(::define (http-close server client)
  (:anno: (ragnarok-server ragnarok-client) -> ANY)
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
   (else (%%raw-close-connection server client))))

(define (new-http-protocol)
  (make-ragnarok-protocol 'http http-open http-read http-write http-close))
