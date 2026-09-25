;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2026
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

;; The `websocket' ragnarok-protocol.
;;
;; After the 101 response, http-read registers the connection to this
;; protocol in the proto table, and Ragnarok selects read/write/close by the
;; table on each call. So the task of the connection is the reader of it:
;; each round of the task reads one message, calls the route handler, and
;; writes the reply. When the task reads half a frame, the suspendable port
;; breaks the task, so it never blocks the server.
;;
;; After the 101 response, any error on the connection is a close frame and
;; a TCP close, never an HTTP response:
;;  - a protocol error of the peer: the close code of the error;
;;  - an error of the handler: mapped from the HTTP status, see
;;    status->close-code (5xx is 1011);
;;  - a timeout, a resource recycling, etc.: 1001 (Going Away).
;; If the connection is closed in the middle of an outgoing frame, nothing
;; more is written, not even the close frame, the TCP connection is dropped.
;;
;; LAYER-2 STOPGAP: The route handler is called once per message through the
;; regular HTTP path (handle-request with the handshake request, route
;; matching, OHT options, rc). The target (layer 4) is that the HTTP route is
;; only for the handshake, and messages are dispatched directly. What goes
;; away then is marked with "LAYER-2 STOPGAP" below.

(define-module (artanis server websocket)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis websocket)
  #:use-module (artanis websocket frame)
  #:use-module (artanis websocket named-pipe)
  #:use-module (artanis server server-context)
  #:use-module (artanis server scheduler)
  #:use-module (artanis server http)
  #:use-module ((artanis config) #:select (get-conf))
  #:use-module ((artanis session) #:select (session-restore))
  #:use-module ((ice-9 suspendable-ports) #:select (current-read-waiter))
  #:use-module (ice-9 format)
  #:use-module ((rnrs) #:select (bytevector? bytevector-length))
  #:export (new-websocket-protocol
            status->close-code))

(define (log-ws client fmt . args)
  (format (artanis-current-output) "[WebSocket] Client `~a': ~a~%"
          (client-ip client) (apply format #f fmt args)))

;; Map an HTTP status of an error response to a close code.
;; #f means no close frame is sent for it here: 408 is a timeout, ws-close
;; sends 1001 for it.
;; LAYER-2 STOPGAP: the handler error only reaches ws-write as a rendered HTTP
;;                  error response. With direct dispatch (layer 4), the
;;                  exception is caught at the call site and mapped there.
(define (status->close-code status)
  (cond
   ((= status 408) #f)
   ((>= status 500) 1011)
   ((memv status '(401 403)) 1008)
   ((= status 413) 1009)
   (else 1008)))

;; Send a close frame as far as it's possible. It never throws.
(define (send-close! port code)
  (cond
   ((websocket-output-closed? port) #t)
   ((websocket-frame-in-flight? port)
    ;; The peer has half a frame, a close frame would be parsed as its data.
    (DEBUG "A frame is in flight on ~a, drop the close frame~%" port)
    (websocket-output-close! port))
   (else
    (catch #t
      (lambda () (send-websocket-close port #:code code))
      (lambda e
        (DEBUG "Failed to send close frame ~a: ~a~%" code e)
        (websocket-output-close! port))))))

;; End the task of the connection: close it and leave the task.
;; NOTE: Only within the task, since simply-quit aborts to the task prompt.
(define (end-connection! server client peer-shutdown?)
  (ws-close server client peer-shutdown?)
  (simply-quit))

;; Fail the connection with a close code, see end-connection!.
(define (fail-connection! server client code)
  (let ((port (client-sockport client)))
    (cond
     ((= code 1006)
      ;; The peer is gone, nothing can be written anymore.
      (websocket-output-close! port)
      (end-connection! server client #t))
     (else
      (send-close! port code)
      (end-connection! server client #f)))))

;; NOTE: Called by http-read right after the connection is switched to this
;;       protocol, within the task.
;; ---------------------------------------------------------------------------
;; Session check
;;
;; A connection authenticated with a session at its handshake is closed with
;; 1008 once the session isn't valid anymore (expired, logged out, ...). The
;; session is checked every session lifetime (cookie.expires, the expiration
;; of new sessions), so it's closed at most one period after the session is
;; gone. The check is done within the task, since the session backend may do
;; I/O which suspends the task:
;;  - A timed watch (recheck-watch!) resumes the task every period, even if
;;    the peer sends nothing. The task is waiting in ws-read-waiter then.
;;  - ws-read also checks before each message, for a peer that never lets
;;    the task wait.
;; NOTE: The waiter only breaks the task to the Ragnarok scheduler, as
;;       async-read-waiter does, there's no other scheduler.

(define (recheck-period)
  (get-conf '(cookie expires)))

;; Throws 'websocket-err 1008 if the session is gone.
(define (ws-recheck! state)
  (let ((sid (websocket-state-sid state))
        (period (recheck-period)))
    (when (and sid
               (> period 0)
               (>= (- (current-time) (websocket-state-checked-at state))
                   period))
      ;; Set first: if the backend suspends the task, this waiter is called
      ;; again, then it just breaks the task.
      (websocket-state-checked-at-set! state (current-time))
      (let ((session (session-restore sid)))
        ;; Same outcomes as the check of #:session.
        (case session
          ((expired not-found)
           (throw 'websocket-err 1008 'ws-recheck!
                  "The session is ~a" session))
          (else #t))))))

;; The read waiter of a WebSocket connection. Guile's suspendable ports call
;; the current read waiter when a read would block, ignore what it returns,
;; and retry the read after it returns (see read-bytes in
;; (ice-9 suspendable-ports)). So, like async-read-waiter:
;;  1. break-task aborts to the Ragnarok scheduler, which saves the task.
;;  2. When Ragnarok resumes the task (data from the peer, or the timed
;;     watch), break-task returns here.
;; Then the session is checked before the read is retried. If it's gone,
;; ws-recheck! throws out of the read to the catch in ws-read.
(define (ws-read-waiter state)
  (lambda (port)
    (break-task)
    (ws-recheck! state)))

(define (ws-open server client)
  (let* ((state (proto-conn-state client))
         (req (websocket-state-request state))
         (timeout (websocket-rule-timeout (websocket-state-rule state))))
    (DEBUG "ws-open ~a, timeout ~a~%" (client-ip client) timeout)
    ;; The connection is long-lived, its task timeout is the idle timeout.
    (task-timeout-set! (current-task) timeout)
    (when (> timeout 0)
      (idle-watch! client timeout))
    (when (and (websocket-state-sid state) (> (recheck-period) 0))
      (recheck-watch! client (recheck-period)))
    (catch #t
      (lambda ()
        (run-hook *after-websocket-handshake-hook* req client))
      (lambda (k . e)
        (log-ws client "after-websocket-handshake hook failed: ~a ~a" k e)
        (fail-connection! server client 1011)))))

;; Read one message. Returns (values request message) for handle-request,
;; message is a websocket-frame of a whole text or binary message.
;; LAYER-2 STOPGAP: the handshake request is returned as the request of each
;;                  message, so that handle-request can route it to the
;;                  handler of the WebSocket route.
(define (ws-read server client)
  (let ((port (client-sockport client))
        (state (proto-conn-state client)))
    (when (websocket-output-closed? port)
      ;; We've sent a close frame, e.g. for an error of the handler.
      (end-connection! server client #f))
    (let ((msg (catch 'websocket-err
                 (lambda ()
                   (ws-recheck! state)
                   (parameterize ((current-read-waiter (ws-read-waiter state)))
                     (read-websocket-message port)))
                 (lambda (k code thrower fmt . args)
                   (log-ws client "failed with close code ~a: ~a"
                           code (apply format #f fmt args))
                   (fail-connection! server client code)))))
      (cond
       ((= #x8 (websocket-frame-opcode msg))
        ;; The close frame of the peer, it has been replied by the control
        ;; handler of read-websocket-message.
        (DEBUG "Received close frame from ~a~%" (client-ip client))
        (end-connection! server client #f))
       (else
        (websocket-state-reply-type-set! state (websocket-frame-type msg))
        (values (websocket-state-request state) msg))))))

;; Write the reply of the handler.
;; LAYER-2 STOPGAP: the handler's return value arrives as an HTTP response
;;                  rendered by handler-render:
;;  - An error status is mapped to a close code (see status->close-code),
;;    the body is dropped.
;;  - An empty body means no reply.
;;  - Otherwise the body is sent as one message. A string has been encoded by
;;    handler-render with server.charset, so the type (text or binary) can't
;;    be told from the body: the reply has the type of the received message.
;; NOTE: It may be called out of the task prompt (the error branches of
;;       main-loop), so it never aborts to the prompt. If it has sent a close
;;       frame, the connection is ended by ws-read or ws-close.
(define (ws-write server client response body method-is-head?)
  (let ((port (client-sockport client))
        (status (response-code response)))
    (cond
     ((>= status 400)
      (let ((code (status->close-code status)))
        (log-ws client "error status ~a, close code ~a" status code)
        (when code (send-close! port code))))
     ((not (bytevector? body))
      (log-ws client "can't send a body of `~a', close with 1011" body)
      (send-close! port 1011))
     ((zero? (bytevector-length body))
      (DEBUG "Empty reply to ~a, nothing is sent~%" (client-ip client)))
     (else
      (catch 'websocket-err
        (lambda ()
          (write-websocket-message
           port
           (let ((state (proto-conn-state client)))
             (if state (websocket-state-reply-type state) 'text))
           body))
        (lambda (k code thrower fmt . args)
          (log-ws client "failed to write with close code ~a: ~a"
                  code (apply format #f fmt args))
          (if (= code 1006)
              (websocket-output-close! port)
              (send-close! port code))))))))

;; Close the connection: send a close frame (1001) unless it has been sent or
;; the peer is gone, then release the connection.
;; NOTE: It may be called out of the task, so it never aborts to the prompt.
(define (ws-close server client peer-shutdown?)
  (DEBUG "ws-close ~a~%" (client-ip client))
  (let ((port (client-sockport client)))
    ;; NOTE: No close frame when the server is quitting: there's no task to
    ;;       wait for a full socket, and we don't wait for anything.
    (unless (or peer-shutdown? (preparing-quit?) (port-closed? port))
      (send-close! port 1001))
    ;; Nothing may be written after this point.
    (when (not (port-closed? port))
      (websocket-output-close! port))
    (remove-named-pipe-if-the-connection-is-websocket! client)
    (catch #t
      (lambda () (run-hook *after-websocket-close-hook*))
      (lambda (k . e)
        (log-ws client "after-websocket-close hook failed: ~a ~a" k e)))
    (%%raw-close-connection server client peer-shutdown?)))

(define (new-websocket-protocol)
  (make-ragnarok-protocol 'websocket ws-open ws-read ws-write ws-close))
