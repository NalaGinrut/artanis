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
;; protocol in the proto table, then calls the open and read methods of it
;; (ws-open and ws-serve) within the task of the connection:
;;  - ws-open calls the route handler once with the rc of the handshake, it
;;    returns the dispatcher of the connection, see
;;    (artanis websocket connection).
;;  - ws-serve never returns: it loops over reading one message, passing it
;;    to the dispatcher, and writing the outbound queue. The connection
;;    only ends by end-connection! or fail-connection!, which close it and
;;    leave the task. So the rest of the HTTP continuation (handle-request,
;;    ragnarok-write, keep-alive) is never reached for a WebSocket
;;    connection.
;; When the task waits for the peer (the read would block), the suspendable
;; port calls ws-read-waiter, which breaks the task to the Ragnarok
;; scheduler, so it never blocks the server. The task is resumed by data
;; from the peer, by ws-send/ws-close! from anywhere (wake-up-task!), or by
;; a timed watch; then the waiter writes the outbound queue, and checks the
;; idle timeout and the session, before the read is retried.
;;
;; The task of the connection is the only writer of its socket: the replies
;; to control frames are written within the read, and the outbound queue is
;; written between two messages or in the waiter, never in the middle of a
;; read or a write. If a write suspends (the socket is full), the task waits
;; in that write, so nothing else can be written meanwhile.
;;
;; After the 101 response, any error on the connection is a close frame and
;; a TCP close, never an HTTP response:
;;  - a protocol error of the peer: the close code of the error;
;;  - an error of the route handler or the dispatcher: an artanis-err is
;;    mapped from its HTTP status (see status->close-code), anything else is
;;    1011;
;;  - the session is gone: 1008;
;;  - the idle timeout, a resource recycling, the server quitting: 1001.
;; If the connection is closed in the middle of an outgoing frame, nothing
;; more is written, not even the close frame, the TCP connection is dropped.

(define-module (artanis server websocket)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis websocket)
  #:use-module (artanis websocket frame)
  #:use-module (artanis websocket connection)
  #:use-module ((artanis websocket handshake) #:select (websocket-rule-timeout))
  #:use-module (artanis websocket named-pipe)
  #:use-module ((artanis route) #:select (rc-handler rc-req))
  #:use-module (artanis server server-context)
  #:use-module (artanis server scheduler)
  #:use-module (artanis server http)
  #:use-module ((artanis config) #:select (get-conf))
  #:use-module ((artanis session) #:select (session-restore))
  #:use-module ((ice-9 suspendable-ports) #:select (current-read-waiter))
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module ((rnrs) #:select (bytevector-length
                                 bytevector-u16-ref
                                 bytevector-copy!
                                 make-bytevector
                                 utf8->string))
  #:export (new-websocket-protocol
            status->close-code))

(define (log-ws client fmt . args)
  (format (artanis-current-output) "[WebSocket] Client `~a': ~a~%"
          (client-ip client) (apply format #f fmt args)))

;; Map an HTTP status of an error to a close code.
;; #f means no close frame is sent for it: 408 is a timeout, ws-close sends
;; 1001 for it.
(define (status->close-code status)
  (cond
   ((= status 408) #f)
   ((>= status 500) 1011)
   ((memv status '(401 403)) 1008)
   ((= status 413) 1009)
   (else 1008)))

;; Send a close frame as far as it's possible. It never throws.
(define* (send-close! port code #:optional (reason #f))
  (cond
   ((websocket-output-closed? port) #t)
   ((websocket-frame-in-flight? port)
    ;; The peer has half a frame, a close frame would be parsed as its data.
    (DEBUG "A frame is in flight on ~a, drop the close frame~%" port)
    (websocket-output-close! port))
   (else
    (catch #t
      (lambda () (send-websocket-close port #:code code #:reason reason))
      (lambda e
        (DEBUG "Failed to send close frame ~a: ~a~%" code e)
        (websocket-output-close! port))))))

;; The conn of the client, or #f. It never throws, even if the port has been
;; closed.
(define (client-conn client)
  (and (client-live-fd client)
       (let ((conn (proto-conn-state client)))
         (and (websocket-conn? conn) conn))))

;; End the task of the connection: close it and leave the task.
;; NOTE: Only within the task, since simply-quit aborts to the task prompt.
(define (end-connection! server client peer-shutdown?)
  (ws-close server client peer-shutdown?)
  (simply-quit))

;; Fail the connection with a close code, see end-connection!.
(define* (fail-connection! server client code #:optional (reason #f))
  (let ((port (client-sockport client))
        (conn (client-conn client)))
    (when conn
      (websocket-conn-note-close! conn code (or reason "")))
    (cond
     ((= code 1006)
      ;; The peer is gone, nothing can be written anymore.
      (websocket-output-close! port)
      (end-connection! server client #t))
     (else
      (send-close! port code reason)
      (end-connection! server client #f)))))

;; Is it an error of a peer that has gone? See make-io-exception-handler.
(define (peer-gone? k args)
  (and (eq? k 'system-error)
       (memv (system-error-errno (cons k args)) (list EPIPE EIO ECONNRESET))
       #t))

;; Call thunk, a WebSocket error or a gone peer fails the connection.
(define (with-connection-errors server client thunk)
  (catch #t
    thunk
    (lambda (k . args)
      (match (cons k args)
        (('websocket-err code thrower fmt . fmt-args)
         (log-ws client "failed with close code ~a: ~a"
                 code (apply format #f fmt fmt-args))
         (fail-connection! server client code))
        ((? (lambda _ (peer-gone? k args)))
         (DEBUG "The peer ~a has gone: ~a~%" (client-ip client) args)
         (fail-connection! server client 1006))
        (else (apply throw k args))))))

;; The close code for an error of the route handler or the dispatcher.
(define (handler-error->close-code k args)
  (match (cons k args)
    (('artanis-err (? integer? status) . _)
     (or (status->close-code status) 1001))
    (('websocket-err (? integer? code) . _) code)
    (else 1011)))

;; Call thunk for the route handler or the dispatcher, an error of it fails
;; the connection.
(define (call-handler server client who thunk)
  (catch #t
    thunk
    (lambda (k . args)
      (case k
        ((quit interrupt) (apply throw k args))
        (else
         (let ((code (handler-error->close-code k args)))
           (log-ws client "~a failed with ~a ~s, close code ~a"
                   who k args code)
           (fail-connection! server client code)))))))

;; ---------------------------------------------------------------------------
;; Timed checks
;;
;; A connection is resumed by a timed watch (recheck-watch!), even if the
;; peer sends nothing, to do the checks below within the task. The task
;; waits in ws-read-waiter then.
;; 1. The idle timeout (#:timeout of the route, or websocket.timeout): the
;;    connection is closed with 1001 when nothing comes from the peer for
;;    that long. It's counted from the last data of the peer, so what the
;;    server sends doesn't keep a connection alive. The Ragnarok task
;;    timeout isn't used for it, since any resume of the task refreshes the
;;    task time, including a wake-up to send a message.
;;    NOTE: Time has 1-second resolution and is checked every period, so a
;;          connection is closed within [T, 2T] seconds.
;; 2. The session: a connection authenticated with a session at its
;;    handshake is closed with 1008 once the session isn't valid anymore
;;    (expired, logged out, ...). It's checked every session lifetime
;;    (cookie.expires, the expiration of new sessions), so it's closed at
;;    most one period after the session is gone. ws-serve also checks it
;;    before each message, for a peer that never lets the task wait.
;; The check is done within the task, since the session backend may do I/O
;; which suspends the task.

(define (recheck-period)
  (get-conf '(cookie expires)))

(define (idle-timeout conn)
  (websocket-rule-timeout (websocket-conn-rule conn)))

;; The period of the timed watch: the shortest of the enabled checks, or 0 if
;; none is enabled.
(define (watch-period conn)
  (let ((periods (filter positive?
                         (list (idle-timeout conn)
                               (if (websocket-conn-sid conn)
                                   (recheck-period)
                                   0)))))
    (if (null? periods) 0 (apply min periods))))

(define (touch-inbound! conn)
  (websocket-conn-last-inbound-set! conn (current-time)))

;; Throws 'websocket-err 1001 if the connection is idle.
(define (check-idle! conn)
  (let ((timeout (idle-timeout conn)))
    (when (and (> timeout 0)
               (>= (- (current-time) (websocket-conn-last-inbound conn))
                   timeout))
      (throw 'websocket-err 1001 'check-idle!
             "Idle for ~a seconds" timeout))))

;; Throws 'websocket-err 1008 if the session is gone.
(define (ws-recheck! conn)
  (let ((sid (websocket-conn-sid conn))
        (period (recheck-period)))
    (when (and sid
               (> period 0)
               (>= (- (current-time) (websocket-conn-checked-at conn))
                   period))
      ;; Set first: if the backend suspends the task, this waiter is called
      ;; again, then it just breaks the task.
      (websocket-conn-checked-at-set! conn (current-time))
      (let ((session (session-restore sid)))
        ;; Same outcomes as the check of #:session.
        (case session
          ((expired not-found)
           (throw 'websocket-err 1008 'ws-recheck!
                  "The session is ~a" session))
          (else #t))))))

;; ---------------------------------------------------------------------------
;; Outbound queue

;; Write the outbound queue of the connection. If a close is written, the
;; connection is ended.
;; NOTE: Only within the task of the connection, which is the only writer.
(define (flush-outbound! server client conn port)
  (let lp ()
    (match (websocket-conn-take-outbound! conn)
      (#f #t)
      (('data type bv)
       ;; All the fragments of a message are written in a row, nothing is
       ;; read in between. It's dropped if the output has been closed.
       (write-websocket-message port type bv)
       (lp))
      (('close code reason)
       (websocket-conn-note-close! conn code (or reason ""))
       (send-close! port code reason)
       (end-connection! server client #f)))))

;; ---------------------------------------------------------------------------
;; The protocol

;; The read waiter of a WebSocket connection. Guile's suspendable ports call
;; the current read waiter when a read would block, ignore what it returns,
;; and retry the read after it returns (see read-bytes in
;; (ice-9 suspendable-ports)). So, like async-read-waiter:
;;  1. break-task aborts to the Ragnarok scheduler, which saves the task.
;;  2. When Ragnarok resumes the task (data from the peer, a wake-up, or the
;;     timed watch), break-task returns here.
;; Then the outbound queue is written, and the timed checks are done before
;; the read is retried. If one fails, it throws out of the read to
;; with-connection-errors in ws-serve.
;; NOTE: A write of the outbound queue may suspend the task again, in the
;;       write waiter. The read isn't retried until the write is done.
;; NOTE: The session backend may read another port (e.g. a DB) within
;;       ws-recheck!, then this waiter is called for that port: it only
;;       breaks the task, like async-read-waiter.
(define (ws-read-waiter server client conn ws-port)
  (lambda (port)
    (break-task)
    (when (eq? port ws-port)
      ;; Any data from the peer, even half a frame or a ping, isn't idle.
      ;; char-ready? doesn't read, and it's #t at EOF, which the read gets.
      (when (char-ready? port)
        (touch-inbound! conn))
      (flush-outbound! server client conn port)
      (check-idle! conn)
      (ws-recheck! conn))))

(define (ws-open server client)
  (let* ((conn (proto-conn-state client))
         (rc (websocket-conn-rc conn))
         (period (watch-period conn)))
    (DEBUG "ws-open ~a, idle timeout ~a~%" (client-ip client) (idle-timeout conn))
    (websocket-conn-client-set! conn client)
    ;; No task timeout, the idle timeout is checked by the timed watch.
    (task-timeout-set! (current-task) 0)
    (when (> period 0)
      (recheck-watch! client period))
    (call-handler
     server client "after-websocket-handshake hook"
     (lambda ()
       (run-hook *after-websocket-handshake-hook* (rc-req rc) client)))
    ;; The route handler is called once for this connection.
    (let* ((handler (rc-handler rc))
           (result (call-handler
                    server client "The route handler"
                    (lambda ()
                      (if (thunk? handler) (handler) (handler rc))))))
      (cond
       ((->ws-dispatcher result)
        => (lambda (dispatcher)
             (websocket-conn-dispatcher-set! conn dispatcher)
             (and=> (ws-dispatcher-on-open dispatcher)
                    (lambda (on-open)
                      (call-handler server client "on-open"
                                    (lambda () (on-open conn)))))))
       (else
        (log-ws client "the route handler returned `~a', not a dispatcher"
                result)
        (fail-connection! server client 1011))))))

;; Serve the connection until it ends, it never returns: read, dispatch, and
;; write the outbound queue. It's the read method of the protocol, since
;; http-read enters it right after the 101 response.
(define (ws-serve server client)
  (let* ((port (client-sockport client))
         (conn (proto-conn-state client))
         (on-message (ws-dispatcher-on-message
                      (websocket-conn-dispatcher conn)))
         (waiter (ws-read-waiter server client conn port)))
    (let lp ()
      (let ((msg (with-connection-errors
                  server client
                  (lambda ()
                    ;; What the handler or other tasks have sent.
                    (flush-outbound! server client conn port)
                    (ws-recheck! conn)
                    (parameterize ((current-read-waiter waiter))
                      (read-websocket-message port))))))
        (touch-inbound! conn)
        (cond
         ((= #x8 (websocket-frame-opcode msg))
          ;; The close frame of the peer, it has been replied by the control
          ;; handler of read-websocket-message.
          (DEBUG "Received close frame from ~a~%" (client-ip client))
          (call-with-values (lambda () (close-payload->code+reason
                                        (websocket-frame-payload msg)))
            (lambda (code reason)
              (websocket-conn-note-close! conn code reason)))
          (end-connection! server client #f))
         (else
          (call-handler
           server client "on-message"
           (lambda ()
             (on-message conn (make-ws-message
                               (websocket-frame-type msg)
                               (websocket-frame-payload msg)))))
          ;; Handling a message isn't idle, e.g. a long runner.
          (touch-inbound! conn)
          (lp)))))))

;; The payload of a close frame, which has been checked by the frame layer.
;; No code means 1005 (RFC 6455 7.1.5).
(define (close-payload->code+reason payload)
  (let ((len (bytevector-length payload)))
    (if (< len 2)
        (values 1005 "")
        (let ((reason (make-bytevector (- len 2))))
          (bytevector-copy! payload 2 reason 0 (- len 2))
          (values (bytevector-u16-ref payload 0 'big)
                  (utf8->string reason))))))

;; Fail the connection for an error that escaped from the task, e.g. a bug
;; of this module. It's the write method of the protocol: the error
;; branches of the Ragnarok main-loop write the rendered error response with
;; it. The status is sent as a close frame, the body is dropped. Messages
;; are never written by it, see ws-serve.
;; NOTE: It's called out of the task prompt, so it never aborts to the
;;       prompt. The TCP connection is closed by ws-close right after it.
(define (ws-fail-and-close server client response body method-is-head?)
  (let* ((port (client-sockport client))
         (status (response-code response))
         (code (if (< status 400) 1011 (status->close-code status))))
    (log-ws client "error status ~a, close code ~a" status code)
    (when code
      (and=> (client-conn client)
             (lambda (conn) (websocket-conn-note-close! conn code)))
      (send-close! port code))))

;; Close the connection: send a close frame (1001) unless it has been sent or
;; the peer is gone, call on-close of the dispatcher, then release the
;; connection.
;; NOTE: It may be called out of the task (the peer shut down, the server is
;;       quitting, an error escaped from the task, ...), so it never aborts
;;       to the prompt.
(define (ws-close server client peer-shutdown?)
  (DEBUG "ws-close ~a~%" (client-ip client))
  (let ((port (client-sockport client))
        (conn (client-conn client)))
    ;; NOTE: No close frame when the server is quitting: there's no task to
    ;;       wait for a full socket, and we don't wait for anything.
    (unless (or peer-shutdown? (preparing-quit?) (port-closed? port))
      (when conn (websocket-conn-note-close! conn 1001))
      (send-close! port 1001))
    ;; Nothing may be written after this point.
    (when (not (port-closed? port))
      (websocket-output-close! port))
    (when conn
      (notify-close! client conn (if peer-shutdown? 1006 1001)))
    (remove-named-pipe-if-the-connection-is-websocket! client)
    (catch #t
      (lambda () (run-hook *after-websocket-close-hook*))
      (lambda (k . e)
        (log-ws client "after-websocket-close hook failed: ~a ~a" k e)))
    (%%raw-close-connection server client peer-shutdown?)))

;; Mark the conn closed and call on-close, only once.
(define (notify-close! client conn default-code)
  (let ((why (websocket-conn-close! conn default-code))
        (dispatcher (websocket-conn-dispatcher conn)))
    (when (and why dispatcher (ws-dispatcher-on-close dispatcher))
      (catch #t
        (lambda ()
          ((ws-dispatcher-on-close dispatcher) conn (car why) (cdr why)))
        (lambda (k . e)
          (log-ws client "on-close failed: ~a ~s" k e))))))

(define (new-websocket-protocol)
  (make-ragnarok-protocol 'websocket
                          ws-open ws-serve ws-fail-and-close ws-close))
