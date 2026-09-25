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

;; The connection-oriented API of WebSocket.
;;
;; The route of a WebSocket connection is the connection: after the 101
;; response, its handler is called once with the rc of the handshake, and
;; returns the dispatcher of this connection. Per-connection state lives in
;; the closure of the handler.
;;
;;   (get "/room/:id" #:websocket 'raw
;;     (lambda (rc)
;;       (let ((room (params rc "id")))
;;         (ws-dispatcher
;;          #:on-open (lambda (conn) (join-room! room conn))
;;          #:on-message (lambda (conn msg)
;;                         (ws-send conn (string-append room ":"
;;                                                      (ws-message-text msg))))
;;          #:on-close (lambda (conn code reason) (leave-room! room conn))))))
;;
;;   ;; A procedure is the short form of #:on-message only.
;;   (get "/echo" #:websocket 'raw
;;     (lambda (rc) (lambda (conn msg) (ws-send conn msg))))
;;
;; Semantics:
;; 0. on-open is called once within the task of the connection, before any
;;    message. It's where the connection is registered for pushing, e.g. a
;;    connection that only receives notifications never sends a message.
;; 1. on-message is called within the task of the connection, one message at
;;    a time. While it runs (including a call-with-runner in it), the
;;    connection doesn't read, doesn't answer pings, and doesn't write its
;;    outbound queue. So a runner is a backpressure on the input, and what's
;;    sent meanwhile goes out when on-message returns.
;; 2. ws-send and ws-close! can be called from anywhere: the task of the
;;    connection, another task (e.g. an HTTP handler), or another thread
;;    (e.g. a runner). They never write to the socket, they put the message
;;    into the outbound queue of the connection and wake its task up. The
;;    task of the connection is the only writer of its socket, so a control
;;    frame can never be put in the middle of a data frame.
;; 3. The outbound queue is limited by websocket.maxqueue in bytes. When a
;;    message would exceed it, ws-send returns 'overflow, and by the #:overflow
;;    option of the route:
;;     - reject (default): the message is dropped, the caller decides what to
;;       do with it;
;;     - close: the peer is regarded as a slow consumer, the queued messages
;;       are dropped, and the connection is closed with 1008.
;; 4. on-close is called once when the connection is closed, whoever closes
;;    it, with the close code and reason:
;;     - the close frame of the peer: its code (1005 if it has none);
;;     - the peer went away without a close frame: 1006;
;;     - a close frame sent by the server: its code, e.g. ws-close!, an
;;       error, the session is gone (1008), the idle timeout (1001).
;;    NOTE: When the peer goes away, or the server is quitting, the close is
;;          done outside the task of the connection. So on-close must not do
;;          any I/O that may suspend (e.g. DB queries): use it to release
;;          in-memory state, e.g. remove the connection from a room.
;; 5. Strings are sent as text messages encoded in UTF-8 (server.charset is
;;    not used here). Anything else is sent from a ws-buffer, see below.
;;
;; TODO: #:websocket '(proto X) will attach a codec of X to the dispatcher,
;;       (decode msg) -> obj on the way in, (encode obj) -> (values type
;;       ws-buffer) on the way out. For now it's served as 'raw.
;; TODO: Rate limiting (e.g. an OHT #:rate-limit), and a buffer pool.

(define-module (artanis websocket connection)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis server server-context)
  #:use-module ((artanis websocket handshake) #:select (websocket-rule-overflow))
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module ((rnrs) #:select (define-record-type
                                 bytevector?
                                 bytevector-length
                                 make-bytevector
                                 bytevector-u8-set!
                                 bytevector-copy!
                                 string->utf8
                                 utf8->string))
  #:export (;; The API for the handler of WebSocket routes
            ws-dispatcher
            ws-dispatcher?
            ws-send
            ws-close!

            ws-message?
            ws-message-type
            ws-message-text?
            ws-message-binary?
            ws-message-text
            ws-message-payload

            make-ws-buffer
            ws-buffer?
            ws-buffer-length
            ws-buffer-u8-set!
            ws-buffer-put!
            ws-buffer-frozen?
            string->ws-buffer
            bytevector->ws-buffer

            ;; For the server side, see (artanis server websocket)
            new-websocket-conn
            websocket-conn?
            websocket-conn-rc
            websocket-conn-rule
            websocket-conn-sid
            websocket-conn-client
            websocket-conn-client-set!
            websocket-conn-checked-at
            websocket-conn-checked-at-set!
            websocket-conn-last-inbound
            websocket-conn-last-inbound-set!
            websocket-conn-dispatcher
            websocket-conn-dispatcher-set!
            websocket-conn-queued-bytes
            websocket-conn-take-outbound!
            websocket-conn-closing?
            websocket-conn-note-close!
            websocket-conn-close!
            ->ws-dispatcher
            ws-dispatcher-on-open
            ws-dispatcher-on-message
            ws-dispatcher-on-close
            make-ws-message))

;; ---------------------------------------------------------------------------
;; Outgoing buffers
;;
;; A ws-buffer holds the bytes of an outgoing message. Its bytevector is never
;; exposed: it's filled by the procedures below, and once it's sent it's
;; frozen and can't be modified anymore, since it may still be waiting in the
;; outbound queue, or being written. So a buffer is never reused after it's
;; sent, but it can be sent many times, e.g. to broadcast one message to many
;; connections without copying it.

(define-record-type (ws-buffer* make-ws-buffer* ws-buffer?)
  (fields (immutable bv ws-buffer-bv)
          (mutable frozen? ws-buffer-frozen? ws-buffer-frozen?-set!))
  (protocol (lambda (new) (lambda (bv) (new bv #f)))))

(define (make-ws-buffer size)
  (unless (and (exact-integer? size) (>= size 0))
    (throw 'artanis-err 500 'make-ws-buffer "Invalid buffer size `~a'" size))
  (make-ws-buffer* (make-bytevector size 0)))

(define (ws-buffer-length buf)
  (bytevector-length (ws-buffer-bv buf)))

(define (check-writable buf who)
  (unless (ws-buffer? buf)
    (throw 'artanis-err 500 who "Not a ws-buffer `~a'" buf))
  (when (ws-buffer-frozen? buf)
    (throw 'artanis-err 500 who
           "The buffer has been sent, it can't be modified anymore")))

(define (ws-buffer-u8-set! buf index value)
  (check-writable buf 'ws-buffer-u8-set!)
  (bytevector-u8-set! (ws-buffer-bv buf) index value))

;; Copy src[start, start+count) into the buffer at offset.
(define* (ws-buffer-put! buf offset src
                         #:optional
                         (start 0)
                         (count (and (bytevector? src)
                                     (- (bytevector-length src) start))))
  (check-writable buf 'ws-buffer-put!)
  (unless (bytevector? src)
    (throw 'artanis-err 500 'ws-buffer-put! "Not a bytevector `~a'" src))
  (bytevector-copy! src start (ws-buffer-bv buf) offset count))

;; The UTF-8 bytes of str, e.g. to send the same text to many connections.
(define (string->ws-buffer str)
  (make-ws-buffer* (string->utf8 str)))

;; A copy of bv[start, start+count).
(define* (bytevector->ws-buffer bv
                                #:optional
                                (start 0)
                                (count (and (bytevector? bv)
                                            (- (bytevector-length bv) start))))
  (let ((buf (make-ws-buffer count)))
    (ws-buffer-put! buf 0 bv start count)
    buf))

;; ---------------------------------------------------------------------------
;; Incoming messages
;;
;; type is 'text or 'binary. A text message has been checked as valid UTF-8
;; by the frame layer. payload is the bytevector of the whole message, it
;; belongs to the handler.

(define-record-type ws-message
  (fields type payload))

(define (ws-message-text? msg) (eq? 'text (ws-message-type msg)))

(define (ws-message-binary? msg) (eq? 'binary (ws-message-type msg)))

(define (ws-message-text msg)
  (unless (ws-message-text? msg)
    (throw 'artanis-err 500 'ws-message-text "Not a text message"))
  (utf8->string (ws-message-payload msg)))

;; ---------------------------------------------------------------------------
;; Dispatchers

(define-record-type (ws-dispatcher* make-dispatcher ws-dispatcher?)
  (fields (immutable on-open ws-dispatcher-on-open)
          (immutable on-message ws-dispatcher-on-message)
          (immutable on-close ws-dispatcher-on-close))
  (protocol
   (lambda (new)
     (lambda (on-open on-message on-close)
       (unless (or (not on-open) (procedure? on-open))
         (throw 'artanis-err 500 'ws-dispatcher
                "#:on-open must be a procedure, but it's `~a'" on-open))
       (unless (procedure? on-message)
         (throw 'artanis-err 500 'ws-dispatcher
                "#:on-message must be a procedure, but it's `~a'" on-message))
       (unless (or (not on-close) (procedure? on-close))
         (throw 'artanis-err 500 'ws-dispatcher
                "#:on-close must be a procedure, but it's `~a'" on-close))
       (new on-open on-message on-close)))))

;; on-open:    (conn) -> ANY, or #f
;; on-message: (conn msg) -> ANY
;; on-close:   (conn code reason) -> ANY, or #f
(define* (ws-dispatcher #:key (on-open #f) on-message (on-close #f))
  (make-dispatcher on-open on-message on-close))

;; The return value of a route handler as a dispatcher, or #f if it's not a
;; valid one.
(define (->ws-dispatcher x)
  (cond
   ((ws-dispatcher? x) x)
   ((procedure? x) (make-dispatcher #f x #f))
   (else #f)))

;; ---------------------------------------------------------------------------
;; Connections
;;
;; rc: the route context of the handshake, the route handler is called with
;;     it once.
;; rule: the websocket-rule of the route.
;; sid: the session the connection was authenticated with at its handshake,
;;      or #f. It's checked again periodically, see ws-recheck! in
;;      (artanis server websocket).
;; checked-at: when the session was checked last time, in seconds.
;; client: the ragnarok-client, set when the connection is opened.
;; last-inbound: when data came from the peer last time, in seconds. The idle
;;               timeout is counted from it, so what the server sends doesn't
;;               keep a connection alive.
;; dispatcher: the ws-dispatcher returned by the route handler.
;; outbound: the queue of outgoing items, each is one of
;;   (data type bytevector)
;;   (close code reason)
;; queued: the payload bytes in outbound.
;; state: 'open, 'closing (a close is queued, nothing can be sent anymore),
;;        or 'closed.
;; close-code, close-reason: why the connection is closed, for on-close. The
;;                           first one recorded wins.
;; NOTE: mutex protects outbound, queued, state, close-code and close-reason,
;;       since ws-send may be called from other threads. The rest is only
;;       touched by the server thread.

(define-record-type websocket-conn
  (fields rc rule sid
          (mutable checked-at)
          (mutable client)
          (mutable last-inbound)
          (mutable dispatcher)
          mutex
          outbound
          (mutable queued)
          (mutable state)
          (mutable close-code)
          (mutable close-reason)))

(define (new-websocket-conn rc rule sid)
  (make-websocket-conn rc rule sid (current-time) #f (current-time) #f
                       (make-mutex) (new-queue) 0 'open #f #f))

(define (websocket-conn-queued-bytes conn)
  (with-mutex (websocket-conn-mutex conn)
    (websocket-conn-queued conn)))

(define (websocket-conn-closing? conn)
  (with-mutex (websocket-conn-mutex conn)
    (not (eq? 'open (websocket-conn-state conn)))))

;; Record why the connection is closed, unless it has been recorded.
(define* (websocket-conn-note-close! conn code #:optional (reason ""))
  (with-mutex (websocket-conn-mutex conn)
    (note-close! conn code reason)))

;; NOTE: With the mutex held.
(define (note-close! conn code reason)
  (unless (websocket-conn-close-code conn)
    (websocket-conn-close-code-set! conn code)
    (websocket-conn-close-reason-set! conn reason)))

;; The connection is closed: nothing can be sent anymore, and the queued
;; items are dropped. Returns (code . reason) for on-close, or #f if it has
;; been closed before, since on-close is called only once.
;; default-code is used if no close code has been recorded.
(define (websocket-conn-close! conn default-code)
  (with-mutex (websocket-conn-mutex conn)
    (cond
     ((eq? 'closed (websocket-conn-state conn)) #f)
     (else
      (websocket-conn-state-set! conn 'closed)
      (drop-queue! conn)
      (note-close! conn default-code "")
      (cons (websocket-conn-close-code conn)
            (websocket-conn-close-reason conn))))))

;; NOTE: With the mutex held.
(define (drop-queue! conn)
  (let ((q (websocket-conn-outbound conn)))
    (let lp ()
      (unless (queue-empty? q)
        (queue-out! q)
        (lp))))
  (websocket-conn-queued-set! conn 0))

;; Take the next outgoing item, or #f if there's none.
;; NOTE: Only for the task of the connection, see flush-outbound! in
;;       (artanis server websocket).
(define (websocket-conn-take-outbound! conn)
  (with-mutex (websocket-conn-mutex conn)
    (let ((q (websocket-conn-outbound conn)))
      (cond
       ((queue-empty? q) #f)
       (else
        (let ((item (queue-out! q)))
          (match item
            (('data _ bv)
             (websocket-conn-queued-set!
              conn (- (websocket-conn-queued conn) (bytevector-length bv))))
            (else #t))
          item))))))

;; Wake the task of the connection up to write the queue. Not needed within
;; the task itself: it writes the queue when on-message returns.
(define (wake-up-conn! conn)
  (let ((client (websocket-conn-client conn)))
    (when (and client (not (eq? client (current-client))))
      (wake-up-task! client))))

(define (max-queue) (get-conf '(websocket maxqueue)))

(define (outgoing data type)
  (define (check-type who type)
    (unless (memq type '(text binary))
      (throw 'artanis-err 500 who "Invalid message type `~a'" type))
    type)
  (cond
   ((string? data)
    (values (check-type 'ws-send (or type 'text)) (string->utf8 data)))
   ((ws-buffer? data)
    (ws-buffer-frozen?-set! data #t)
    (values (check-type 'ws-send (or type 'binary)) (ws-buffer-bv data)))
   ((ws-message? data)
    ;; Forward a received message as it is, e.g. echo.
    (values (check-type 'ws-send (or type (ws-message-type data)))
            (ws-message-payload data)))
   (else
    (throw 'artanis-err 500 'ws-send
           "Can't send `~a', expect a string, a ws-buffer or a ws-message"
           data))))

;; Send a message to the connection. data is a string (a text message by
;; default), a ws-buffer (a binary message by default) or a ws-message.
;; #:type 'text or 'binary overrides the type. A ws-buffer sent as text must
;; hold valid UTF-8.
;; Returns:
;;  #t         the message is queued;
;;  'overflow  the queue is full, see #:overflow of the route;
;;  'closed    the connection is closing or closed, the message is dropped.
(define* (ws-send conn data #:key (type #f))
  (unless (websocket-conn? conn)
    (throw 'artanis-err 500 'ws-send "Not a WebSocket connection `~a'" conn))
  (call-with-values (lambda () (outgoing data type))
    (lambda (type bv)
      (let* ((len (bytevector-length bv))
             (limit (max-queue))
             (result
              (with-mutex (websocket-conn-mutex conn)
                (cond
                 ((not (eq? 'open (websocket-conn-state conn))) 'closed)
                 ((and (> limit 0)
                       (> (+ (websocket-conn-queued conn) len) limit))
                  (when (eq? 'close (rule-overflow (websocket-conn-rule conn)))
                    ;; A slow consumer: drop what it hasn't taken, and close
                    ;; it right after the frame being written, if any.
                    (drop-queue! conn)
                    (queue-in! (websocket-conn-outbound conn)
                               (list 'close 1008 "Outbound queue overflow"))
                    (websocket-conn-state-set! conn 'closing))
                  'overflow)
                 (else
                  (queue-in! (websocket-conn-outbound conn) (list 'data type bv))
                  (websocket-conn-queued-set!
                   conn (+ (websocket-conn-queued conn) len))
                  #t)))))
        (unless (eq? result 'closed)
          (wake-up-conn! conn))
        result))))

;; The #:overflow of the route. The rule is #f in unit tests.
(define (rule-overflow rule)
  (if rule (websocket-rule-overflow rule) 'reject))

(define (sendable-close-code? code)
  (and (exact-integer? code)
       (or (<= 1000 code 1003)
           (<= 1007 code 1011)
           (<= 3000 code 4999))))

;; Close the connection with a close frame. It's queued after the messages
;; that have been sent, and nothing can be sent after it.
;; reason is a string of at most 123 bytes in UTF-8.
;; Returns #t, or 'closed if the connection is closing or closed.
(define* (ws-close! conn #:key (code 1000) (reason #f))
  (unless (websocket-conn? conn)
    (throw 'artanis-err 500 'ws-close! "Not a WebSocket connection `~a'" conn))
  (unless (sendable-close-code? code)
    (throw 'artanis-err 500 'ws-close! "Close code ~a can't be sent" code))
  (when (and reason
             (not (and (string? reason)
                       (<= (bytevector-length (string->utf8 reason)) 123))))
    (throw 'artanis-err 500 'ws-close!
           "The close reason must be a string of at most 123 bytes"))
  (let ((result
         (with-mutex (websocket-conn-mutex conn)
           (cond
            ((not (eq? 'open (websocket-conn-state conn))) 'closed)
            (else
             (queue-in! (websocket-conn-outbound conn) (list 'close code reason))
             (websocket-conn-state-set! conn 'closing)
             #t)))))
    (when (eq? result #t)
      (wake-up-conn! conn))
    result))
