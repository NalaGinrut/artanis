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

;; Topics of WebSocket connections, and the release of a connection.
;;
;; A connection subscribes to topics, then anyone (an HTTP handler, a runner
;; thread, another connection) publishes to a topic, and every subscriber
;; gets the message with ws-send. A topic is a string chosen by the server,
;; e.g. "user:42" or "room:7".
;;
;;   (get "/notify" #:websocket 'raw #:with-auth 'status
;;     (lambda (rc)
;;       (let ((uid (user-id-of rc)))
;;         (ws-dispatcher
;;          #:on-open (lambda (conn)
;;                      (ws-subscribe! conn (string-append "user:" uid)))
;;          #:on-message (lambda (conn msg) #t)))))
;;
;;   ;; Elsewhere, e.g. in an HTTP handler after an order is paid:
;;   (ws-publish (string-append "user:" uid) "paid")
;;
;; Rules:
;; 1. Only a connection authenticated at its handshake (its route has
;;    #:with-auth) can subscribe, since the topic is how a message reaches a
;;    user. The topic is chosen by the server, never taken from the client
;;    as it is.
;; 2. A connection subscribes to websocket.maxtopics topics at most.
;; 3. #:exclusive? #t makes the connection the only subscriber of the topic:
;;    the other subscribers are closed with 4001 "Replaced", e.g. a user can
;;    be online in one place only. Don't use it if several connections of a
;;    user should get the messages, e.g. several tabs.
;; 4. A connection is unsubscribed from all its topics when it's released.
;; 5. ws-publish calls ws-send for each subscriber, so a message is encoded
;;    by the application protocol of each connection (once per protocol),
;;    and the outbound queue limit applies to each connection.
;; NOTE: The topic table is protected by a mutex, since ws-publish may be
;;       called from other threads.
;;
;; The release of a connection is here too, since it's the bottom module that
;; knows all the resources of a connection, and Ragnarok can import it:
;; websocket-release! is called by ws-close, and by the error paths of
;; Ragnarok that drop a task without ws-close. It's done once, whoever calls
;; it first: the connection is closed (nothing can be sent anymore),
;; unsubscribed from its topics, on-close of its dispatcher is called, then
;; *after-websocket-close-hook* is run.

(define-module (artanis websocket topic)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis config)
  #:use-module ((artanis logger) #:select (artanis-log))
  #:use-module (artanis websocket connection)
  #:use-module (artanis server server-context)
  #:use-module (ice-9 threads)
  #:export (ws-subscribe!
            ws-unsubscribe!
            ws-publish
            ws-subscribers
            ws-topics-of

            websocket-conn-release!
            websocket-release!))

;; topic -> list of conns
(define *topics* (make-hash-table))
;; conn -> list of topics
(define *conn-topics* (make-hash-table))
(define *topic-mutex* (make-mutex))

(define (max-topics) (get-conf '(websocket maxtopics)))

(define (check-topic topic who)
  (unless (and (string? topic) (not (string-null? topic)))
    (throw 'artanis-err 500 who "Invalid topic `~a', expect a string" topic)))

(define (check-conn conn who)
  (unless (websocket-conn? conn)
    (throw 'artanis-err 500 who "Not a WebSocket connection `~a'" conn)))

;; NOTE: With the mutex held.
(define (remove-subscriber! topic conn)
  (let ((conns (delq conn (hash-ref *topics* topic '()))))
    (if (null? conns)
        (hash-remove! *topics* topic)
        (hash-set! *topics* topic conns))))

;; NOTE: With the mutex held.
(define (remove-topic-of-conn! conn topic)
  (let ((topics (delete topic (hashq-ref *conn-topics* conn '()))))
    (if (null? topics)
        (hashq-remove! *conn-topics* conn)
        (hashq-set! *conn-topics* conn topics))))

;; Subscribe conn to topic. Returns #t, or 'closed if the connection is
;; closing or closed.
(define* (ws-subscribe! conn topic #:key (exclusive? #f))
  (check-conn conn 'ws-subscribe!)
  (check-topic topic 'ws-subscribe!)
  (unless (websocket-conn-authenticated? conn)
    (throw 'artanis-err 500 'ws-subscribe!
           "Only a connection authenticated at its handshake can subscribe, add #:with-auth to its route"))
  (let ((replaced
         (with-mutex *topic-mutex*
           (cond
            ((websocket-conn-closing? conn) 'closed)
            (else
             (let ((topics (hashq-ref *conn-topics* conn '())))
               (unless (or (member topic topics)
                           (let ((limit (max-topics)))
                             (or (<= limit 0) (< (length topics) limit))))
                 (throw 'artanis-err 500 'ws-subscribe!
                        "A connection can subscribe to ~a topics at most, see websocket.maxtopics"
                        (max-topics)))
               (let* ((others (delq conn (hash-ref *topics* topic '())))
                      (replaced (if exclusive? others '())))
                 (for-each (lambda (c)
                             (remove-subscriber! topic c)
                             (remove-topic-of-conn! c topic))
                           replaced)
                 (unless (memq conn (hash-ref *topics* topic '()))
                   (hash-set! *topics* topic
                              (cons conn (hash-ref *topics* topic '()))))
                 (unless (member topic topics)
                   (hashq-set! *conn-topics* conn (cons topic topics)))
                 replaced)))))))
    (cond
     ((eq? replaced 'closed) 'closed)
     (else
      ;; Close them out of the mutex, ws-close! only queues the close frame,
      ;; each connection closes itself within its own task.
      (for-each (lambda (c) (ws-close! c #:code 4001 #:reason "Replaced"))
                replaced)
      #t))))

(define (ws-unsubscribe! conn topic)
  (check-conn conn 'ws-unsubscribe!)
  (check-topic topic 'ws-unsubscribe!)
  (with-mutex *topic-mutex*
    (remove-subscriber! topic conn)
    (remove-topic-of-conn! conn topic)))

(define (ws-subscribers topic)
  (check-topic topic 'ws-subscribers)
  (with-mutex *topic-mutex*
    (list-copy (hash-ref *topics* topic '()))))

(define (ws-topics-of conn)
  (check-conn conn 'ws-topics-of)
  (with-mutex *topic-mutex*
    (list-copy (hashq-ref *conn-topics* conn '()))))

;; Send obj to the subscribers of topic. On a '(proto X) route obj is
;; encoded by X, once for all the subscribers of X; otherwise obj is sent as
;; on a 'raw route. Returns the number of subscribers it's queued for. A
;; subscriber that has been closed is unsubscribed.
;; NOTE: An encode error is thrown to the caller, like ws-send.
(define (ws-publish topic obj)
  (check-topic topic 'ws-publish)
  (let ((conns (ws-subscribers topic))
        (encoded (make-hash-table)))
    (define (data-for conn)
      (let ((p (websocket-conn-protocol conn)))
        (cond
         ((not p) obj)
         ((hashq-ref encoded p))
         (else
          (let ((m (ws-pre-encode conn obj)))
            (hashq-set! encoded p m)
            m)))))
    (let lp ((conns conns) (n 0))
      (cond
       ((null? conns) n)
       (else
        (let* ((conn (car conns))
               (r (ws-send conn (data-for conn))))
          (when (eq? r 'closed)
            ;; It should have been released, unsubscribe it anyway.
            (with-mutex *topic-mutex*
              (remove-subscriber! topic conn)
              (remove-topic-of-conn! conn topic)))
          (lp (cdr conns) (if (eq? r #t) (1+ n) n))))))))

;; ---------------------------------------------------------------------------
;; Release

(define (log-release conn fmt . args)
  (artanis-log 'websocket #f #f
               #:msg (apply format #f fmt args)
               #:meta (let ((client (websocket-conn-client conn)))
                        (if client `((client . ,(client-ip client))) '()))))

;; Release conn, only the first call does it. default-code is the close code
;; for on-close if none has been recorded, e.g. 1006 when the connection is
;; dropped. It never throws.
(define (websocket-conn-release! conn default-code)
  (let ((why (websocket-conn-close! conn default-code)))
    (when why
      (with-mutex *topic-mutex*
        (for-each (lambda (topic) (remove-subscriber! topic conn))
                  (hashq-ref *conn-topics* conn '()))
        (hashq-remove! *conn-topics* conn))
      (let ((dispatcher (websocket-conn-dispatcher conn)))
        (when (and dispatcher (ws-dispatcher-on-close dispatcher))
          (catch #t
            (lambda ()
              ((ws-dispatcher-on-close dispatcher) conn (car why) (cdr why)))
            (lambda (k . e)
              (log-release conn "on-close failed: ~a ~s" k e)))))
      (catch #t
        (lambda () (run-hook *after-websocket-close-hook*))
        (lambda (k . e)
          (log-release conn "after-websocket-close hook failed: ~a ~s" k e))))))

;; Release the WebSocket connection of client, if it's one. It's for the
;; paths that drop a task without ws-close (see Ragnarok), the connection is
;; regarded as gone (1006). It doesn't close the socket, the caller does.
(define* (websocket-release! client #:optional (default-code 1006))
  (catch #t
    (lambda ()
      (let ((conn (and (ragnarok-client? client)
                       (client-live-fd client)
                       (proto-conn-state client))))
        (when (websocket-conn? conn)
          (websocket-conn-release! conn default-code))))
    (lambda _ #t)))
