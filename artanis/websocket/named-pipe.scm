;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2018-2025
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

(define-module (artanis websocket named-pipe)
  #:use-module (artanis utils)
  #:use-module (artanis irregex)
  #:use-module (artanis route)
  #:use-module (artanis websocket frame)
  #:use-module (artanis server)
  #:use-module (artanis server server-context)
  #:use-module ((ice-9 iconv) #:select (string->bytevector))
  #:use-module ((rnrs) #:select (bytevector? define-record-type))
  #:export (register-websocket-pipe!
            pair-name-to-client!
            send-to-websocket-named-pipe
            named-pipe-subscribe
            remove-named-pipe-if-the-connection-is-websocket!
            detect-pipe-name
            get-named-pipe
            new-named-pipe
            named-pipe-clients named-pipe-clients-set!
            named-pipe-task-queue-set!))

;; NOTE:
;; By default, named-pipe and client is 1:1 relation.
;; However, it could be 1:N if you enabled `inexclusive' mode.
;; We also make a table for getting name from client. It's worth
;; since every client can register a named-pipe, and there're too
;; many named-pipe to be traversed to close when the client refresh
;; the page.
(define *client-to-named-pipe* (make-hash-table))
(define *websocket-named-pipe* (make-hash-table))

(define-record-type named-pipe
  (fields
   name
   (mutable clients)
   task-queue))

(define* (new-named-pipe name clients #:optional (task-queue (new-queue)))
  (make-named-pipe name clients task-queue))

(define (client->pipe-name client)
  (hash-ref *client-to-named-pipe* client))

;; NOTE:
;; Iff clients list is empty, the named-pipe could be removed.
(define (remove-named-pipe-if-the-connection-is-websocket! client)
  (let* ((name (client->pipe-name client))
         (np (get-named-pipe name))
         (clients (named-pipe-clients np)))
    (when np
      (DEBUG "Removing websocket client `~a' from named-pipe `~a' ......" client name)
      (hashq-remove! *client-to-named-pipe* client)
      (named-pipe-clients-set! np (delete client clients))
      (when (null? (named-pipe-clients np))
        (DEBUG "Removing named-pipe `~a' since its all clients are closed ......" name)
        (hash-remove! *websocket-named-pipe* name))
      (DEBUG "Done~%"))))

(define (get-named-pipe name)
  (hash-ref *websocket-named-pipe* name))

(define (get-pipe-clients name)
  (and=> (get-named-pipe name) named-pipe-clients))

(define (get-pipe-task-queue name)
  (and=> (get-named-pipe name) named-pipe-task-queue))

(define *named-pipe-re*
  (string->sre "artanis_named_pipe=(.*)"))

(define (detect-pipe-name req)
  (let ((m (irregex-match *named-pipe-re* (uri-query (request-uri req)))))
    (and m
         (irregex-match-substring m 1))))

(::define (pair-name-to-client! client name)
  (:anno: (ragnarok-client string) -> ANY)
  (hash-set! *client-to-named-pipe* client name))

(::define (register-websocket-pipe! named-pipe)
  (:anno: (named-pipe) -> ANY)
  (let ((clients (named-pipe-clients named-pipe))
        (name (named-pipe-name named-pipe)))
    (hash-set! *websocket-named-pipe* (named-pipe-name named-pipe) named-pipe)
    (for-each
     (lambda (client)
       (pair-name-to-client! client name))
     clients)))

(define (send-to-websocket-named-pipe name data)
  (let ((clients (get-pipe-clients name))
        (frame (new-websocket-frame/client
                'text #t
                (cond
                 ((string? data) (string->bytevector data "iso-8859-1"))
                 ((bytevector? data) data)
                 (else (throw 'artanis-err 500 send-to-websocket-named-pipe
                              "Wrong type of websocket data, should be string or bv `~a'"
                              data))))))
    (cond
     (clients
      (let ((task-queue (get-pipe-task-queue name)))
        ;; NOTE: We can't just send the data to the named-pipe, since it's possible to have
        ;;       race conditions. If data-1 were blocked in transmission, and data-2 were
        ;;       coming then the operation would be undetermined when the connection is
        ;;       awakend.
        ;; NOTE: The task queue will be handled in named-pipe-event-loop to send the data
        ;;       one by one.
        (when task-queue
          (for-each
           (lambda (client)
             (oneshot-mention! client)
             (queue-in! task-queue
                        (lambda ()
                          (parameterize ((current-client client))
                            (write-websocket-frame/client (client-sockport client) frame)))))
           clients))))
     (else
      (throw 'artanis-err 400 send-to-websocket-named-pipe
             "Pipe name `~a' hasn't been registered, or it's closed by client!" name)))))

(define* (named-pipe-subscribe rc #:key (init-thunk #f))
  (let* ((name (detect-pipe-name (rc-req rc)))
         (task-queue (get-pipe-task-queue name)))
    (and init-thunk (init-thunk))
    (let lp ()
      (cond
       ((not task-queue)
        (throw 'artanis-err 400 named-pipe-subscribe
               "Named-pipe `~a' was closed, we drop this connection!" name))
       ((queue-empty? task-queue)
        (DEBUG "Named-pipe: task queue is empty, we scheduled!~%")
        (break-task)
        (lp))
       (else
        ;; NOTE: Don't pop out message here, the correct way is to pop
        ;;       after thunk calling successfully.
        ;;       Because the client maybe closed, so that we lose the
        ;;       chance to resend again.
        (let ((t (queue-head task-queue)))
          (DEBUG "Named-pipe: run a task ~a" t)
          (when (t)
            (queue-out! task-queue))
          (break-task)
          (lp)))))))
