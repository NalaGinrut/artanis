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

(define-module (artanis server server-context)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (make-ragnarok-engine
            ragnarok-engine?
            ragnarok-engine-name
            ragnarok-engine-breaker
            ragnarok-engine-runner
            ragnarok-engine-loader

            make-ragnarok-server
            ragnarok-server?
            ragnarok-server-epfd
            ragnarok-server-listen-socket
            ragnarok-server-work-table
            ragnarok-server-ready-queue
            ragnarok-server-event-set

            make-work-table
            work-table?
            work-table-content
            work-table-mutex

            make-protocol
            protocol?
            protocol-name
            protocol-read
            protocol-write
            protocol-close
            define-protocol

            make-task
            task?
            task-conn
            task-kont
            task-prio

            make-ragnarok-client
            ragnarok-client?
            client-sockport
            client-sockport-decriptor
            client-connection

            remove-from-work-table!
            add-current-task-to-work-table!
            get-task-from-work-table))

(define-record-type ragnarok-engine
  (fields
   name
   breaker
   runner
   loader))

(define-record-type ragnarok-server
  (fields
   epfd
   listen-socket
   work-table ; a table contains continuations
   ready-queue ; a queue contains connect socket
   event-set))

;; A table contains continuations
;; * content: the continuation
;; * mutex: a mutex for lock
(define-record-type work-table
  (fields content mutex))

(define-record-type protocol
  (fields name open read write close))

(define-syntax-rule (define-protocol name open read write close)
  (define name
    (make-protocol 'name open read write close)))

(define-record-type task
  (fields
   conn   ; connecting socket port
   kont   ; delimited continuation
   prio)) ; priority

(define-box-type ragnarok-client)
(define (make-ragnarok-client v)
  (make-box-type ragnarok-client v))

;; for emacs:
;; (put '::define 'scheme-indent-function 1)

;; ragnarok-client -> socket-port
(::define (client-sockport c)
  (:anno: (ragnarok-client) -> socket-port)
  (car (unbox-type c)))

;; ragnarok-client -> integer
(::define (client-sockport-decriptor c)
  (:anno: (ragnarok-client) -> int)
  (port->fdes (client-port (unbox-type c))))

;; ragnarok-client -> vector 
(::define (client-details c)
  (:anno: (ragnarok-client) -> vector)
  (cdr (unbox-type c)))

;; ragnarok-client -> integer
(::define (client-fam c)
  (:anno: (ragnarok-client) -> int)
  (sockaddr:fam (client-details c)))

;; ragnarok-client -> integer
(::define (client-addr c)
  (:anno: (ragnarok-client) -> int)
  (sockaddr:addr (client-details c)))

;; ragnarok-client -> integer
;; NOTE: Different from listenning-port
(::define (client-connecting-port c)
  (:anno: (ragnarok-client) -> int)
  (sockaddr:port (client-details c)))

;; work-table -> ragnarok-client -> ANY
(::define (remove-from-work-table! wt client)
  (:anno: (work-table ragnarok-client) -> ANY)
  (assume-type wt hash-table?)
  (assume-type client ragnarok-client?)
  (hashv-remove! (work-table-content wt) (client-sockport-decriptor client )))

;; work-table -> integer -> ANY
(::define (add-current-task-to-work-table! wt client)
  (:anno: (work-table int) -> ANY)
  (assume-type wt hash-table?)
  (assume-type client ragnarok-client?)
  (hashv-set! wt conn (current-task)))

;; work-table -> sockport -> task -> ANY
(::define (add-a-task-to-work-table! wt client task)
  (:anno: (work-table sockport task) -> ANY)
  (hashv-set! wt (client-sockport-decriptor client) task))

;; work-table -> ragnarok-client -> task
(::define (get-task-from-work-table wt client)
  (:anno: (work-table ragnarok-client) -> task)
  (hashv-ref wt (client-sockport-decriptor client)))
