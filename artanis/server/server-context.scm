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
  #:export (make-ragnarok-server
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
            task-prio))

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
   conn   ; connect socket fd
   kont   ; delimited continuation
   prio)) ; priority
