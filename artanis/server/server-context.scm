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
  #:use-module (artanis utils)
  #:use-module (artanis env)
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
            current-work-table

            make-work-table
            work-table?
            work-table-content
            work-table-mutex

            make-protocol
            protocol?
            protocol-name
            protocol-open
            protocol-read
            protocol-write
            protocol-close
            protocol-rt
            define-protocol

            make-redirector
            redirector?
            redirector-type
            redirector-content
            redirector-count
            redirector-mutex
            register-redirector!
            get-the-redirector-of-protocol
            
            make-task
            task?
            task-conn
            task-kont
            task-prio

            make-ragnarok-client
            ragnarok-client?
            client-sockport
            client-sockport-decriptor
            client-connecting-port

            remove-from-work-table!
            add-a-task-to-work-table!
            get-task-from-work-table

            specified-proto?
            register-proto!))

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
   work-table ; a table list contains continuations
   ready-queue ; a queue contains connect socket
   event-set))

(define (current-work-table server)
  (list-ref (ragnarok-server-work-table server)
            (current-worker)))

;; A table contains continuations
;; * content: the continuation
;; * mutex: a mutex for lock
(define-record-type work-table
  (fields content mutex))

(define-record-type protocol
  (fields name  ; the name of the protocol (in symbol)
          open  ;
          read  ; server -> client -> ANY
          write ; server -> client -> response -> str/bv -> ANY
          close ; server -> ANY
          rt))  ; rt stands for redirect-table

;; NOTE: Any methods in protocol shouldn't be bound to Ragnarok.
;;       We have to make sure the developers could implement their
;;       own server-core.
(define-syntax-rule (define-protocol name open read write close)
  (define name
    (make-protocol 'name open read write close (make-hash-table))))

(define-record-type redirector
  (fields
   type        ; proxy or websocket
   content     ; remote port or #f
   count       ; transfered bytes
   mutex))     ; a mutex for locking

;; A redirectors table holds all the redirectors as the value, and the
;; client port descriptor is the key.
;; NOTE: There're 2 kinds of redirector types:
;; 1. 'proxy
;;    For redirecting to the remote socket port.
;;    The content field will be the remote socket port.
;; 2. 'websocket
;;    For regular usage of websocket. If the http-read detects the current
;;    client was bound to a websocket, then http-read won't read its body.
;;    The body reading will be delayed to be in the handler, users have to
;;    use :websocket command to read the parsed body according to the
;;    registered protocol parser.
;;    The content field will be #f/
(define (make-redirectors-table) (make-hash-table))

(::define (get-the-redirector-of-protocol server proto)
  (hashq-ref
   (ragnaork-server-services server)
   (protocol-name proto)))

(define (register-redirector! server client proto type content)
  (hashv-set! (get-the-redirector-of-protocol server proto)
              (client-sockport-decriptor client)
              (make-redirector
               type
               content
               0
               (make-mutex))))

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
;; NOTE: The remote connection wrapped in Guile socket port.
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
;; NOTE: It's actually sin_addr.s_addr, which is the remote IP.
(::define (client-addr c)
  (:anno: (ragnarok-client) -> int)
  (sockaddr:addr (client-details c)))

(::define (address->ip c)
  (:anno: (ragnarok-client) -> string)
  (inet-ntop (get-family) (client-addr c)))

;; ragnarok-client -> integer
;; NOTE: Different from listenning-port
;; NOTE: This is socket port, say, ip:port, don't be confused with Guile port.
(::define (client-connecting-port c)
  (:anno: (ragnarok-client) -> int)
  (sockaddr:port (client-details c)))

;; work-table -> ragnarok-client -> ANY
(::define (remove-from-work-table! wt client)
  (:anno: (work-table ragnarok-client) -> ANY)
  (hashv-remove! (work-table-content wt) (client-sockport-decriptor client)))

;; work-table -> sockport -> task -> ANY
(::define (add-a-task-to-work-table! wt client task)
  (:anno: (work-table sockport task) -> ANY)
  (hashv-set! wt (client-sockport-decriptor client) task))

;; work-table -> ragnarok-client -> task
(::define (get-task-from-work-table wt client)
  (:anno: (work-table ragnarok-client) -> task)
  (hashv-ref wt (client-sockport-decriptor client)))

;; This is a table to record client and proto pairs (CP pairs), client is the
;; key while protocol name is the value.
;; NOTE: The CP pairs should be recorded in http-open handler, 
(define *proto-conn-table* (make-hash-table))

(define (specified-proto? client)
  (and=> (hashv-ref *proto-conn-table* (client-sockport-decriptor client))
         lookup-protocol))

(define (register-proto! client protoname)
  (hashv-set! *proto-conn-table* (client-sockport-decriptor client) protoname))
