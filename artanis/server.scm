;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014,2015,2016,2018
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

;; ---------------------------------------------------------------------
;; This module is the brand new high performance concurrency server of
;; Artanis, which is based on Guile's powerful delimited-continuations.

(define-module (artanis server)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis env)
  #:use-module (artanis server ragnarok)
  #:use-module (artanis server http)
  #:use-module (artanis server scheduler)
  #:re-export (establish-http-gateway
               get-task-breaker
               protocol-service-open
               break-task
               close-task)
  #:export (init-server-core
            schedule-task
            try-to-recycle-resources))

(define schedule-task
  (lambda ()
    (error
     'schedule-task
     "If you saw this line, it means server-core hasn't been initialized!")))

;; NOTE: There're only 2 places to recycle resources automatically:
;;       1. When there's no available port to allocate.
;;       2. When there's no availble DB connection in the pool.
;; WARN: Users may call it freely, but please notice that timeout checking
;;       will scan all the existing requested connection to drop timeout
;;       connections. If you do this frequently, you'll lose the benefit of
;;       epoll.
(define try-to-recycle-resources
  (lambda ()
    (error
     'try-to-recycle-resources
     "If you saw this line, it means server-core hasn't been initialized!")))

(define (init-server-core)
  (set! schedule-task (get-task-breaker))
  (set! try-to-recycle-resources (get-resources-collector))
  (protocol-add! 'http (new-http-protocol)))
