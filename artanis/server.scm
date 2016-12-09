;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014,2015,2016
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
            schedule-task))

(define schedule-task
  (lambda ()
    (error
     'schedule-task
     "If you saw this line, it means server-core hasn't been initialized!")))

(define (init-server-core)
  (set! schedule-task (get-task-breaker))
  (protocol-add! 'http (new-http-protocol)))
