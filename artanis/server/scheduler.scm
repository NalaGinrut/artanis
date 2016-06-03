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

(define-module (artanis server scheduler)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis server epoll)
  #:use-module (artanis server server-context)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  #:use-module ((rnrs) #:select (define-record-type))
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:export (ragnarok-scheduler))

(define (compute-prio proto client server)
  ;; TODO: how to compute priority
  #t)

;; NOTE: When we fetch the current task from work-queue, we'll remove it
;;       (say, the head) from work-queue. We don't remove the head when we close it.
(define (save-current-task! k proto client server)
  (let ((wt (ragnarok-server-work-table server))
        (task (make-task (car client) k (compute-prio proto client server))))
    (DEBUG "Save current task!~%")
    (hashv-set! wt (car client) k)))

(define (ragnarok-scheduler k proto server client)
  (DEBUG "Enter ragnarok scheduler!~%")
  (save-current-task! k proto client server)
  #t)
