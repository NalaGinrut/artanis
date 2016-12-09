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

;; =============================================================================
;; Asynchronous I/O wrapper
;;
;; Actually this module is unnecessary for Guile-2.2, but I want to give unified
;; interface for users who want to add their own extra server engine to Artanis.
;; So one may use this aio interface to make sure Artanis is happy.
;; =============================================================================

(define-module (artanis server aio)
  #:use-module (artanis utils)
  #:use-module (artanis server scheduler)
  #:use-module (ice-9 suspendable-ports)
  #:export (async-read-waiter
            async-write-waiter
            define-artanis-read-waiter
            define-artanis-write-waiter))

(define (async-read-waiter port)
  (DEBUG "Async read!~%")
  (DEBUG "I would break ~a~%" port)
  (break-task))

(define (async-write-waiter port)
  (DEBUG "Async read!~%")
  (DEBUG "I would break ~a~%" port)
  (break-task))

;; NOTE: Most of the time, you just need to parameterize async-read-waiter or
;;       async-write-waiter, that's enough. But if you want to do more things
;;       when EAGAIN happens, please use these helper functions:
(define-syntax define-artanis-write-waiter
  (syntax-rules ()
    ((_ (name port . args) body ...)
     (define (name port . args)
       body ...
       (async-write-waiter port)))))

(define-syntax define-artanis-read-waiter
  (syntax-rules ()
    ((_ (name port . args) body ...)
     (define (name port . args)
       body ...
       (async-read-waiter port)))))
