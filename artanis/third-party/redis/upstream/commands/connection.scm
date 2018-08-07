;;; (artanis third-party redis upstream commands connection) --- redis module for Guile.

;; Copyright (C) 2013-2017 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of guile-redis.
;;
;; guile-redis is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; guile-redis is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with guile-redis; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; Redis module for Guile

;;; Code:

(define-module (artanis third-party redis upstream commands connection)
  #:use-module (artanis third-party redis upstream main)
  #:use-module (artanis third-party redis upstream utils)
  #:use-module (artanis third-party redis upstream commands define)
  #:export (auth echo ping quit select))

(define (auth password)
  (make-command "AUTH" password))

(define (echo message)
  (make-command "ECHO" message))

(define* (ping #:optional (message #f))
  (if message
      (make-command "PING" message)
      (make-command "PING")))

(define (quit)
  (make-command
   "QUIT"
   #:reply
   (lambda (conn)
     (read-reply conn)
     (redis-close conn))))

(define (select index)
  (make-command "SELECT" index))

(define (swapdb index1 index2)
  (make-command "SWAPDB" index1 index2))

;;; (artanis third-party redis upstream commands connection) ends here
