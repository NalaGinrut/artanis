;;; (redis connection) --- redis module for Guile.

;; Copyright (C) 2013-2018 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of guile-redis.
;;
;; guile-redis is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; guile-redis is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with guile-redis. If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; Redis module for Guile

;;; Code:

(define-module (artanis third-party redis upstream connection)
  #:use-module (srfi srfi-9)
  #:export (make-connection
            redis-connection?
            redis-host
            redis-port
            redis-socket))

(define-record-type <redis-connection>
  (make-connection host port sock)
  redis-connection?
  (host redis-host)
  (port redis-port)
  (sock redis-socket))

;;; (artanis third-party redis upstream connection) ends here
