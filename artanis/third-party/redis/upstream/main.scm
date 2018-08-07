;;; (artanis third-party redis upstream main) --- redis module for Guile.

;; Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo@gmail.com>
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

(define-module (artanis third-party redis upstream main)
  #:use-module (artanis third-party redis upstream connection)
  #:use-module (artanis third-party redis upstream utils)
  #:export (redis-connect
            redis-close
            redis-send))

(define* (redis-connect #:key (host "127.0.0.1") (port 6379))
  "Establish a connection to the redis server at the given @var{host}
and @var{port}. The @var{host} defaults to 127.0.0.1 and @var{port}
defaults to 6379. Returns a redis connection."
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (connect sock AF_INET (inet-pton AF_INET host) port)
    (make-connection host port sock)))

(define (redis-close connection)
  "Close the @var{connection} to the redis server."
  (shutdown (redis-socket connection) 2))

(define (redis-send connection commands)
  "Send the given list of @var{commands} to the redis
@var{connection}. @var{commands} can be a single command or a list of
commands. For a list of commands, a list of all the replies is
returned."
  (send-commands connection commands)
  (receive-commands connection commands))

;;; (artanis third-party redis upstream main) ends here
