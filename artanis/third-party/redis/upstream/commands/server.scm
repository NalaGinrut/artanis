;;; (artanis third-party redis upstream commands server) --- redis module for Guile.

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

(define-module (artanis third-party redis upstream commands server)
  #:use-module (artanis third-party redis upstream commands define)
  #:export (bgrewriteaof
            bgsave client-kill client-list
	    client-getname client-setname config-get config-set
	    config-resetstat dbsize debug-object debug-segfault
	    flushall flushdb info lastsave monitor save
	    shutdown slaveof slowlog sync time))

(define (bgrewriteaof)
  (make-command "BGREWRITEAOF"))

(define (bgsave)
  (make-command "BGSAVE"))

(define (client-kill ip port)
  (make-command "CLIENT KILL" (string-join ":" ip (number->string port))))

(define (client-list)
  (make-command "CLIENT LIST"))

(define (client-getname)
  (make-command "CLIENT GETNAME"))

(define (client-setname connection-name)
  (make-command "CLIENT SETNAME" connection-name))

(define (config-get parameter)
  (make-command "CONFIG GET" parameter))

(define (config-set parameter value)
  (make-command "CONFIG SET" parameter value))

(define (config-resetstat)
  (make-command "CONFIG RESETSTAT"))

(define (dbsize)
  (make-command "DBSIZE"))

(define (debug-object key)
  (make-command "DEBUG OBJECT" key))

(define (debug-segfault)
  (make-command "DEBUG SEGFAULT"))

(define (flushall)
  (make-command "FLUSHALL"))

(define (flushdb)
  (make-command "FLUSHDB"))

(define* (info #:optional section)
  (make-command "INFO" section))

(define (lastsave)
  (make-command "LASTSAVE"))

(define (monitor)
  (make-command "MONITOR"))

(define (save)
  (make-command "SAVE"))

(define* (shutdown #:optional (save -1))
  (cond
   ((eq? save -1) (make-command "SHUTDOWN"))
   (else
    (if save
        (make-command "SHUTDOWN" "SAVE")
        (make-command "SHUTDOWN" "NOSAVE")))))

(define (slaveof host port)
  (make-command "SLAVEOF" host (number->string port)))

(define* (slowlog subcommand #:optional argument)
  (make-command "SLOWLOG" subcommand argument))

(define (sync)
  (make-command "SYNC"))

(define (time)
  (make-command "TIME"))

;;; (artanis third-party redis upstream commands server) ends here
