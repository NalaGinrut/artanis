;;; (artanis third-party redis upstream commands lists) --- redis module for Guile.

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

(define-module (artanis third-party redis upstream commands lists)
  #:use-module (artanis third-party redis upstream commands define)
  #:export (blpop brpop brpoplpush lindex linsert
		  llen lpop lpush lpushx lrange
		  lrem lset ltrim rpop rpoplpush
		  rpush rpushx))

(define (blpop keys timeout)
  (apply make-command "BLPOP" keys (number->string timeout)))

(define (brpop keys timeout)
  (apply make-command "BRPOP" keys (number->string timeout)))

(define (brpoplpush source destination timeout)
  (make-command "BRPOPLPUSH" source destination (number->string timeout)))

(define (lindex key index)
  (make-command "LINDEX" key (number->string index)))

(define (linsert key where pivot value)
  (case where
    ((BEFORE) (make-command "LINSERT" key "BEFORE" pivot value))
    ((AFTER) (make-command "LINSERT" key "AFTER" pivot value))
    (else (throw 'redis-error "Invalid LINSERT position"))))

(define (llen key)
  (make-command "LLEN" key))

(define (lpop key)
  (make-command "LPOP" key))

(define (lpush key values)
  (apply make-command "LPUSH" key values))

(define (lpushx key value)
  (make-command "LPUSHX" key value))

(define (lrange key start stop)
  (make-command "LRANGE" key (number->string start) (number->string stop)))

(define (lrem key count value)
  (make-command "LREM" key (number->string count) value))

(define (lset key index value)
  (make-command "LSET" key (number->string index) value))

(define (ltrim key start stop)
  (make-command "LTRIM" key (number->string start) (number->string stop)))

(define (rpop key)
  (make-command "RPOP" key))

(define (rpoplpush source destination)
  (make-command "RPOPLPUSH" source destination))

(define (rpush key values)
  (apply make-command `("RPUSH" ,key ,@values)))

(define (rpushx key value)
  (make-command "RPUSHX" key value))

;;; (artanis third-party redis upstream commands lists) ends here
