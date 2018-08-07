;;; (artanis third-party redis upstream commands hashes) --- redis module for Guile.

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

(define-module (artanis third-party redis upstream commands hashes)
  #:use-module (artanis third-party redis upstream commands define)
  #:export (hdel hexists hget hgetall
		 hincrby hincrbyfloat hkeys hlen
		 hmget hmset hset hsetnx
		 hstrlen hvals hscan))

(define* (hdel key field #:rest fields)
  (apply make-command "HDEL" key field fields))

(define (hexists key field)
  (make-command "HEXISTS" key field))

(define (hget key field)
  (make-command "HGET" key field))

(define (hgetall key)
  (make-command "HGETALL" key))

(define (hincrby key field increment)
  (make-command "HINCRBY" key field (number->string increment)))

(define (hincrbyfloat key field increment)
  (make-command "HINCRBYFLOAT" key field (number->string increment)))

(define (hkeys key)
  (make-command "HKEYS" key))

(define (hlen key)
  (make-command "HLEN" key))

(define (hmget key fields)
  (apply make-command "HMGET" key fields))

(define (hmset key pairs)
  (apply make-command "HMSET" key (cons-list->list pairs)))

(define (hset key field value)
  (make-command "HSET" key field value))

(define (hsetnx key field value)
  (make-command "HSETNX" key field value))

(define (hstrlen key field)
  (make-command "HSTRLEN" key field))

(define (hvals key)
  (make-command "HVALS" key))

(define* (hscan key cursor #:key (match #f) (count #f))
  (let* ((args (list key cursor))
         (args (append args (if match `("MATCH" ,match) '())))
         (args (append args (if count `("COUNT" ,count) '()))))
    (apply make-command "HSCAN" args)))

;;; (artanis third-party redis upstream commands hashes) ends here
