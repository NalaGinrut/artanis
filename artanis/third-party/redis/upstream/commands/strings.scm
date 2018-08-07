;;; (artanis third-party redis upstream commands strings) --- redis module for Guile.

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

(define-module (artanis third-party redis upstream commands strings)
  #:use-module (artanis third-party redis upstream commands define)
  #:export (append
            bitcount bitop decr decrby get
	    getbit getrange getset incr incrby incrbyfloat
	    mget mset msetnx psetex set setbit
	    setex setnx setrange strlen))

(define (append key value)
  (make-command "APPEND" key value))

(define* (bitcount key #:optional (start 0) (end -1))
  (make-command "BITCOUNT" key (number->string start) (number->string end)))

(define (bitop operation destkey keys)
  (apply make-command "BITOP" operation destkey keys))

(define (decr key)
  (make-command "DECR" key))

(define (decrby key decrement)
  (make-command "DECRBY" key (number->string decrement)))

(define (get key)
  (make-command "GET" key))

(define (getbit key offset)
  (make-command "GETBIT" key (number->string offset)))

(define (getrange key start end)
  (make-command "GETRANGE" key (number->string start) (number->string end)))

(define (getset key value)
  (make-command "GETSET" key value))

(define (incr key)
  (make-command "INCR" key))

(define (incrby key increment)
  (make-command "INCRBY" key (number->string increment)))

(define (incrbyfloat key increment)
  (make-command "INCRBYFLOAT" key (number->string increment)))

(define (mget keys)
  (apply make-command "MGET" keys))

(define (mset pairs)
  (apply make-command "MSET" (cons-list->list pairs)))

(define (msetnx pairs)
  (apply make-command "MSETNX" (cons-list->list pairs)))

(define (psetex key milliseconds value)
  (make-command "PSETEX" key (number->string milliseconds) value))

;; TODO: add extra arguments
(define (set key value)
  (make-command "SET" key value))

(define (setbit key offset value)
  (make-command "SETBIT" key (number->string offset) value))

(define (setex key seconds value)
  (make-command "SETEX" key (number->string seconds) value))

(define (setnx key value)
  (make-command "SETNX" key value))

(define (setrange key offset value)
  (make-command "SETRANGE" key (number->string offset) value))

(define (strlen key)
  (make-command "STRLEN" key))

;;; (artanis third-party redis upstream commands strings) ends here
