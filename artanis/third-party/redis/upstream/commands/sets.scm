;;; (artanis third-party redis upstream commands sets) --- redis module for Guile.

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

(define-module (artanis third-party redis upstream commands sets)
  #:use-module (artanis third-party redis upstream commands define)
  #:export (sadd
            scard sdiff sdiffstore
	    sinter sinterstore sismember smembers
	    smove spop srandmember srem
	    sunion sunionstore))

(define (sadd key members)
  (apply make-command "SADD" key members))

(define (scard key)
  (make-command "SCARD" key))

(define (sdiff keys)
  (apply make-command "SDIFF" keys))

(define (sdiffstore destination keys)
  (apply make-command "SDIFFSTORE" destination keys))

(define (sinter keys)
  (apply make-command "SINTER" keys))

(define (sinterstore destination keys)
  (apply make-command "SINTERSTORE" destination keys))

(define (sismember key member)
  (make-command "SISMEMBER" key member))

(define (smembers key)
  (make-command "SMEMBERS" key))

(define (smove source destination member)
  (make-command "SMOVE" source destination member))

(define (spop key)
  (make-command "SPOP" key))

(define* (srandmember key #:optional count)
  (if count
      (make-command "SRANDMEMBER" key count)
      (make-command "SRANDMEMBER" key)))

(define (srem key members)
  (apply make-command "SREM" key members))

(define (sunion keys)
  (apply make-command "SUNION" keys))

(define (sunionstore destination keys)
  (apply make-command "SUNIONSTORE" destination keys))

;;; (artanis third-party redis upstream commands sets) ends here
