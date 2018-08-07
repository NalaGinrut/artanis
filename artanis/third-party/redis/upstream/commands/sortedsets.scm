;;; (artanis third-party redis upstream commands sortedsets) --- redis module for Guile.

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

(define-module (artanis third-party redis upstream commands sortedsets)
  #:use-module (artanis third-party redis upstream commands define)
  #:export (zadd
            zcard zcount zincrby
	    zrange zrank
	    zrem zremrangebyrank zremrangebyscore zrevrange
	    zrevrank zscore))

(define (zadd key pairs)
  (apply make-command "ZADD" key (cons-list->list pairs)))

(define (zcard key)
  (make-command "ZCARD" key))

(define (zcount key min max)
  (make-command "ZCOUNT" key (number->string min) (number->string max)))

(define (zincrby key increment member)
  (make-command "ZINCRBY" key (number->string increment) member))

;; TODO: ZINTERSTORE

(define* (zrange key start stop #:optional withscores)
  (if withscores
      (make-command "ZRANGE" key
                    (number->string start)
                    (number->string stop) "WITHSCORES")
      (make-command "ZRANGE" key
                    (number->string start)
                    (number->string stop))))

;; TODO: ZRANGEBYSCORE

(define (zrank key member)
  (make-command "ZRANK" key member))

(define (zrem key members)
  (apply make-command "ZREM" key members))

(define (zremrangebyrank key start stop)
  (make-command "ZREMRANGEBYRANK" key
                (number->string start)
                (number->string stop)))

(define (zremrangebyscore key min max)
  (make-command "ZREMRANGEBYSCORE" key
                (number->string min)
                (number->string max)))

(define* (zrevrange key start stop #:optional withscores)
  (if withscores
      (make-command "ZREVRANGE" key
                    (number->string start)
                    (number->string stop) "WITHSCORES")
      (make-command "ZREVRANGE" key
                    (number->string start)
                    (number->string stop))))

;; TODO: ZREVRANGEBYSCORE

(define (zrevrank key member)
  (make-command "ZREVRANK" key member))

(define (zscore key member)
  (make-command "ZSCORE" key member))

;; TODO: ZUNIONSTORE

;;; (artanis third-party redis upstream commands sortedsets) ends here
