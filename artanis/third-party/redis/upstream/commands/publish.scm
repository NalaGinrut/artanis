;;; (artanis third-party redis upstream commands publish) --- redis module for Guile.

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

(define-module (artanis third-party redis upstream commands publish)
  #:use-module (artanis third-party redis upstream commands define)
  #:export (psubscribe
            pubsub
	    publish punsubscribe
	    subscribe unsubscribe))

(define (psubscribe patterns)
  (apply make-command "PSUBSCRIBE" patterns))

(define* (pubsub subcommand #:optional arguments)
  (case subcommand
    ((CHANNELS)
     (if arguments
         (apply make-command "PUBSUB" "CHANNELS" arguments)
         (apply make-command "PUBSUB" "CHANNELS")))
    ((NUMSUB)
     (if arguments
         (apply make-command "PUBSUB" "NUMSUB" arguments)
         (apply make-command "PUBSUB" "NUMSUB")))
    ((NUMPAT)
     (apply make-command "PUBSUB" "NUMPAT"))
    (else (throw 'redis-error "Invalid PUBSUB command"))))

(define (publish channel message)
  (make-command "PUBLISH" channel message))

(define (punsubcribe patterns)
  (apply make-command "PUNSUBCRIBE" patterns))

(define (subscribe channels)
  (apply make-command "SUBSCRIBE" channels))

(define (unsubscribe channels)
  (apply make-command "UNSUBSCRIBE" channels))

;;; (artanis third-party redis upstream commands publish) ends here
