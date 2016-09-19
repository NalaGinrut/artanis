;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2016
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  Artanis is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License and GNU
;;  Lesser General Public License published by the Free Software
;;  Foundation, either version 3 of the License, or (at your option)
;;  any later version.

;;  Artanis is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License and GNU Lesser General Public License
;;  for more details.

;;  You should have received a copy of the GNU General Public License
;;  and GNU Lesser General Public License along with this program.
;;  If not, see <http://www.gnu.org/licenses/>.

(define-module (artanis server http)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis server server-context)
  #:export (new-http-protocol))

;; NOTE: HTTP service is established by default, so it's unecessary to do any
;;       openning work.
(define (http-open server client)
  #t)

(define (http-read server client)
  #t)

(define (http-write server client)
  #t)

(define (http-close server client)
  #t)

(define-protocol http http-open http-read http-write http-close)

(define (new-http-protocol)
  (make-protocol 'http http-open http-read http-write http-close (make-hash-table)))
