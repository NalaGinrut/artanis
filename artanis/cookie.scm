;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  Artanis is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  Artanis is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (artanis cookie)
  #:use-module (artanis utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (web request)
  #:export (make-cookie cookie?
            cookie-nvp cookie-nvp!
            cookie-expir cookie-expir!
            cookie-path cookie-path!
            cookie-domain cookie-domain!
            cookie-secure cookie-secure!
            cookie->header-string))                        

(define-record-type cookie
  (make-cookie nvp expir path domain secure ho)
  cookie?
  (nvp cookie-nvp cookie-nvp!)          ; Name-Value-Pairs of the cookie
  (expir cookie-expir cookie-expir!)    ; The expiration in Greenwich Mean Time
  (path cookie-path cookie-path!)       ; The path the cookie is good for
  (domain cookie-domain cookie-domain!) ; The domain the cookie is good for
  ;; keep cookie communication limited to encrypted transmission
  (secure cookie-secure cookie-secure!) ; The secure need of cookie
  (ho cookie-httponly cookie-httponly!)); http-only

(define (nvp name v-ref)
  (lambda (c)
  (list (list name (v-ref c)))))

(define (nvp->string nvp)
  (let ((v (cadr nvp)))
    (if (boolean? v)
         (and v (car nvp))
         (format #f "~a=~a" (car nvp) v))))

(define nvp-accessors
  (list cookie-nvp
        (nvp "Expires" cookie-expir)
        (nvp "Path" cookie-path)
        (nvp "Domain" cookie-domain)
        (nvp "Secure" cookie-secure)
        (nvp "HttpOnly" cookie-httponly)))

(define (cookie->header-string cookie)
  (let ((nvps (append-map
               (lambda (nvp-ref)
                 (filter-map nvp->string (nvp-ref cookie)))
               nvp-accessors)))
    (string-join nvps ";")))
