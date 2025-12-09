;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2025
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
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

(define-module (artanis security hash)
  #:use-module (artanis security nss)
  #:use-module (artanis third-party base32)
  #:use-module (artanis third-party base16)
  #:re-export (nss:base64-decode
               nss:base64-encode
               nss:hash

               string->md5
               string->sha-1
               string->sha-224
               string->sha-256
               string->sha-384
               string->sha-512
               generate-rule-uid)
  #:export (base32-decode
            base32-encode

            base16-decode
            base16-encode))

(define (->bv str/bv proc)
  (cond
   ((string? str/bv) (string->bytevector str/bv "iso-8859-1"))
   (((@ (rnrs) bytevector?) str/bv) str/bv)
   (else (throw 'artanis-err 500 proc
                "need string or bytevector!" str/bv))))

(define (->str str/bv pro)
  (cond
   ((string? str/bv) str/bv)
   (((@ (rnrs) bytevector?) str/bv)
    (bytevector->string str/bv "utf-8"))
   (else (throw 'artanis-err 500 proc
                "need string or bytevector!" str/bv))))

(define (base32-encode str/bv)
  (bytevector->base32-string (->bv str/bv base32-encode)))

(define (base32-decode str/bv)
  (base32-string->bytevector (->str str/bv base32-decode)))

(define (base16-encode str/bv)
  (bytevector->base16-string (->bv str/bv base16-encode)))

(define (base16-decode str/bv)
  (base16-string->bytevector (->str str/bv base16-decode)))
