;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2025-2026
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
  #:use-module (ice-9 iconv)
  #:use-module ((rnrs) #:select (bytevector?
                                 bytevector-length
                                 bytevector-u8-ref))
  #:re-export (nss:base64-decode
               nss:base64-encode

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
            base16-encode

            string->hmac-md5
            string->hmac-sha-1
            string->hmac-sha-256
            string->hmac-sha-384
            string->hmac-sha-512
            string->hmac-sha-224))

(define (->bv str/bv proc)
  (cond
   ((string? str/bv) (string->bytevector str/bv "iso-8859-1"))
   ((bytevector? str/bv) str/bv)
   (else (throw 'artanis-err 500 proc
                "need string or bytevector!" str/bv))))

(define (->str str/bv proc)
  (cond
   ((string? str/bv) str/bv)
   ((bytevector? str/bv) (bytevector->string str/bv "iso-8859-1"))
   (else (throw 'artanis-err 500 proc
                "need string or bytevector!" str/bv))))

(define *hex-chars* "0123456789abcdef")

(define (bv->hex bv)
  (let* ((len (bytevector-length bv))
         (out (make-string (* len 2))))
    (let lp ((i 0))
      (when (< i len)
        (let ((b (bytevector-u8-ref bv i)))
          (string-set! out (* i 2)
                       (string-ref *hex-chars* (ash b -4)))
          (string-set! out (+ (* i 2) 1)
                       (string-ref *hex-chars* (logand b #xf))))
        (lp (+ i 1))))
    out))

(define (base32-encode str/bv)
  (bytevector->base32-string (->bv str/bv base32-encode)))

(define* (base32-decode str/bv #:key (bv? #f))
  (let ((ret (base32-string->bytevector (->str str/bv base32-decode))))
    (if (not bv?)
        (->str ret base32-decode)
        ret)))

(define (base16-encode str/bv)
  (bytevector->base16-string (->bv str/bv base16-encode)))

(define* (base16-decode str/bv #:key (bv? #f))
  (let ((ret (base16-string->bytevector (->str str/bv base16-decode))))
    (if (not bv?)
        (->str ret base16-decode)
        ret)))

;; Public HMAC API — same calling convention as string->sha-256 etc.
;; Each takes a key and data (string or bytevector), returns a hex string.

(define (string->hmac-sha-1 key str/bv)
  (bv->hex (nss:hmac-raw CKM_SHA1_HMAC SHA1_DIGEST_LENGTH
                         (->bv key string->hmac-sha-1)
                         (->bv str/bv string->hmac-sha-1))))

(define (string->hmac-md5 key str/bv)
  (bv->hex (nss:hmac-raw CKM_MD5_HMAC MD5_DIGEST_LENGTH
                         (->bv key string->hmac-md5)
                         (->bv str/bv string->hmac-md5))))

(define (string->hmac-sha-256 key str/bv)
  (bv->hex (nss:hmac-raw CKM_SHA256_HMAC SHA256_DIGEST_LENGTH
                         (->bv key string->hmac-sha-256)
                         (->bv str/bv string->hmac-sha-256))))

(define (string->hmac-sha-384 key str/bv)
  (bv->hex (nss:hmac-raw CKM_SHA384_HMAC SHA384_DIGEST_LENGTH
                         (->bv key string->hmac-sha-384)
                         (->bv str/bv string->hmac-sha-384))))

(define (string->hmac-sha-512 key str/bv)
  (bv->hex (nss:hmac-raw CKM_SHA512_HMAC SHA512_DIGEST_LENGTH
                         (->bv key string->hmac-sha-512)
                         (->bv str/bv string->hmac-sha-512))))

;; NOTE: NSS is not guaranteed to implement SHA-224
(define (string->hmac-sha-224 key str/bv)
  (throw 'artanis-err 500 string->hmac-sha-224
         "HMAC-SHA-224 is not supported by this NSS version"))
