;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2022
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

(define-module (artanis client)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (srfi srfi-11)
  #:use-module (curl)
  #:export (artanis:http-get
            artanis:http-post))

;; It's recommend to use (artanis client) rather than (web client)

(define (request-it url handle headers cert)
  (curl-easy-setopt handle 'url url)
  (curl-easy-setopt handle 'http-version 2)
  (when (not cert)
    (curl-easy-setopt handle 'ssl-verifypeer #f))
  (curl-easy-setopt handle 'httpheader
                    (map (lambda (e)
                           (format #f "~a: ~a"
                                   (string-capitalize (symbol->string (car e)))
                                   (cdr e)))
                         headers))
  (let ((ret (curl-easy-perform handle #f #t))
        (code (curl-error-code))
        (errstr (curl-error-string)))
    (values ret code errstr)))

(define (get-result url method handle headers cert)
  (let-values (((ret code errstr) (request-it url handle headers cert)))
    (let* ((res (call-with-input-string (car ret) read-response))
           (body (cdr ret))
           (status (response-code res)))
      (cond
       ((or (= status 301) (= status 302))
        (let ((new-url (uri->string (assoc-ref (response-headers res) 'location))))
          (get-result new-url method handle headers cert)))
       ((not (zero? code))
        (curl-easy-cleanup handle)
        (throw 'artanis-error 500 method errstr))
       (else
        (curl-easy-cleanup handle)
        (values res body))))))

(define* (artanis:http-head url #:key (headers '()) (cert #f))
  (let ((handle (curl-easy-init)))
    (curl-easy-setopt handle 'nobody #t)
    (get-result url artanis:http-head handle headers cert)))

(define* (artanis:http-get url #:key (headers '()) (cert #f))
  (let ((handle (curl-easy-init)))
    (curl-easy-setopt handle 'httpget #t)
    (get-result url artanis:http-get handle headers cert)))

(define* (artanis:http-post url #:key (headers '()) (cert #f) (body '()))
  (let ((handle (curl-easy-init)))
    (curl-easy-setopt handle 'httpget #f)
    (curl-easy-setopt handle 'post #t)
    (curl-easy-setopt handle 'postfields body)
    (get-result url artanis:http-post handle headers cert)))
