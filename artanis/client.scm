;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2022-2025
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

(define-module (artanis client)
  #:use-module (artanis utils)
  #:use-module (artanis server server-context)
  #:use-module (artanis runner)
  #:use-module (artanis config)
  #:use-module (web response)
  #:use-module (web http)
  #:use-module (web uri)
  #:use-module (srfi srfi-11)
  #:use-module (curl)
  #:export (artanis:http-head
            artanis:http-get
            artanis:http-post
            artanis:http-patch
            artanis:http-delete
            artanis:http-put))

;; It's recommended to use (artanis client) rather than (web client)

(define (gen-headers-list headers)
  (map (lambda (e)
         (string-trim-both
          (call-with-output-string
           (lambda (port)
             (write-header (car e) (cdr e) port)))))
       headers))

(define (request-it url handle headers cert bv?)
  (curl-easy-setopt handle 'url url)
  (curl-easy-setopt handle 'http-version 2)
  (curl-easy-setopt handle 'followlocation #t)
  (cond
   ((not cert)
    (curl-easy-setopt handle 'ssl-verifypeer #f)
    (curl-easy-setopt handle 'ssl-verifyhost #f))
   (else
    (curl-easy-setopt handle 'sslcert cert)))
  (curl-easy-setopt handle 'httpheader
                    (gen-headers-list headers))
  (let* ((ret (call-with-runner
               (lambda ()
                 (DEBUG "artanis-client: do the request...~%")
                 (let ((ret (curl-easy-perform handle bv? #t)))
                   (DEBUG "artanis-client: done the request.~%")
                   ret))))
         (code (curl-error-code))
         (errstr (curl-error-string)))
    (values ret code errstr)))

(define (get-result url method handle headers cert bv?)
  (let-values (((ret code errstr) (request-it url handle headers cert bv?)))
    (when (not ret)
      (curl-easy-cleanup handle)
      (throw 'artanis-error 500 get-result
             (format #f "client error: method `~a', code `~a', errstr `~a'!"
                     method code errstr)))
    (let* ((res (call-with-input-string
                 (car ret)
                 (lambda (port)
                   (let lp ((ret (read-response port)))
                     (cond
                      ((eof-object? (peek-char port)) ret)
                      (else (lp (read-response port))))))))
           (body (cadr ret))
           (status (response-code res)))
      (cond
       ((or (= status 301) (= status 302))
        (let ((new-url (uri->string (assoc-ref (response-headers res)
                                               'location))))
          (get-result new-url method handle headers cert bv?)))
       ((not (zero? code))
        (curl-easy-cleanup handle)
        (throw 'artanis-error 500 get-result
               (format #f "client error: method `~a', code `~a', errstr `~a'!"
                       method code errstr)))
       (else
        (curl-easy-cleanup handle)
        (values res body))))))

(define* (artanis:http-head url #:key (headers '()) (cert #f))
  (let ((handle (curl-easy-init)))
    (curl-easy-setopt handle 'nobody #t)
    (get-result url artanis:http-head handle headers cert #f)))

(define* (artanis:http-get url #:key (headers '()) (cert #f)
                           (bytevector? #f))
  (let ((handle (curl-easy-init)))
    (curl-easy-setopt handle 'httpget #t)
    (get-result url artanis:http-get handle headers cert bytevector?)))

(define* (artanis:http-post url #:key (headers '()) (cert #f) (body #u8(0))
                            (bytevector? #f) (customrequest #f))
  (let ((handle (curl-easy-init)))
    (curl-easy-setopt handle 'httpget #f)
    (curl-easy-setopt handle 'post #t)
    (when customrequest
      (curl-easy-setopt handle 'customrequest customrequest))
    (curl-easy-setopt handle 'postfields body)
    (get-result url artanis:http-post handle headers cert bytevector?)))

(define* (artanis:http-patch url #:key (headers '()) (cert #f) (body #u8(0))
                             (bytevector? #f))
  (artanis:http-post url #:headers headers #:cert cert #:body body
                     #:bytevector? bytevector? #:customrequest "PATCH"))

(define* (artanis:http-delete url #:key (headers '()) (cert #f) (body #u8(0))
                              (bytevector? #f))
  (artanis:http-post url #:headers headers #:cert cert #:body body
                     #:bytevector? bytevector? #:customrequest "DELETE"))

(define* (artanis:http-put url #:key (headers '()) (cert #f) (body #u8(0))
                           (bytevector? #f))
  (artanis:http-post url #:headers headers #:cert cert #:body body
                     #:bytevector? bytevector? #:customrequest "PUT"))
