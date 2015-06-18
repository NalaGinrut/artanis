;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  === Test helper functions ===
;;  Copyright (C) 2015
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

(define-module (test-suite helper)
  #:use-module (artanis utils)
  #:use-module (artanis artanis)
  #:use-module (artanis page)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module ((rnrs) #:select (bytevector? bytevector=? get-string-all
                                 string->utf8))
  #:export (*unified-modify-time*
            *unified-modify-time-header*
            *unified-global-date*
            *default-unified-headers*
            make-unified-header
            test-from-request
            responses-equal?
            upload-file-verify))

(define *unified-modify-time* ((@ (srfi srfi-19) current-time)))
(define *unified-modify-time-header*
  (get-global-date
   (cons ((@ (srfi srfi-19) time-second) *unified-modify-time*)
         ((@ (srfi srfi-19) time-nanosecond) *unified-modify-time*))))
(define *unified-global-date* (get-global-date))

(define (make-unified-header type)
  `((server . ,artanis-version)
    (date . ,*unified-global-date*)
    (last-modified . ,*unified-modify-time-header*)
    (content-type . ,type)))

(define *default-unified-headers*
  (make-unified-header '(text/html (charset . "utf-8"))))

(define* (test-from-request rq-str #:optional (debug #f))
  (let* ((rq (read-request (open-input-string rq-str)))
         (body (read-request-body rq))
         (null (open-output-file "/dev/null"))
         (out (if debug (current-output-port) null))
         (err (if debug (current-error-port) null)))
    (parameterize ((current-output-port out)
                   (current-error-port err))
      (receive (res b) (server-handler rq body)
               ((@@ (web server) sanitize-response) rq res b)))))

(define (headers-equal? h1 h2)
  (every (lambda (f)
           (equal? (assq-ref h1 f) (assq-ref h2 f)))
         h1))

(define (body-equal? b1 b2)
  (define-syntax-rule (-> x)
    (cond
     ((bytevector? x) x)
     ((string? x) (string->utf8 x))
     (else (error body-equal? "Wrong body type!" x))))
  (bytevector=? (-> b1) (-> b2)))
  
(define (responses-equal? r1 body1 r2 body2)
  (and (equal? (response-version r1) (response-version r2))
       (equal? (response-code r1) (response-code r2))
       (equal? (response-reason-phrase r1) (response-reason-phrase r2))
       (headers-equal? (response-headers r1) (response-headers r2))
       (body-equal? body1 body2)))

(define (upload-file-verify filepath expect-content)
  (and (file-exists? filepath)
       (string=? (call-with-input-file filepath get-string-all) expect-content)))
