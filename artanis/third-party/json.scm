;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014,2015,2017
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

;;; Commentary:

;; JSON module for Guile

;;; Code:

(define-module (artanis third-party json)
  #:use-module (artanis env)
  #:use-module (artanis third-party json upstream builder)
  #:use-module (artanis third-party json upstream parser)
  #:use-module (artanis third-party json upstream syntax)
  #:use-module (artanis irregex)
  #:use-module (srfi srfi-1)
  #:export (->json-string
            json-ref
            json-set!)
  #:re-export (scm->json
               scm->json-string
               json->scm
               json-string->scm
               json-parser?
               json-parser-port
               json))

(define *word-re* (string->irregex "^[a-zA-Z0-9_]+$"))

(define (make-json-checker fix re err)
  (lambda (jstr)
    (let ((r (fix (irregex-match re jstr))))
      (unless r (format (current-error-port) err))
      r)))

;; check whether JSON object is an array, it is considered to be harmful to
;; return an array directly.
(define *array-re* (string->irregex "^\\[[^\\]]*\\]$"))
(define not-an-array
  (make-json-checker not *array-re* "[ERROR] return JSON as an array is dangerous!~%"))

(define *checker-list*
  (list not-an-array))

(define (validate-json jstr)
  (if (every (lambda (checker) (checker jstr)) *checker-list*)
      jstr
      (throw 'artanis-err 500 validate-json "JSON safe check didn't pass!" jstr)))

;; NOTE: I'll let you specify the regexp for validating your own JSONP in case
;;       the default regexp is not enough for you, but AT YOUR OWN RISK!!!
;; NOTE: #:security-check? is recommended to enable when it's in debug/test.
(define* (->json-string sxml #:key (jsonp #f) (jsonp-regexp *word-re*)
                        (securty-check? #f))
  (cond
   (jsonp
    (if (irregex-match jsonp-regexp jsonp)
        (format #f "~a(~a)" jsonp (validate-json (scm->json-string sxml)))
        ;; if jsonp contains a non-word character, this could be an XSS attack.
        (throw 'artanis-err 400 ->json-string
               "Invalid JSONP, possibly be an XSS attack!" jsonp)))
   (else ((if securty-check? validate-json identity) (scm->json-string sxml)))))

(define (json-ref json-obj key)
  (let ((val (hash-ref json-obj key)))
    (if (string? val)
        ((current-encoder) val)
        val)))

(define (json-set! json-obj key val)
  (hash-set! json-obj
             ((current-encoder) key)
             (if (string? val)
                 ((current-encoder) val)
                 val)))
