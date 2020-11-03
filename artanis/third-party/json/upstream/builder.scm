;;; (json builder) --- Guile JSON implementation.

;; Copyright (C) 2013-2020 Aleix Conchillo Flaque <aconchillo@gmail.com>
;; Copyright (C) 2015,2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;
;; This file is part of guile-json.
;;
;; guile-json is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; guile-json is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with guile-json. If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; JSON module for Guile

;;; Code:

(define-module (artanis third-party json upstream builder)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:export (scm->json
            scm->json-string))

;;
;; Miscellaneuos helpers
;;

(define (indent-string pretty level)
  (if pretty (format #f "~v_" (* 2 level)) ""))

;;
;; String builder helpers
;;

(define (unicode->json-string unicode)
  (format #f "\\u~4,'0x" unicode))

(define (unicode->json-surrogate-pair unicode)
  (let* ((u (- unicode #x10000))
         (w1 (+ #xD800 (ash u -10)))
         (w2 (+ #xDC00 (logand u #x3ff))))
    (string-append (unicode->json-string w1)
                   (unicode->json-string w2))))

(define (build-json-unicode c)
  (let* ((value (char->integer c)))
    (cond
     ((< value 32)
      (unicode->json-string value))
     ((<= value 255)
      (string c))
     ((<= value #xFFFF)
      (unicode->json-string value))
     ((<= value #x10FFFF)
      (unicode->json-surrogate-pair value))
     (else (throw 'json-invalid (string c))))))

(define (->string x)
  (cond ((char? x) (make-string 1 x))
        ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        (else x)))

(define (build-string c port solidus unicode)
  (case c
    ((#\" #\\) (format port "\\~c" c))
    ((#\bs) (put-string port "\\b"))
    ((#\ff) (put-string port "\\f"))
    ((#\lf) (put-string port "\\n"))
    ((#\cr) (put-string port "\\r"))
    ((#\ht) (put-string port "\\t"))
    ((#\/) (if solidus
               (put-string port "\\/")
               (put-char port c)))
    (else (if unicode
              (put-string port (build-json-unicode c))
              (put-char port c)))))

(define (json-build-string scm port solidus unicode)
  (put-string port "\"")
  (for-each (lambda (c) (build-string c port solidus unicode))
            (string->list (->string scm)))
  (put-string port "\""))

;;
;; Object builder functions
;;

(define (build-object-pair p port solidus unicode null pretty level)
  (put-string port (indent-string pretty level))
  (json-build-string (car p) port solidus unicode)
  (put-string port ":")
  (build-space port pretty)
  (json-build (cdr p) port solidus unicode null pretty level))

(define (build-newline port pretty)
  (cond (pretty (newline port))))

(define (build-space port pretty)
  (cond (pretty (put-string port " "))))

(define (json-build-object scm port solidus unicode null pretty level)
  (put-string port "{")
  (let ((pairs scm))
    (unless (null? pairs)
      (build-newline port pretty)
      (build-object-pair (car pairs) port solidus unicode null pretty (+ level 1))
      (for-each (lambda (p)
                  (put-string port ",")
                  (build-newline port pretty)
                  (build-object-pair p port solidus unicode null pretty (+ level 1)))
                (cdr pairs))
      (build-newline port pretty)
      (put-string port (indent-string pretty level))))
  (put-string port "}"))

;;
;; Array builder functions
;;

(define (json-build-array scm port solidus unicode null pretty level)
  (put-string port "[")
  (unless (or (null? scm) (zero? (vector-length scm)))
    (build-newline port pretty)
    (vector-for-each (lambda (i v)
                       (cond
                        ((> i 0)
                         (put-string port ",")
                         (build-newline port pretty)))
                       (put-string port (indent-string pretty (+ level 1)))
                       (json-build v port solidus unicode null pretty (+ level 1)))
                     scm)
    (build-newline port pretty)
    (put-string port (indent-string pretty level)))
  (put-string port "]"))

;;
;; Booleans, null and number builder functions
;;

(define (json-build-boolean scm port)
  (put-string port (if scm "true" "false")))

(define (json-build-null port)
  (put-string port "null"))

(define (json-build-number scm port)
  (if (and (rational? scm) (not (integer? scm)))
      (put-string port (number->string (exact->inexact scm)))
      (put-string port (number->string scm))))

;;
;; Main builder functions
;;

(define (json-number? number)
  (and (number? number) (eqv? (imag-part number) 0) (finite? number)))

(define (json-key? scm)
  (or (symbol? scm) (string? scm)))

(define (json-valid? scm null)
  (cond
   ((eq? scm null) #t)
   ((boolean? scm) #t)
   ((json-number? scm) #t)
   ((symbol? scm) #t)
   ((string? scm) #t)
   ((vector? scm) (vector-every (lambda (elem) (json-valid? elem null)) scm))
   ((pair? scm)
    (every (lambda (entry)
             (and (pair? entry)
                  (json-key? (car entry))
                  (json-valid? (cdr entry) null)))
           scm))
   ((null? scm) #t)
   (else (throw 'json-invalid scm))))

(define (json-build scm port solidus unicode null pretty level)
  (cond
   ((eq? scm null) (json-build-null port))
   ((boolean? scm) (json-build-boolean scm port))
   ((json-number? scm) (json-build-number scm port))
   ((symbol? scm) (json-build-string (symbol->string scm) port solidus unicode))
   ((string? scm) (json-build-string scm port solidus unicode))
   ((vector? scm) (json-build-array scm port solidus unicode null pretty level))
   ((or (pair? scm) (null? scm))
    (json-build-object scm port solidus unicode null pretty level))
   (else (throw 'json-invalid scm))))

;;
;; Public procedures
;;

(define* (scm->json scm
                    #:optional (port (current-output-port))
                    #:key
                    (solidus #f) (unicode #f) (null 'null)
                    (validate #t) (pretty #f))
  "Creates a JSON document from native. The argument @var{scm} contains the
native value of the JSON document. Takes one optional argument, @var{port},
which defaults to the current output port where the JSON document will be
written. It also takes a few keyword arguments: @{solidus}: if true, the
slash (/ solidus) character will be escaped (defaults to false), @{unicode}:
if true, unicode characters will be escaped when needed (defaults to false),
@{null}: value for JSON's null (defaults to the 'null symbol), @{validate} :
if true, the native value will be validated before starting to print the JSON
document (defaults to true) and @{pretty}: if true, the JSON document will be
pretty printed (defaults to false).

Note that when using alists to build JSON objects, symbols or numbers might be
used as keys and they both will be converted to strings.
"
  (cond
   ((and validate (json-valid? scm null))
    (json-build scm port solidus unicode null pretty 0))
   (else
    (json-build scm port solidus unicode null pretty 0))))

(define* (scm->json-string scm #:key
                           (solidus #f) (unicode #f) (null 'null)
                           (validate #t) (pretty #f))
  "Creates a JSON document from native into a string. The argument @var{scm}
contains the native value of the JSON document. It also takes a few keyword
arguments: @{solidus}: if true, the slash (/ solidus) character will be
escaped (defaults to false), @{unicode}: if true, unicode characters will be
escaped when needed (defaults to false), @{null}: value for JSON's
null (defaults to the 'null symbol), @{validate} : if true, the native value
will be validated before starting to print the JSON document (defaults to
true) and @{pretty}: if true, the JSON document will be pretty
printed (defaults to false).

Note that when using alists to build JSON objects, symbols or numbers might be
used as keys and they both will be converted to strings.
"
  (call-with-output-string
    (lambda (p)
      (scm->json scm p
                 #:solidus solidus #:unicode unicode #:null null
                 #:pretty pretty #:validate validate))))

;;; (json builder) ends here
