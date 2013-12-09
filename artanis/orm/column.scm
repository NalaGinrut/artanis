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

(define-module (artanis orm column)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis ssql)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (create-db-column db-column:dump))

(define-syntax-rule (compound-type? type)
  (and (list? type) (= (length type) 2)))

;; This is an inner helper macro, so don't hate it, since common user never meet it.
(define-syntax ->
  (syntax-rules (<<)
    ((_ name type)
     (if (compound-type? type)
         (format #f "\"~a\" \"~a(~a)\"" name (car type) (cadr type))
         (format #f "\"~a\" \"~a\"" name type)))
    ((_ name type << constraint)
     (if (compound-type? type)
         (format #f "\"~a\" \"~a(~a)\" \"~a\"" name (car type) (cadr type) constraint)
         (format #f "\"~a\" \"~a\" \"~a\"" name type constraint)))))

;; * NAME should be a symbol, although it's OK for a string
;; * TYPE is a list whose head *must* be a symbol, 
;;   and the second element *must* be an integer:
;;   'int as int
;;   '(varchar n) as varchar(n)
;;   etc...
;; * CONSTRAINT should be a symbol? (draft)
(define-record-type db-column
  (fields name type constraint))

(define* (create-db-column name type #:optional (constraint #f))
  (make-db-column name type constraint))

(define (db-column:dump column)
  (let ((name (db-column-name column))
        (type (db-column-type column))
        (constraint (db-column-constraint column)))
    (if constraint
        (-> name type << constraint)
        (-> name type))))
