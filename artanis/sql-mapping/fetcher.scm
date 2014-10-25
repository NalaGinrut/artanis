;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014
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

(define-module (artanis sql-mapping fetcher)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (sql-mapping-simple-fetch
            sql-mapping-fetch-from-path
            sql-mapping-tpl-add!))

(define-record-type <sql-mapping> (fields type name path sm))

(define sm-ref hash-ref)
(define sm-set! hash-set!)
(define-syntax-rule (sql-mapping-ref name)
  (and=> (sm-ref *sql-mapping-lookup-table* name) <sql-mapping>-sm))

(define (sql-mapping-simple-fetch rc name)
  (let ((conn (DB-open rc))
        (sm (sql-mapping-ref name)))
    (lambda kargs
      (DB-query conn (apply sm kargs))
      conn)))

(define (sql-mapping-fetch-from-path rc path name)
  (define (fetch-from-path p)
    #t)
  (let ((conn (DB-open rc))
        (sm (fetch-from-path path)))
    #t))

(define (sql-mapping-tpl-add! rc name tpl)
  #t)
