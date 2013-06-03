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

(define-module (artanis fprm)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis ssql)
  #:use-module (artanis db)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 match)
  #:export (new-table))

(define-record-type fprm
  (make-fprm db name vars ops update)
  fprm?
  (dbi fprm-db fprm-db!) ; db object
  (name fprm-name fprm-name!) ; table name
  (vars fprm-vars fprm-vars!) ; vars search table
  (cache fprm-cache fprm-cache!) ; fprm operations
  (update fprm-update fprm-update!)) ; is updated?

(define-record-type query-cache
  (make-query-cache dbi sql)
  query-cache?
  (dbi query-cache-db)
  ;; no need to modify dbi, each cache only used by one db (recommended)
  (sql query-cache-sql query-cache-sql!))

(define (make-inner-sql op table fields where having group-by order-by as distinct)
  (call-with-output-string
   (lambda (port)
     (format port "~{~a~^ ~} ~a " op fields)
     (and as (format port "as ~a " as)
     (format port "from ~a " table)
     (and where (format port "where ~a " where))
     (and having (format port "having ~a " having))
     (and group-by (format port "group by ~a " group-by))
     (and order-by (format port "order by ~a " order-by))
     (and distinct (format port "distinct ~a " distinct))
     (display ";")))))

(define* (table-insert fprm #:key (find '*) (where #f)) 
  (define db (fprm-db fprm))
  (define name (fprm-name fprm))
  (define sql (make-inner-sql '(insert into) name find 
  (query db sql)
  (receive (status reason) (check-status db)
    (if (zero? status)
        #t ; insert sucessfully
        (throw 'artanis-err 500 reason sql))))  

(define* (table-delete fprm #:key (find '*) (where #f) (having #f)
                       (group-by #f) 
  (define db (fprm-db fprm))
  (define name (fprm-name fprm))
  (define sql (make-inner-sql '(delete) name find where having
                              #f #f #f #f))
  (query db sql)
  (receive (status reason) (check-status db)
    (if (zero? status)
        #t ; delete sucessfully
        (throw 'artanis-err 500 reason sql))))

(define* (table-ref fprm #:key (find '*) (where #f) (having #f)
                    (group-by #f) (order-by #f) (as #f) (distinct #f))
  (define db (fprm-db fprm))
  (define name (fprm-name fprm))
  (define sql (make-inner-sql '(select) name find where having 
                              group-by order-by as distinct))
  (query db sql)
  (receive (status reason) (check-status db)
    (if (zero? status)
        (get-all-rows db)
        (throw 'artanis-err 500 reason sql))))

;; use query-cache to make a sql cache, and use it directly to avoid ssql->sql
(define (new-query-cache table)
  (let ((sql (fprm-cache table))
        (db (fprm-db table)))
    (make-query-cache db sql)))

(define (update-query-cache qc sql)
  (query-cache-sql! qc sql)
  qc)
