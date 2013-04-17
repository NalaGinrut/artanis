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

(define (->conditon . conds)   
  (receive (a b) (partition keyword? conds)
    (map (lambda (x y)
           (format #f "~a ~a ~s" (keyword->symbol x) (car y) (cadr y)))
         a b)))

(define (->fvs . kvs)
  (receive (fields vals) (partition keyword? kvs)
    (values (map keyword->symbol fields) vals)))

(define (new-table db name . args)
  (let* ((vars (receive (a b) (partition keyword? args) (map cons a b)))
         (ops (new-ops))
         (fprm (make-fprm db name vars ops #f)))
    (lambda (pattern)
      (match pattern
        ((#:get (fields ...) conds ...) (table-ref fprm fields (apply ->condition conds)))
        ((#:get-all conds ...) (table-ref fprm #f (apply ->condition conds)))
        ((#:delete conds ...) (table-delete fprm (apply ->condition conds)))
        ((#:insert kvs ...) (receive (fields vals) (->fvs kvs) (table-insert fprm fields vals)))
        ((#:dump-cache) (new-query-cache fprm))
        ;; TODO: finish others
        ))))

(define (table-insert fprm fields vals)
  (define db (fprm-db fprm))
  (define name (fprm-name fprm))
  (define sql
    (->sql insert into fields values vals))
  (query db sql)
  (receive (status reason) (check-status db)
    (if (zero? status)
        #t ; insert sucessfully
        (throw 'artanis-err 500 reason sql))))  

(define (table-delete fprm . condition)
  (define db (fprm-db fprm))
  (define name (fprm-name fprm))
  (define sql 
    (if (null? condition)
        (->sql delete * from name) ; delete all rows
        (->sql delete from name where condition))) ; delete rows as the condition
  (query db sql)
  (receive (status reason) (check-status db)
    (if (zero? status)
        #t ; delete sucessfully
        (throw 'artanis-err 500 reason sql))))

(define (table-ref fprm fields . condition)
  (define db (fprm-db fprm))
  (define name (fprm-name fprm))
  (define sql 
    (cond 
     ((not fields)
      (if (null? condition)
          (->sql select * from name)
          (->sql select * from name where condition)))
     (else
      (if (null? condition)
          (->sql select fields from name)
          (->sql select fields from name where condition)))))
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
