;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014
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

(define-module (artanis orm table)
  #:use-module (artanis utils)
  #:use-module (artanis db)
  #:use-module (artanis ssql)
  #:use-module (oop goops)
  #:export (<db-table> create-table 
            table:dirty-set! 
            table:dirty-clear! 
            table:cache-add! 
            table:cache-clear! 
            table:cache-set!
            table:new-column-add! 
            table:new-column-remove!
            table:new-column-clear! 
            table:columns-clear! 
            table:drop!
            table:column-drop! 
            table:create
            table:async!
            table:column-get-all
            table:column-get-first
            table:column-get
            table:result-fetch!
            table:dump 
            table:dump-result
            table:column-set!
            table:column-remove!))

(define-class <db-table> ()
  (name #:init-keyword #:name #:accessor db-table:name)
  (db #:init-keyword #:db #:accessor db-table:db)
  ;; NOTE: this new-columns only for adding new columns, not for query
  (new-columns #:init-thunk new-stack #:accessor db-table:new-columns)
  ;; NOTE: ref-columns used for quering columns from table
  (ref-columns #:init-thunk '() #:accessor db-table:ref-columns)
  ;; We use cache to hold the compiled SQL string, to avoid compile SQL each time.
  ;; It's necessary for users to update this cache each time they modified the attributes of table.
  ;; table:sync! is a good tool for that.
  (cache #:init-value "" #:accessor db-table:cache)
  ;; each time the table is modified, the dirty flag will be set
  (dirty #:init-value #f)
  ;; the parsed result from DBI
  (result #:init-value #f)
  ;; the mutex 
  ;; NOTE: make the methods thread safe,  it'd be caller's duty.
  (mutex #:init-thunk make-mutex #:accessor db-table:mutex))

(define-method (create-table (self <db-table>) (name <string>))
  (make <db-table> #:name name))

(define-method (table:dirty-set! (self <db-table>))
  (or (slot-ref self 'dirty) (slot-set! self 'dirty #t))) ; set the dirty to true 

(define-method (table:dirty-clear! (self <db-table>))
  (and (slot-ref self 'dirty) (slot-set! self 'dirty #f)))

;; concatenate sql string in cache
(define-method (table:cache-add! (self <db-table>) (sql <string>))
  (let ((cache (db-table:cache self)))
    (set! (string-concatenate
           (list (or (and (string? cache) cache) "") sql)))))

(define-method (table:cache-clear! (self <db-table>))
  (set! (db-table:cache self) ""))

;; set cached sql string directly, sometimes you may need it
(define-method (table:cache-set! (self <db-table>) (sql <string>))
  (set! (db-table:cache self) sql))

;;-------------add new columns--------------------
;; NOTE: these methods only for adding new columns
(define* (%add-column! table name type #:optional (constraint #f))
  (stack-push! (db-table:new-columns table) (create-db-column name type constraint))
  (table:dirty-set! table) ; it's dirty!
  table)

(define-method (table:new-column-add! (self <db-table>) (name <symbol>) (type <symbol>))
  (%add-column! self name type))

(define-method (table:new-column-add! (self <db-table>) (name <symbol>) (type <symbol>) (constraint <string>))
  (%add-column! self name type constraint))

;; columns MUST be '((name (varchar 10)) (age (int 3))) or similar
(define-method (table:new-column-add! (self <db-table>) (columns <list>))
  (for-each %add-column! columns)
  self)

;;-------------remove columns------------------
;; NOTE: remove is not DROP in SQL!!! It's just remove the element from column lists in <db-table>
;; NOTE: we don't set dirty flag here, because we don't use 1this function during SQL generation on the fly.
;;       If you really need it on the fly, which means you understand this ORM totally wrong!
;;       Maybe you need table:column-drop! or table:drop! actually!
(define* (%remove-new-column! table name)
  (stack-remove! (db-table:new-columns table) name))

(define-method (table:new-column-remove! (self <db-table>) (name <symbol>))
  (%remove-column! self name))

(define-method (table:new-column-clear! (self <db-table>))
  (set! (db-table:new-columns self) '()))

;;-------------drop table------------------
(define-method (table:drop! (self <db-table>))
  (table:cache-add! self (->sql drop table (db-table:name self)))
  (table:dirty-set! self)
  (table:cache self))

(define-method (table:column-drop! (self <db-table>) (name <symbol>))
  (let ((sql (->sql alter table (db-table:name self) drop column name)))
    (table:cache-add! self sql)
    (table:dirty-set! self)
    (table:cache self)))

;;-------------dump table-----------------
;; run sql with the dbi
(define-method (table:result-fetch! (self <db-table>))
  (let ((db (db-table:db self)))
    (slot-set! self 'result (get-all-rows db))))

(define-method (table:dump (self <db-table>))
  (let* ((db (db-table:db self))
         (sql (db-table:cache self))
         (status (get-status db)))
    (arm:log "~a" (cdr (status))) ; print status message
    (case (status->symbol status)
      ((ok)
       (orm:log "sql: ~a" sql) ; for DEBUG
       (table:result-fetch! self))
      ;; TODO: finish the rest status check
      (else (throw 'artanis-err 500 "[ORM] DB has fatal error!" sql)))))

(define-method (table:dump-result (self <db-table>))
  ;; TODO: should parse then wrap to some objects, rather than pure assoc-list
  (slot-ref self 'result))    

;;-------------get column------------------
;; NOTE: This function should be run-at-once, which means there's no other succeed sql in the cache.
;;       Or it doesn't make sense.
(define* (%column-get tb name #:optional (num #f))
  (define sql
    (if num
        (->sql select * from name limit num)
        (->sql select * from name)))
  (table:cache-add! tb sql)
  (table:dump tb)
  (table:dump-result tb))  

(define-method (table:column-get-all (self <db-table>) (name <symbol>))
  (%column-get self name))

(define-method (table:column-get-first (self <db-table>) (name <symbol>))
  (%column-get self name 1))

;; The same with get-all
(define-method (table:column-get (self <db-table>) (name <symbol>))
  (%column-get self name))

(define-method (table:column-get (self <db-table>) (name <symbol>) (num <integer>))
  (%column-get self name num))

;; set column name & value
(define-method (table:column-set! (self <db-table>) (name <symbol>) (value <top>))
  (assoc-set! (db-table:ref-columns self) name value)
  (table:dirty-set! self)
  value)

;; clear ref-columns
(define-method (table:columns-clear! (self <db-table>))
  (set! (db-table:ref-columns self) '()))

;; remove a column from the query request
(define-method (table:column-remove! (self <db-table>) (name <symbol>))
  (assoc-remove! (db-table:ref-columns self) name))

;;-------------create table---------------------
(define-method (table:create (self <db-table>))
  (let* ((name (db-table:name self))
         (columns (for-each db-column:dump (db-table:new-columns self)))
         (sql (->sql create table name columns)))
    ;; reasonably, create should be the first operation, if no, it's illlogic
    (table:cache-set! self sql)
    (table:dirty-set! self)
    sql))

;;-------------cache table---------------------
(define-method (table:async! (self <db-table>))
  (table:cache-set! self (table:dump self))
  (table:dirty-clear! self) ; clear the dirty flag
  self)
