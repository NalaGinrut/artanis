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

(define-module (artanis ssql)
  #:use-module (artanis utils)
  #:use-module (ice-9 match)
  #:export (->sql))

;; ENHANCE: 
;; 1. conds should be expanded from s-expr
;; e.g (select * from 'table where "name='nala' and age=15")
;; expect: (select * from 'table where '(and (name "nale") (age 15)))
;; 
;; 2. order-by should be expanded from s-expr
;; e.g: (select * from 'table order by "column asc")
;; expect: (select * from 'table order by '(column asc))
;; above all accept only strings at present, it's not so elegant.
 
(define-syntax ->
  (syntax-rules (end)
    ((_ end fmt args ...)
     (format #f "~@?;" fmt args ...))
    ((_ fmt args ...)
     (format #f fmt args ...))))

(define-syntax-rule (->end name arg)
  (-> end "~a ~a" name arg))

(define-syntax ->where
  (syntax-rules (end)
    ((_ end sentence rest ...)
     (-> end "~a where ~a" sentence (sql-where rest ...)))
    ((_ sentence rest ...)
     (-> "~a where ~a" sentence (sql-where rest ...)))))

(define-syntax-rule (->lst pairs)
  (map (lambda (l) (-> "~{~a~^=~s~}" l)) pairs))

(define (->cond lst)
  (define (->logical and/or ll)
    (string-join (map ->cond ll) (format #f " ~a " and/or)))
  (define (->op2 op a1 a2) (-> "~a~a~s" a1 op a2))
  (define (->opn opn . ll) (-> "~a ~{~s~^ ~}" opn ll))
  (match lst
   (() "")
   (('and ll ...) (->logical 'and ll))
   (('or ll ...) (->logical 'or ll))
   ((op2 a1 a2) (->op2 op2 a1 a2))
   ((opn ll ...) (->opn opn ll))
   (((l1 ...) ll ...) (map ->cond (cons l1 ll)))
   (else (throw 'artanis-err 500 "invalid sql syntax!" lst))))

(define-syntax sql-where
  (syntax-rules (select in like between and is null limit)
    ((_ limit n)
     (-> "limit ~a" n))
    ((_ column in (lst ...))
     (-> "~a in (~{~a~^,~})" 'column '(lst ...)))
    ((_ column in (select rest ...))
     (-> "~a in (~{~a~^,~}) in (select ~a)" 'column (sql-select rest ...)))
    ((_ column like pattern)
     (-> "~a like ~s" 'column pattern))
    ((_ column between a and b)
     (-> "~a between ~s and ~s" 'column a b))
    ((_ column is null)
     (-> "~a is null" 'column))
    ;; e.g.   (->sql select * from 'user where (and (= user "name") (> age 15)))
    ;;        ==>  "select * from user where user=\"name\" and age>15;"
    ((_ (lst ...))
     (-> "~a" (->cond `(,lst ...))))))

(define-syntax sql-select
  (syntax-rules (* where from distinct order by group having as)
    ((_ * from table)
     (-> "* from ~a" table))
    ((_ (fields ...) from table)
     (-> "~{~s~^,~} from ~a" '(fields ...) 'table))
    ((_ (fields ...) name from table)
     (-> "~{~s~^,~} ~a from ~a" '(fields ...) 'name 'table))
    ((_ (fields ...) as name from table)
     (-> "~{~s~^,~} as ~a from ~a" '(fields ...) 'name 'table))
    ((_ fields from table where rest ...)
     (->where (sql-select fields from table) rest ...))
    ((_ distinct rest ...)
     (-> "distinct ~a" (sql-select rest ...)))
    ((_ rest ... group by groups)
     (let ((gg (-> "~{~a~^,~}" groups)))
       (-> "~a group by ~a" (sql-select rest ...) gg)))
    ((_ rest ... order by orders)
     (-> "~a order by ~a" (sql-select rest ...) orders))
    ((_ rest ... having what)
     (-> "~a having ~a" (sql-select rest ...) what))))

(define-syntax sql-insert
  (syntax-rules (into values select)
    ((_ into table)
     (-> "into ~a" 'table))
    ((_ into table select rest ...)
     (-> "into ~a select ~a" 'table (sql-select rest ...)))
    ((_ into table values (lst ...))
     (-> "into ~a values (~{~s~^,~})" 'table '(lst ...)))
    ((_ into table values (lst ...) select rest ...)
     (-> "into ~a values (~{~s~^,~}) select ~a"
         'table '(lst ...) (sql-select rest ...)))
    ((_ into table fields values lst)
     (-> "into ~a (~{~a~^,~}) values (~{~s~^,~})" 'table fields lst))
    ((_ into table fields values lst select rest ...)
     (-> "into ~a (~{~a~^,~}) values (~{~s~^,~}) select ~a" 
         'table fields lst (sql-select rest ...)))))

(define-syntax sql-update
  (syntax-rules (set where)
    ((_ table set pairs)
     (-> end "update ~a set ~{~a^,~}" table (->lst pairs)))
    ((_ table set pairs where rest ...)
     (->where end (sql-update table set pairs) (sql-where rest ...)))))

(define-syntax sql-delete
  (syntax-rules (* from where)
    ((_ * from table)
     (-> end "delete * from ~a" table))
    ((_ from table)
     (-> end "delete from ~a" table))
    ((_ from table where rest ...)
     (->where end (sql-delete from table) (sql-where rest ...)))))

(define-syntax sql-create
  (syntax-rules (table as select)
    ;; NOTE: don't use it directly, please take advantage of (orm column)
    ;;(->sql create table tablename '("\"name\" \"varchar(10)\"" "\"age\" \"int\""))
    ((_ table name columns)
     (-> end "create table ~a (~{~a~^,~})" name columns))
    ;;(->sql create view 'mmr select '(a b) from 'tmp where "a=1 and b=2")
    ((_ view name as select rest ...)
     (-> end "create view ~a as select ~a" (sql-select rest ...)))))

(define-syntax sql-alter
  (syntax-rules (table rename to add modify drop column as select)
    ((_ table old-name rename to new-name)
     (-> end "alter table ~a rename to ~a" old-name new-name))
    ((_ table name add pairs) 
     ;; e.g: (->sql alter table 'mmr add '((cname "varchar(50)")))
     (-> end "alter table ~a add (~{~a~^,~})" name (->lst pairs)))
    ((_ table name mofify pairs)
     (-> end "alter table ~a modify (~{~a~^,~})" name (->lst pairs)))
    ((_ table name drop column cname)
     (-> end "alter table ~a drop column ~a" name cname))
    ((_ table name rename column old-name to new-name)
     (-> end "alter table ~a rename column ~a to ~a" name old-name new-name))))

(define-syntax ->sql
  (syntax-rules (select insert alter create update delete use)
    ((_ select rest ...)
     (->end 'select (sql-select rest ...)))
    ((_ insert rest ...)
     (->end 'insert (sql-insert rest ...)))
    ((_ create rest ...)
     (sql-create rest ...))
    ((_ alter rest ...)
     (sql-alter rest ...))
    ((_ update rest ...)
     (sql-update rest ...))
    ((_ delete rest ...)
     (sql-delete rest ...))
    ((_ truncate table name)
     (->end "truncate table ~a" name))
    ((_ drop table name)
     (->end "drop table ~a" name))
    ((_ use db)
     (-> end "use ~a" db))))
