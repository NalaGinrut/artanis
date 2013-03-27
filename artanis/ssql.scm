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

(define-module (artanis ssql)
  #:use-module (artanis utils)
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
    ((_ end sentence conds)
     (-> end "~a where ~a" sentence conds))
    ((_ sentence conds)
     (-> "~a where ~a" sentence conds))))

(define-syntax sql-select
  (syntax-rules (* where from distinct order by group having)
    ((_ * from table)
     (-> "* from ~a" table))
    ((_ fields from table)
     (-> "~{~s~^,~} from ~a" fields table))
    ((_ fields from table where conds)
     (->where (sql-select fields from table) conds))
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
     (-> "into ~a" table))
    ((_ into table select rest ...)
     (-> "into ~a select ~a" table (sql-select rest ...)))
    ((_ into table values lst)
     (-> "into ~a values (~{~s~^,~})" table lst))
    ((_ into table values lst select rest ...)
     (-> "into ~a values (~{~s~^,~}) select ~a"
         table lst (sql-select rest ...)))
    ((_ into table fields values lst)
     (-> "into ~a (~{~a~^,~}) values (~{~s~^,~})" table fields lst))
    ((_ into table fields values lst select rest ...)
     (-> "into ~a (~{~a~^,~}) values (~{~s~^,~}) select ~a" 
         table fields lst (sql-select rest ...)))))

(define-syntax sql-update
  (syntax-rules (set where)
    ((_ table set pairs)
     (let ((pp (map (lambda (l) (-> "~{~a~^=~s~}" l)) pairs)))
       (-> end "update ~a set ~{~a^,~}" table pp)))
    ((_ table set pairs where conds)
     (->where end (sql-update table set pairs) conds))))

(define-syntax sql-delete
  (syntax-rules (* from where)
    ((_ * from table)
     (-> end "delete * from ~a" table))
    ((_ from table)
     (-> end "delete from ~a" table))
    ((_ from table where conds)
     (->where end (sql-delete from table) conds))))
    
(define-syntax ->sql
  (syntax-rules (select insert alter create update delete)
    ((_ select rest ...)
     (->end 'select (sql-select rest ...)))
    ((_ insert rest ...)
     (->end 'insert (sql-insert rest ...)))
    ((_ alter rest ...)
     (sql-alter rest ...))
    ((_ create rest ...)
     (sql-create rest ...))
    ((_ update rest ...)
     (sql-update rest ...))
    ((_ delete rest ...)
     (sql-delete rest ...))))
