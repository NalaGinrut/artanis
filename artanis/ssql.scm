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
  #:export (sql))

(define-syntax-rule (-> fmt args ...)
  (format #f fmt args ...))

(define-syntax sql-select
  (syntax-rules (* where from)
    ((_ * from table)
     (-> "select * from ~a" table))
    ((_ fields from table)
     (-> "select ~{~s~^,~} from ~a" fields table))
    ((_ fields from table where conds)
     (string-append (sql-select fields from table) " where " conds))))

(define-syntax sql-insert
  (syntax-rules (into values)
    ((_ into table values lst)
     (-> "insert into ~a values (~{~s~^,~})" table lst))
    ((_ into table fields values lst)
     (-> "insert into ~a (~{~a~^,~}) values (~{~s~^,~})" table fields lst))))

(define-syntax sql
  (syntax-rules (select insert alter create)
    ((_ select rest ...)
     (sql-select rest ...))
    ((_ insert rest ...)
     (sql-insert rest ...))
    ((_ alter rest ...)
     (sql-alter rest ...))
    ((_ create rest ...)
     (sql-create rest ...))))