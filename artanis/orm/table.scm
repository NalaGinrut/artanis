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

(define-module (artanis orm table)
  #:use-module (artanis utils)
  #:use-module (artanis db)
  #:use-module (oop goops)
  #:export (<db-table>))

(define-class <db-table> (<db>)
  (name #:init-keyword #:name #:accessor db-table:name)
  (columns #:init-thunk new-stack #:accessor db-table:columns))

(define-method (create-table (self <db-table>) (name <string>))
  (make <db-table> #:name name))

;;-------------adding columns--------------------
(define* (%add-column! table name type #:optional (constraint #f))
  (stack-push! (db-table:columns table) (list name type constraint))
  table)

(define-method (table:add-column (self <db-table>) (name <symbol>) (type <symbol>))
  (%add-column! self name type))

(define-method (table:add-column (self <db-table>) (name <symbol>) (type <symbol>) (constraint <string>))
  (%add-column! self name type constraint))

(define-method (table:add-column (self <db-table>) (columns <list>))
  (for-each %add-column! columns)
  self)

;;-------------removing columns------------------
(define* (%remove-column! table name)
  (stack-remove! (db-table:column table) name))

(define-method (table:remove-column (self <db-table>) (name <symbol>))
  (%remove-column! self name))

;;-------------dumping table---------------------
(define-method (table:dump (self (<db-table>)))
  (let ((name (db-table:name self))
        (columns (for-each db-column:dump (db-table:columns self))))
  (format #f
          "create table \"~a\" (~{~a~^,~});"
          name columns)))
