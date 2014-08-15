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
  #:export (->sql
            where))

(define (->string obj) (if (string? obj) obj (object->string obj)))

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
   ((op2 a1 a2) (->op2 op2 a1 (->string a2)))
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
    ((_ lst ...)
     (-> "~a" (->cond lst ...)))))

;; Examples:
;; NOTE: All the values will be casted to string according to SQL convention.
;; 1-1. Normal `where' syntax
;;    (->sql select * from 'user where '(= a 1))
;;    => "select * from user where a=\"1\";"
;; 1-2. Powerful `where' generator
;;    (->sql select * from 'user (where #:a 1))
;;    => "select * from user where a=\"1\" ;"
(define-syntax sql-select
  (syntax-rules (where from distinct order by group having as)
    ((_ field from table)
     (-> "~a from ~a" 'field table))
    ((_ (fields ...) from table)
     (-> "~{~a~^,~} from ~a" '(fields ...) table))
    ((_ (fields ...) name from table)
     (-> "~{~a~^,~} ~a from ~a" '(fields ...) 'name table))
    ((_ (fields ...) as name from table)
     (-> "~{~a~^,~} as ~a from ~a" '(fields ...) 'name table))
    ((_ fields from table where rest ...)
     (->where (sql-select fields from table) rest ...))
    ((_ fields from table cond-str)
     (string-concatenate (list (sql-select fields from table) cond-str)))
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
     (-> "~a set ~{~a~^,~}" table (->lst pairs)))
    ((_ table set pairs cond-str)
     (string-concatenate (list (sql-update table set pairs) cond-str)))
    ((_ table set pairs where rest ...)
     (->where end (sql-update table set pairs) (sql-where rest ...)))))

(define-syntax sql-delete
  (syntax-rules (* from where)
    ((_ * from table)
     (-> "* from ~a" table))
    ((_ * from table cond-str)
     (-> "* from ~a ~a" table cond-str))
    ((_ from table)
     (-> "from ~a" table))
    ((_ from table cond-str)
     (-> "from ~a ~a" table cond-str))
    ((_ from table where rest ...)
     (->where end (sql-delete from table) (sql-where rest ...)))))

(define-syntax sql-create
  (syntax-rules (table as select index unique on if exists not)
    ;; NOTE: don't use it directly, please take advantage of fprm.
    ;; (->sql create table 'mmr '("name varchar(10)" "age int(5)"))
    ((_ table name columns)
     (-> end "create table ~a (~{~a~^,~})" name columns))
    ((_ table if exists name columns)
     (-> end "create table if exists ~a (~{~a~^,~})" name columns))
    ((_ table if not exists name columns)
     (-> end "create table if not exists ~a (~{~a~^,~})" name columns))
    ;;(->sql create view 'mmr select '(a b) from 'tmp where "a=1 and b=2")
    ((_ view name as select rest ...)
     (-> end "create view ~a as select ~a" (sql-select rest ...)))
    ;; (->sql create index 'PersonID on 'Persons '(PersonID))
    ((_ index iname on tname columns)
     (-> end "create index ~a on ~a(~{~a~^,~})" iname tname columns))
    ((_ unique index iname on tname columns)
     (-> end "create unique index ~a on ~a(~{~a~^,~})" iname tname columns))))

(define-syntax sql-alter
  (syntax-rules (table rename to add modify drop column as select
                 primary key)
    ((_ table old-name rename to new-name)
     (-> "table ~a rename to ~a" old-name new-name))
    ((_ table name add cname ctype) 
     ;; e.g: (->sql alter table 'mmr add 'cname 'varchar(50))
     (-> "table ~a add ~a ~a" cname ctype))
    ((_ table name mofify pairs)
     (-> "table ~a modify (~{~a~^,~})" name (->lst pairs)))
    ((_ table name drop column cname)
     (-> "table ~a drop column ~a" name cname))
    ((_ table name add primary key keys)
     (-> "table ~a add primary key (~{~a~^,~})" name keys))
    ((_ table name drop primary key)
     (-> "table ~a drop primary key" name))
    ((_ table name rename column old-name to new-name)
     (-> "table ~a rename column ~a to ~a" name old-name new-name))))

(define-syntax sql-drop
  (syntax-rules (table if exists not)
    ((_ table name)
     (-> "table ~a" name))
    ((_ table if exists name)
     (-> "table if exists ~a" name))
    ((_ table if not exists name)
     (-> "table if not exists ~a" name))))

(define-syntax ->sql
  (syntax-rules (select insert alter create update delete use drop)
    ((_ select rest ...)
     (->end 'select (sql-select rest ...)))
    ((_ insert rest ...)
     (->end 'insert (sql-insert rest ...)))
    ((_ create rest ...)
     (sql-create rest ...))
    ((_ alter rest ...)
     (->end 'alter (sql-alter rest ...)))
    ((_ update rest ...)
     (->end 'update (sql-update rest ...)))
    ((_ delete rest ...)
     (->end 'delete (sql-delete rest ...)))
    ((_ truncate table name)
     (->end "truncate table ~a" name))
    ((_ drop rest ...)
     (->end 'drop (sql-drop rest ...)))
    ((_ use db)
     (-> end "use ~a" db))))

;; 'where' is used to generate condition string of SQL
;; There're several modes in it, and can be composited.
;; FIXME: Have to checkout sql-injection in the field value, especially '--'
(define (where . conds)
  (define (->range lst)
    (match lst
      (((? integer? from) (? integer? to))
       (map ->string lst))
      (else (throw 'artanis-err 500 "[SQL] where: invalid range" lst))))
  (define (get-the-conds-str key val)
    (let ((k (symbol->string key))
          (v (if (list? val) (->range val) (->string val))))
      (case key
        ;; These are reversed key in SQL, we use string-concatenate for efficiency
        ((limit)
         ;; FIXME: maybe we need such a check
         ;; (artanis-check-if-current-DB-support key)
         (if (list? v)
             (format #f " ~a ~{~a~^, ~} " k v)
             (string-concatenate (list " " k " " v " ")))) 
        (else (string-concatenate (list " " k "=\"" v "\" "))))))
  (match conds
    (() "")
    ;; If the only arg is string, return it directly
    ((? string? conds) (string-concatenate (list " where" conds)))
    ;; string template mode
    ;; e.g: (where "name = ${name}" #:name nala) ==> "name = \"nala\""
    (((? string? stpl) . vals)
     (apply (make-db-string-template (string-concatenate (list " where" stpl))) vals))
    ;; AND mode:
    ;; (where #:name 'John #:age 15 #:email "john@artanis.com") 
    ;; ==> name="John" and age="15" and email="john@artanis.com"
    (((? keyword? key) (? non-list? val) . rest)
     (let* ((k (keyword->symbol key))
            (str (get-the-conds-str k val)))
       (string-concatenate `(" where",str ,(if (null? rest) "" " and ") ,(apply where rest)))))
    ;; OR mode:
    ;; (where #:name '(John Tom Jim)) ==> name="John" or name="Tom" or name="Jim"
    ;; TODO:
    ;; (where (pick #:name 'John #:age 15)) ==> name="John" or age="15"
    ;; Complex rules could be done with string templation.
    (((? keyword? key) (vals ...) . rest)
     (let ((fmt (string-concatenate `(" where " "~{" ,(keyword->string key) "=\"~a\"~^ or ~}"))))
       (format #f fmt vals)))
    (else (throw 'artanis-err 500 "[SQL] where: invalid condition pattern" conds))))
