;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013-2026
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
;;  Artanis is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License and GNU
;;  Lesser General Public License published by the Free Software
;;  Foundation, either version 3 of the License, or (at your option)
;;  any later version.

;;  Artanis is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License and GNU Lesser General Public License
;;  for more details.

;;  You should have received a copy of the GNU General Public License
;;  and GNU Lesser General Public License along with this program.
;;  If not, see <http://www.gnu.org/licenses/>.

(define-module (artanis ssql)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis irregex)
  #:use-module (artanis logger)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 format)
  #:use-module ((rnrs) #:select (define-record-type))
  #:use-module ((srfi srfi-1) #:select (map-in-order filter-map))
  #:export (->sql
            where
            having
            /in
            /or
            /and
            /exists
            /literal
            /id
            letq
            use-params?
            current-param-index
            current-param-list
            add-param!
            value-detect
            sql-int sql-text sql-bool sql-numeric sql-double sql-real
            sql-bigint sql-smallint sql-varchar sql-char sql-timestamp
            sql-date sql-time sql-bytea sql-json sql-jsonb sql-uuid
            sql-inet sql-cidr sql-macaddr sql-point sql-array))

(define (->string obj) (if (string? obj) obj (object->string obj)))

(define-record-type sql-id (fields name))

;; Get current database type
(define (current-dbd)
  (get-conf '(db dbd)))

(define (fix-value v)
  (match v
    ((? sql-id? id) (sql-id-name id))
    ((? number? num) num)
    ((? sql-type? t) (sql-type-value t))
    (else (format #f "'~a'" v))))

(define-syntax-rule (fix-values lst)
  (map fix-value lst))

;; SQL type record for explicit type annotations
(define-record-type sql-type
  (fields value type))

(define-record-type sql-array (parent sql-type))

;; Constructors for common SQL types
(define (sql-int val) (make-sql-type val "::INT"))
(define (sql-text val) (make-sql-type val "::TEXT"))
(define (sql-bool val) (make-sql-type val "::BOOLEAN"))
(define (sql-numeric val) (make-sql-type val "::NUMERIC"))
(define (sql-double val) (make-sql-type val "::DOUBLE PRECISION"))
(define (sql-real val) (make-sql-type val "::REAL"))
(define (sql-bigint val) (make-sql-type val "::BIGINT"))
(define (sql-smallint val) (make-sql-type val "::SMALLINT"))
(define (sql-varchar val len)
  (make-sql-type val (format #f "::VARCHAR(~a)" len)))
(define (sql-char val len)
  (make-sql-type val (format #f "::CHAR(~a)" len)))
(define (sql-timestamp val) (make-sql-type val "::TIMESTAMP"))
(define (sql-date val) (make-sql-type val "::DATE"))
(define (sql-time val) (make-sql-type val "::TIME"))
(define (sql-bytea val) (make-sql-type val "::BYTEA"))
(define (sql-json val) (make-sql-type val "::JSON"))
(define (sql-jsonb val) (make-sql-type val "::JSONB"))
(define (sql-uuid val) (make-sql-type val "::UUID"))
(define (sql-inet val) (make-sql-type val "::INET"))
(define (sql-cidr val) (make-sql-type val "::CIDR"))
(define (sql-macaddr val) (make-sql-type val "::MACADDR"))
(define (sql-point val) (make-sql-type val "::POINT"))
(define (sql-array vals elem-type) (make-sql-array vals elem-type))

;; Generate placeholder based on database type and parameter index
(define (type-detect value)
  (cond
   ((and (eq? 'postgresql (current-dbd))
         (sql-type? value))
    (let ((type (sql-type-type value)))
      (if (sql-array? value)
          ""
          type)))
   (else "")))

(define (make-placeholder index value)
  (case (current-dbd)
    ((postgresql)
     (cond
      ((sql-array? value)
       (let lp ((vals (sql-type-value value)) (ret '()))
         (cond
          ((null? vals)
           (format #f "ARRAY[~{~a~^,~}]" (reverse ret)))
          (else
           (let* ((index (current-param-index))
                  (elem (format #f "$~a::~a" index (sql-type-type value))))
             (current-param-index (1+ index))
             (lp (cdr vals) (cons elem ret)))))))
      (else (format #f "$~a~a" index (type-detect value)))))
    ((mysql sqlite3)
     (when (sql-type? value)
       (artanis-warn
        "SQL type anno is only for postgresql, not ~a~%" (current-dbd)))
     "?")
    (else
     (throw 'artanis-err 500 make-placeholder
            "Unknown DBD `~a'" (current-dbd)))))

;; ENHANCE:
;; 1. conds should be expanded from s-expr
;; e.g (select * from 'table where "name='nala' and age=15")
;; expect: (select * from 'table where '(and (name "nale") (age 15)))
;;
;; 2. order-by should be expanded from s-expr
;; e.g: (select * from 'table order by "column asc")
;; expect: (select * from 'table order by '(column asc))
;; above all accept only strings at present, it's not so elegant.

(define drop-tail-semicolon? (make-parameter #f))
(define use-params? (make-parameter #t)) ; Always use params by default
(define current-param-index (make-parameter 1))
(define current-param-list (make-parameter (new-queue)))

;; Add a parameter value and return appropriate placeholder
(define (add-param! value)
  (define-syntax-rule (fix x)
    (if (sql-type? x) (sql-type-value x) x))
  (let* ((actual-value (if (sql-type? value) (sql-type-value value) value))
         (placeholder (make-placeholder (current-param-index) value))
         (index (current-param-index))
         (params (current-param-list)))
    (queue-in! params actual-value)
    (current-param-index (+ index 1))
    (fix placeholder)))

(define-syntax ->
  (syntax-rules (end)
    ((_ end fmt args ...)
     (let ((str (string-trim-both (format #f fmt args ...))))
       (if (not (drop-tail-semicolon?))
           (string-concatenate (list str ";"))
           str)))
    ((_ fmt args ...)
     (string-trim-both (format #f fmt args ...)))))

;; NOTE: According to SQL standard, we should always use single-quote.
(define *double-quote-re* (string->irregex "\""))
(define-syntax-rule (->end name arg)
  (irregex-replace/all *double-quote-re* (-> end "~a ~a" name arg) "'"))

(define (->engine . engine)
  (match engine
    ((or () (#f) ("")) "")
    (else (format #f "engine=~a" (car engine)))))

(define-syntax ->where
  (syntax-rules (end)
    ((_ end sentence rest ...)
     (-> end "~a where ~a" sentence (sql-where rest ...)))
    ((_ sentence rest ...)
     (-> "~a where ~a" sentence (sql-where rest ...)))))

(define (kv->bind-str kv-pairs)
  (define (kv->list lst)
    (let lp ((next lst) (ret '()))
      (match next
        (() (reverse ret))
        ((k v rest ...)
         (lp rest
             (cons (format #f "~a=~a"
                           (keyword->symbol k)
                           (if (use-params?)
                               (add-param! v)
                               (fix-value v)))
                   ret))))))
  (format #f "~{~a~^,~}" (kv->list kv-pairs)))

(define (->cond lst)
  (define (->logical and/or ll)
    (string-join (map ->cond ll) (format #f " ~a " and/or)))
  (define (->op2 op a1 a2)
    (if (use-params?)
        (format #f "~a~a~a" a1 op (add-param! a2))
        (-> "~a~a~s" a1 op a2)))
  (define (->opn opn . ll)
    (if (use-params?)
        (-> "~a ~{~a~^ ~}" opn (map-in-order add-param! ll))
        (-> "~a ~{'~a'~^ ~}" opn ll)))
  (match lst
    (() "")
    ((? string? str) (string-trim-both str))
    (('and ll ...) (->logical 'and ll))
    (('or ll ...) (->logical 'or ll))
    ((op2 a1 a2) (->op2 op2 a1 a2))
    ((opn ll ...) (apply ->opn opn ll))
    (((l1 ...) ll ...) (map ->cond (cons l1 ll)))
    (else (throw 'artanis-err 500 ->cond "invalid sql syntax!" lst))))


(define (->combine cols)
  (filter-map
   (lambda (x)
     (if (and (string? x) (string-null? x)) #f x))
   (apply append
          (map (lambda (x) (if (list? x) x (list x)))
               cols))))

(define-syntax sql-where
  (syntax-rules (select in like between and is null limit)
    ((_ limit n)
     (-> "limit ~a" n))
    ((_ column in (lst ...))
     (-> "~a in (~{~a~^,~})" 'column '(lst ...)))
    ((_ column in (select rest ...))
     (-> "~a in (~{~a~^,~}) in (select ~a)" 'column (sql-select rest ...)))
    ((_ column like pattern)
     (-> "~a like '~a'" 'column pattern))
    ((_ column between a and b)
     (-> "~a between '~a' and '~a'" 'column a b))
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
  (syntax-rules (* where from distinct order by group having as)
    ((_ * from table)
     (-> "* from ~a" table))
    ((_ field from table)
     (if (list? field)
         (-> "~{~a~^,~} from ~a" field table)
         (-> "~a from ~a" field table)))
    ((_ fields as name from table)
     (-> "~{~a~^,~} as ~a from ~a" fields 'name table))
    ((_ fields from table where rest ...)
     (->where (sql-select fields from table) rest ...))
    ((_ fields from table cond-str)
     (string-concatenate (list (sql-select fields from table) cond-str)))
    ((_ * from table cond-str)
     (string-concatenate (list (sql-select * from table) cond-str)))
    ((_ * from (select subselect ...) as alias-name)
     (-> "* from (select ~a) as ~a" (sql-select subselect ...) alias-name))
    ((_ field from (select subselect ...) as alias-name)
     (if (list? field)
         (-> "~{~a~^,~} from (select ~a) as ~a" field
             (sql-select subselect ...) alias-name)
         (-> "~a from (select ~a) as ~a" field
             (sql-select subselect ...) alias-name)))
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
     (if (use-params?)
         (-> "into ~a values (~{~a~^,~})"
             table (map-in-order add-param! lst))
         (-> "into ~a values (~{~a~^,~})" table (fix-values lst))))
    ((_ into table values lst select rest ...)
     (if (use-params?)
         (-> "into ~a values (~{~a~^,~}) select ~a"
             table (map-in-order add-param! lst)
             (sql-select rest ...))
         (-> "into ~a values (~{~a~^,~}) select ~a"
             table (fix-values lst) (sql-select rest ...))))
    ((_ into table fields values lst)
     (if (use-params?)
         (-> "into ~a (~{~a~^,~}) values (~{~a~^,~})"
             table fields (map-in-order add-param! lst))
         (-> "into ~a (~{~a~^,~}) values (~{~a~^,~})"
             table fields (fix-values lst))))
    ((_ into table fields values lst select rest ...)
     (if (use-params?)
         (-> "into ~a (~{~a~^,~}) values (~{~a~^,~}) select ~a"
             table fields
             (map-in-order add-param! lst)
             (sql-select rest ...))
         (-> "into ~a (~{~a~^,~}) values (~{~a~^,~}) select ~a"
             table fields (fix-values lst) (sql-select rest ...))))))

(define-syntax sql-update
  (syntax-rules (set where)
    ((_ table set (kv-pairs ...))
     (-> "~a set ~a" table (kv->bind-str (list kv-pairs ...))))
    ((_ table set (kv-pairs ...) cond-str)
     (-> "~a set ~a ~a"
         table (kv->bind-str (list kv-pairs ...))
         (string-trim-both cond-str)))))

(define-syntax sql-delete
  (syntax-rules (* from where)
    ((_ * from table)
     (-> "* from ~a" table))
    ((_ * from table cond-str)
     (-> "* from ~a ~a" table (string-trim-both cond-str)))
    ((_ from table)
     (-> "from ~a" table))
    ((_ from table cond-str)
     (-> "from ~a ~a" table (string-trim-both cond-str)))
    ((_ from table where rest ...)
     (->where end (sql-delete from table) rest ...))))

(define-syntax sql-create
  (syntax-rules (table view as select index unique on if exists not database)
    ;; NOTE: don't use it directly, please take advantage of fprm.
    ;; (->sql create table 'mmr ("name varchar(10)" "age int(5)"))
    ((_ table if exists name columns engine ...)
     (-> end "create table if exists ~a (~{~a~^,~}) ~a"
         name (->combine columns) (->engine engine ...)))
    ;; (->sql create table if not exists 'Persons '("name varchar(10)" "age int"))
    ((_ table if not exists name columns engine ...)
     (-> end "create table if not exists ~a (~{~a~^,~}) ~a"
         name (->combine columns) (->engine engine ...)))
    ((_ table name columns engine ...)
     (-> end "create table ~a (~{~a~^,~}) ~a"
         name (->combine columns) (->engine engine ...)))
    ;; (->sql create view 'mmr as select '(a b) from 'tmp where '(and (= a 1) (= b 2)))
    ;; or equivalent to
    ;; (->sql create view 'mmr as select '(a b) from 'tmp (where #:a "1" #:b "2"))
    ((_ view name as select rest ...)
     (-> end "create view ~a as select ~a" name (sql-select rest ...)))
    ((_ database db)
     (-> end "create database ~a" db))
    ((_ database if not exists db)
     (with-dbd
      'mysql
      (-> end "create database if not exists ~a" db)))
    ;; (->sql create index 'PersonID on 'Persons '(PersonID))
    ((_ index iname on tname columns engine ...)
     (-> end "create index ~a on ~a(~{~a~^,~}) ~a"
         iname tname (->combine columns) (->engine engine ...)))
    ((_ unique index iname on tname columns engine ...)
     (-> end "create unique index ~a on ~a(~{~a~^,~}) ~a"
         iname tname (->combine columns) (->engine engine ...)))))

(define-syntax sql-alter
  (syntax-rules (table rename to add modify drop column as select
                       primary key change index)
    ((_ table old-name rename to new-name)
     (-> "table ~a rename to ~a" old-name new-name))
    ((_ table name add cname ctype)
     ;; e.g: (->sql alter table 'mmr add 'cname "varchar(50)")
     (-> "table ~a add ~a ~a" name cname ctype))
    ((_ table name modify column cname ctype)
     (with-dbd
      'postgresql
      (-> "table ~a modify column ~a ~a" name cname ctype)))
    ((_ table name alter column cname ctype)
     (with-dbd
      'mysql
      (-> "table ~a alter column ~a ~a" name cname ctype)))
    ((_ table name modify (cl ...))
     (with-dbd
      'mysql
      (-> "table ~a modify column (~{~a~^,~})"
          name (map (lambda (c) (format #f "~{~a~^ ~}" c)) cl ...))))
    ((_ table name drop column cname)
     (-> "table ~a drop column ~a" name cname))
    ((_ table name add primary key keys)
     (-> "table ~a add primary key (~{~a~^,~})" name keys))
    ((_ table name drop primary key)
     (-> "table ~a drop primary key" name))
    ((_ table name change column old new type)
     (with-dbd
      'mysql
      (-> "table ~a change column ~a ~a ~a" name old new type)))
    ((_ table name rename column old new)
     (with-dbd
      '(postgresql sqlite3)
      (-> "table ~a rename column ~a to ~a" name old new)))
    ((_ table old-name rename new-name)
     (with-dbd
      'mysql
      (-> "table ~a rename ~a" old-name new-name)))
    ((_ table old-name rename to new-name)
     (with-dbd
      '(postgresql sqlite3)
      (-> "table ~a rename to ~a" old-name new-name)))
    ((_ table rename old-name to new-name)
     (-> "table rename ~a to ~a" old-name new-name))
    ((_ table tname index old rename to new)
     (with-dbd
      'postgresql
      (-> "table tname index ~a rename to ~a" tname old new)))
    ((_ table tname rename index old to new)
     (with-dbd
      'mysql
      (-> "table ~a rename index ~a to ~a" tname old new)))))

(define-syntax sql-drop
  (syntax-rules (table if exists not)
    ((_ table name)
     (-> "table ~a" name))
    ((_ table if exists name)
     (-> "table if exists ~a" name))
    ((_ table if not exists name)
     (-> "table if not exists ~a" name))))

;; 'where' is used to generate condition string of SQL
;; There're several modes in it, and can be composited.
;; FIXME: Have to checkout sql-injection in the field value, especially '--'
(define get-prefix (make-parameter " where "))
(define get-and/or (make-parameter " and "))
(define (where . conds) (apply gen-cond conds))
(define (having . conds)
  (parameterize ((get-prefix " having "))
    (apply gen-cond conds)))

(define *key/pred-re* (string->sre "([^<>=]+)(<|>|<=|>=|<>)"))
(define (gen-cond . conds)
  (define (->or/and kp)
    (let* ((len (string-length kp))
           (p (substring/shared kp (- len 2))))
      (match p
        ("<>" " and ")
        (else " or "))))
  (define (->key/pred k)
    (let* ((str (keyword->string k))
           (m (irregex-match *key/pred-re* str)))
      (if m str (string-append str "="))))
  (define (->range lst)
    (match lst
      (((? integer? from) (? integer? to))
       (map ->string lst))
      (else (throw 'artanis-err 500 gen-cond
                   (format #f "[SQL]~a: invalid range" (get-prefix))
                   lst))))
  (define (->quote v)
    (cond
     ((use-params?) (add-param! v))
     (else (fix-value v))))
  (define (get-the-conds-str key val)
    (let ((v (if (list? val) (->range val) val)))
      (match key
        ;; These are reversed key in SQL, we use string-concatenate for efficiency
        ('limit
         ;; FIXME: maybe we need such a check
         ;; (artanis-check-if-current-DB-support key)
         (let ((k (symbol->string key)))
           (if (list? v)
               (format #f " ~a ~{~a~^, ~} " k (->quote v))
               (format #f " ~a~a " k (->quote v)))))
        (else (format #f "~a~a" key (->quote v))))))
  (match conds
    (() "")
    (((? string? c1) (? string? c2) . rest)
     (string-concatenate (list c1 (get-and/or) c2 (apply gen-cond rest))))
    ;; If the only arg is string, return it directly
    (((? string? c)) (string-concatenate (list (get-prefix) c)))
    ;; string template mode
    ;; e.g: (where "name = ${name}" #:name nala) ==> "name = \"nala\""
    (((? string? stpl) (? keyword? k) . vals)
     (apply (make-db-string-template (string-concatenate (list (get-prefix) stpl))) (cons k vals)))
    ;; AND mode:
    ;; (where #:name "John" #:age 15 #:email "john@artanis.com")
    ;; ==> name="John" and age=15 and email="john@artanis.com"
    (((? keyword? key) (? non-list? val) . rest)
     (let* ((k (->key/pred key))
            (str (get-the-conds-str k val)))
       (string-concatenate
        `(,(get-prefix) ,str
          ,(if (null? rest) "" (get-and/or))
          ,(parameterize ((get-prefix "")) (apply gen-cond rest))))))
    ;; group mode:
    ;; (where #:name '("John" "Tom" "Jim")) ==> name="John" or name="Tom" or name="Jim"
    (((? keyword? key) (vals ...) . rest)
     (let* ((kp (->key/pred key))
            (fmt (string-concatenate `(,(get-prefix) "~{" ,kp "~a~^" ,(->or/and kp) "~}"))))
       (format #f fmt (map ->quote vals))))
    (else (throw 'artanis-err 500 gen-cond
                 (format #f "[SQL]~a: invalid condition pattern `~a'"
                         (get-prefix) conds)))))

;; Order of Precedence in SQL
;; It is important to understand how the database evaluates multiple comparisons in the WHERE clause.
;; All the AND comparisons (evaluated from Left to Right) are evaluated before the OR comparisons
;; (evaluated from Left to Right).

;; (where (/or #:name "John" #:age 15)) ==> " where  name=\"John\"  or  age=15 "
;; (where #:a 1 (/or #:c 3 #:d 4)) ==> " where  a=1  and  c=3  or  d=4 "
;; Complex rules could be done with string templation.
(define (/or . conds)
  (parameterize ((get-and/or " or ") (get-prefix ""))
    (apply where conds)))

;; (where (/or #:name "John" #:age 15 #:email "john@artanis.com" (/and #:c 2 #:d 4) ))
;; ==> " name=\"John\"  or  age=\"15\"  or  email=\"asdf\"  or  a=\"1\"  and  b=\"2\" "
(define (/and . conds)
  (parameterize ((get-and/or " and ") (get-prefix ""))
    (apply where conds)))

(define (/in . lst)
  (match lst
    (() "")
    (((? keyword? column) (? list? vals))
     (if (use-params?)
         (format #f "~a in (~{~a~^,~})"
                 (keyword->symbol column) (map-in-order add-param! vals))
         (format #f "~a in (~{~a~^,~})"
                 (keyword->symbol column) (fix-values vals))))
    (else (throw 'artanis-err 500 /in "Invalid args" lst))))

(define-syntax-rule (/exists subquery)
  (parameterize ((drop-tail-semicolon? #t))
    (format #f "exists (~a)" subquery)))

(define (/literal str)
  (format #f "'~a'" str))

(define-syntax-rule (/id name)
  (make-sql-id 'name))

;; Internal ->sql that generates SQL without parameter handling
(define-syntax ->sql-internal
  (syntax-rules (select insert alter create update delete use drop truncate open)
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
     (->end 'truncate (-> "table ~a" name)))
    ((_ drop rest ...)
     (->end 'drop (sql-drop rest ...)))
    ((_ use db)
     (->end 'use db))
    ((_ open db)
     ;; NOTE: This is only for SQLite3 for it's lacking of `use'
     ;;       statement. And MUSTN'T be end with `;'.
     (with-dbd
      'sqlite3
      (-> ".open ~a" db)))))

;; TODO: We need stronger syntax sugar for Language-INtegrated-Query,
;;       which is what LINQ does.
;; NOTE: This will not be a small work.

;; (letq ((w people) (c people))
;;   (->sql select 'w.name (where #:w.name "nala")))
;; ==>
;; "from people as w from people as c select w.name where w.name='nala';"
(define-syntax letq
  (lambda (stx)
    (define (expand-bindings bs)
      (syntax-case bs ()
        (() #'"")
        (((alias table) . rest)
         #`(string-append
            (format #f "from ~a as ~a " 'table 'alias)
            #,(expand-bindings #'rest)))))
    (syntax-case stx ()
      ((_ bindings sql)
       #`(string-append
          #,(expand-bindings #'bindings)
          sql)))))

(define (value-detect v)
  (cond
   ((sql-type? v) (sql-type-value v))
   ((sql-id? v) (sql-id-name v))
   (else v)))

;; NOTE: We return sql-string and parameters with Multi-Variables,
;;       this brings two advantages:
;; 1. The old GNU Artanis downstream code may still work with ->sql, and for
;;    Guile, the procedures that NOT using call-with-values will accept the
;;    first return value by default. This is Guile specific, for RnRs, it's
;;    an unspecified activity.
;;    We urge you to modify your code to use our FPRM or call-with-values to
;;    work with dbi-params-query.
;; 2. Old users may use (parameterize ((use-params? #f)) ...) to disable it.
(define-syntax ->sql
  (syntax-rules (select insert alter create update delete use drop truncate open)
    ((_ type rest ...)
     (parameterize ((current-param-index 1)
                    (current-param-list (new-queue)))
       ;; NOTE: current-param-list is parameterizing protected global var,
       ;;       though it's thread safe, we still need make sure it's
       ;;       sequential, so we must use let* here.
       (let* ((sql (->sql-internal type rest ...))
              (params (map value-detect (queue-slots (current-param-list)))))
         (values sql params))))))
