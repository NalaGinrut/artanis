;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2016,2017,2018,2019,2022
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
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

(define-module (artanis fprm)
  #:use-module (artanis env)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis route)
  #:use-module (artanis ssql)
  #:use-module (artanis db)
  #:use-module ((srfi srfi-1) #:renamer (symbol-prefix-proc 'srfi-1:))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (map-table-from-DB
            make-table-getter
            make-table-setter
            make-table-builder
            make-table-dropper
            make-table-modifier)
  ;; NOTE:
  ;; We re-export these symbols so that users may use FPRM to handle DB
  ;; independently, without using the web-framework.
  #:re-export (->sql
               where
               /or
               /and
               connect-db))

;; This is Functional Programming Relational Mapping.
;; It's named as this because ... (I'll continue later)

(define (->0 n m)
  (format #f "~a ~{~a~^ ~}" n m))
(define (->1 n m)
  (format #f "~a(~a) ~{~a~^ ~}" n (car m) (cdr m)))
(define (->0/1 n m)
  (if (pair? m) (->1 n m) (->0 n m)))
(define (->2 n m)
  (format #f "~a(~a,~a) ~{~a~^ ~}" n (car m) (cadr m) (cddr m)))
(define (->n n m)
  (format #f "~a(~{'~a'~^,~}) ~{~a~^ ~}" n (car m) (cdr m)))

(define (float-args-is-valid? args)
  (if (< (length args) 2)
      #f
      (let ((i (car args))
            (f (cadr args)))
        (and (positive-integer? i)
             (positive-integer? f)))))

(define (->mysql-type name . args)
  (case name
    ;; Text types:

    ;; CHAR(size) Holds a fixed length string (can contain letters, numbers, and special characters).
    ;; The fixed size is specified in parenthesis. Can store up to 255 characters
    ((char) (->1 name args))
    ((tinytext) (->0 name args)) ; TINYTEXT Holds a string with a maximum length of 255 characters
    ((text) (->0 name args)) ; TEXT Holds a string with a maximum length of 65,535 characters
    ((blob) (->0 name args)) ; BLOB For BLOBs (Binary Large OBjects). Holds up to 65,535 bytes of data
    ((mediumtext) (->0 name args)) ; MEDIUMTEXT Holds a string with a maximum length of 16,777,215 characters
    ((mediumblob) (->0 name args)) ; MEDIUMBLOB For BLOBs (Binary Large OBjects). Holds up to 16,777,215 bytes of data
    ((longtext) (->0 name args)) ; LONGTEXT Holds a string with a maximum length of 4,294,967,295 characters
    ((longblob) (->0 name args)) ; LONGBLOB For BLOBs (Binary Large OBjects). Holds up to 4,294,967,295 bytes of data
    ;; ENUM(x,y,z,etc.) Let you enter a list of possible values. You can list up to 65535 values in an ENUM list.
    ;; If a value is inserted that is not in the list, a blank value will be inserted.
    ;; NOTE: The values are sorted in the order you enter them.
    ;; You enter the possible values in this format: ENUM('X','Y','Z')
    ((enum) (->n name args))
    ;; SET Similar to ENUM except that SET may contain up to 64 list items and can store more than one choice
    ((set) (->n name args))

    ;; Number types:

    ;; TINYINT(size) -128 to 127 normal. 0 to 255 UNSIGNED*.
    ;; The maximum number of digits may be specified in parenthesis
    ((tinyint) (->0/1 name args))
    ;; SMALLINT(size) -32768 to 32767 normal. 0 to 65535 UNSIGNED*.
    ;; The maximum number of digits may be specified in parenthesis
    ((smallint) (->0/1 name args))
    ;; MEDIUMINT(size) -8388608 to 8388607 normal. 0 to 16777215 UNSIGNED*.
    ;; The maximum number of digits may be specified in parenthesis
    ((mediumint) (->1 name args))
    ;; INT(size) -2147483648 to 2147483647 normal. 0 to 4294967295 UNSIGNED*.
    ;; The maximum number of digits may be specified in parenthesis
    ((int) (->1 name args))
    ;; BIGINT(size) -9223372036854775808 to 9223372036854775807 normal.
    ;; 0 to 18446744073709551615 UNSIGNED*. The maximum number of digits may be specified in parenthesis
    ((bigint) (->1 name args))
    ;; FLOAT(size,d) A small number with a floating decimal point.
    ;; The maximum number of digits may be specified in the size parameter.
    ;; The maximum number of digits to the right of the decimal point is specified in the d parameter
    ((float)
     (if (float-args-is-valid? args)
         (->2 name args)
         (throw 'artanis-err 500 ->mysql-type
                "Invalid definition: `~a'" (->2 name args))))
    ;; DOUBLE(size,d) A large number with a floating decimal point.
    ;; The maximum number of digits may be specified in the size parameter.
    ;; The maximum number of digits to the right of the decimal point is specified in the d parameter
    ((double)
     (if (float-args-is-valid? args)
         (->2 name args)
         (throw 'artanis-err 500 ->mysql-type
                "Invalid definition: `~a'" (->2 name args))))
    ;; DECIMAL(size,d) A DOUBLE stored as a string , allowing for a fixed decimal point.
    ;; The maximum number of digits may be specified in the size parameter.
    ;; The maximum number of digits to the right of the decimal point is specified in the d parameter
    ;; NOTE: The integer types have an extra option called UNSIGNED.
    ;;       Normally, the integer goes from an negative to positive value.
    ;;       Adding the UNSIGNED attribute will move that range up so it starts at zero instead of a
    ;;       negative number.
    ((decimal) (->2 name args))

    ;; Date types:

    ;; DATE() A date. Format: YYYY-MM-DD
    ;; NOTE: The supported range is from '1000-01-01' to '9999-12-31'
    ((date) (->0 name args))
    ;; DATETIME() *A date and time combination. Format: YYYY-MM-DD HH:MM:SS
    ;; NOTE: The supported range is from '1000-01-01 00:00:00' to '9999-12-31 23:59:59'
    ((datetime) (->0 name args))
    ;; TIMESTAMP() *A timestamp. TIMESTAMP values are stored as the number of seconds since the
    ;; Unix epoch ('1970-01-01 00:00:00' UTC). Format: YYYY-MM-DD HH:MM:SS
    ;; NOTE: The supported range is from '1970-01-01 00:00:01' UTC to '2038-01-09 03:14:07' UTC
    ((timestamp) (->0 name args))
    ;; TIME() A time. Format: HH:MM:SS
    ;; NOTE: The supported range is from '-838:59:59' to '838:59:59'
    ((time) (->0 name args))
    ;; YEAR() A year in two-digit or four-digit format.
    ;; NOTE: Values allowed in four-digit format: 1901 to 2155.
    ;;       Values allowed in two-digit format: 70 to 69, representing years from 1970 to 2069
    ((boolean) (->0 name args)) ; BOOLEAN Stores TRUE or FALSE values
    ((year) (->0 name args))
    (else (throw 'artanis-err 500 ->mysql-type "Invalid type name `~a'" name))))

(define (->postgresql-type name . args)
  (case name
    ;; Boolean Type
    ((boolean) (->0 name args)) ; BOOLEAN Stores TRUE or FALSE values

    ;; Numeric Types
    ((bigserial) (->0 name args)) ;  bigserial 8 bytes large autoincrementing integer 1 to 9223372036854775807
    ((int) (->0 name args))

    ;; Character Types

    ;; char for 1 byte single-byte internal type
    ((char) (if (null? args) (->0 name args) (->1 name args)))
    ((name) (->0 name args)) ; 64 bytes internal type for object names

    ;; 8 bytes currency amount
    ;; -92233720368547758.08 to +92233720368547758.07
    ((money) (->0 name args))

    ;; bytea 1 or 4 bytes plus the actual binary string
    ;; variable-length binary string
    ((bytea) (->0 name args))

    ;; Geometric Types
    ;; FIXME: personally, I don't think it's worth to support this...
    ;; point 16 bytes Point on a plane (x,y)
    ((point) (->0 name args))
    ;; line 32 bytes Infinite line (not fully implemented) ((x1,y1),(x2,y2))
    ((line) (->0 name args))
    ;; lseg 32 bytes Finite line segment ((x1,y1),(x2,y2))
    ((lseg) (->0 name args))
    ;; box 32 bytes Rectangular box ((x1,y1),(x2,y2))
    ((box) (->0 name args))
    ;; path 16+16n bytes Closed path (similar to polygon) ((x1,y1),...)
    ((path) (->0 name args))
    ;; polygon 40+16n bytes Polygon (similar to closed path) ((x1,y1),...)
    ((polygon) (->0 name args))
    ;; circle 24 bytes Circle <(x,y),r> (center point and radius)
    ((circle) (->0 name args))

    ;; Network Address Types
    ;; cidr 7 or 19 bytes IPv4 and IPv6 networks
    ((cidr) (->0 name args))
    ;; inet 7 or 19 bytes IPv4 and IPv6 hosts and networks
    ((inet) (->0 name args))
    ;; macaddr 6 bytes MAC addresses
    ((macaddr) (->0 name args))

    ;; Bit String Types
    ;; Bit strings are strings of 1's and 0's.
    ;; They can be used to store or visualize bit masks.
    ;; There are two SQL bit types: bit(n) and bit varying(n),
    ;; where n is a positive integer.
    ((bit) (->1 name args))
    ((bit-varying) (->1 name args))

    ;; Text Search Types
    ;; TODO: do we really need this?

    ;; UUID Type
    ;; TODO: do we really need this?

    ;; XML Type
    ;; TODO: do we really need this?

    ;; Arrays Type
    ;; TODO: complicated, but maybe needed.

    ;; Composite Types
    ;; TODO: do we really need this?

    (else (throw 'artanis-err 500 ->postgresql-type "Invalid type name `~a'" name))))

(define (->sql-general-type name . args)
  (case name
    ((character) (->1 name args)) ; CHARACTER(n) Character string. Fixed-length n
    ((varchar) (->1 name args)) ; VARCHAR(n)
    ;; CHARACTER VARYING(n) Character string. Variable length. Maximum length n
    ((char-var) (->1 "character varying" args))
    ((binary) (->1 name args)) ; BINARY(n) Binary string. Fixed-length n
    ((varbinary) (->1 name args)) ; VARBINARY(n)
    ;; Binary VARYING(n) Binary string. Variable length. Maximum length n
    ((binary-var) (->1 "binary varying" args))
    ((smallint) (->0 name args)) ; SMALLINT Integer numerical (no decimal). Precision 5
    ;; INTEGER(p) Integer numerical (no decimal). Precision p
    ;; or
    ;; INTEGER Integer numerical (no decimal). Precision 10
    ((integer) (if (null? args) (->0 name args) (->1 name args)))
    ((bigint) (->0 name args)) ; BIGINT Integer numerical (no decimal). Precision 19
    ;; DECIMAL(p,s) Exact numerical, precision p, scale s.
    ;; Example: decimal(5,2) is a number that has 3 digits before the decimal and 2 digits after the decimal
    ((decimal) (->2 name args))
    ((numeric) (->2 name args)) ; NUMERIC(p,s) Exact numerical, precision p, scale s. (Same as DECIMAL)
    ;; FLOAT(p) Approximate numerical, mantissa precision p. A floating number in base 10 exponential notation.
    ;; The size argument for this type consists of a single number specifying the minimum precision
    ((float)
     (cond
      ((null? args) (->0 name args))
      ((float-args-is-valid? args) #f) ; trigger DBD specific float definition
      (else (->1 name args))))
    ((real) (->0 name args)) ; REAL Approximate numerical, mantissa precision 7
    ;; DOUBLE PRECISION Approximate numerical, mantissa precision 16
    ((double-precision) (->0 "double precision" args))
    ((date) (->0 name args)) ; DATE Stores year, month, and day values
    ((time) (->0 name args)) ; TIME Stores hour, minute, and second values
    ((timestamp) (->0 name args)) ; TIMESTAMP Stores year, month, day, hour, minute, and second values
    ;; INTERVAL Composed of a number of integer fields, representing a period of time,
    ;; depending on the type of interval
    ((interval) (->0 name args))
    ((array) (->0 name args)) ; ARRAY A set-length and ordered collection of elements
    ((multiset) (->0 name args)) ; MULTISET A variable-length and unordered collection of elements
    ((xml) (->0 name args)) ; XML Stores XML data
    ((serial) (->0 name args)) ; serial 4 bytes autoincrementing integer 1 to 2147483647
    (else #f)))

(define-syntax-rule (->symbol x)
  (cond
   ((string? x) (string->symbol x))
   ((symbol? x) x)
   (else (throw 'artanis-err 500 '->symbol "Invalid type!" x))))

;; (map (lambda (x) (->sql-type (cdr x)))
;;      '((name varchar 10) (age int 5) (email varchar 255)))
;; ==> ("varchar(10)" "int(5)" "varchar(255)")
(define-macro (->sql-type name-and-args)
  `(or (apply ->sql-general-type ,name-and-args)
       (apply ,(symbol-append '-> (->symbol (get-conf '(db dbd))) '-type)
              ,name-and-args)))

(define (make-table-dropper rc/conn)
  (define conn
    (cond
     ((route-context? rc/conn) (DB-open rc/conn))
     ((<connection>? rc/conn) rc/conn)
     (else (throw 'artanis-err 500 make-table-dropper "Invalid rc or conn!" rc/conn))))
  (lambda* (name #:key (dump #f))
    (let ((sql (->sql drop table if exists name)))
      (cond
       ((not dump) (DB-query conn sql))
       (else sql)))))

(define *exception-opts*
  '(#:no-edit #:default #:comment #:storage))
(define (is-exception-opt? x)
  (memq x *exception-opts*))

(define (->mysql-opts x opts)
  (define-syntax-rule (->value o x)
    (format #f "~:@(~a~) ~s" o (kw-arg-ref opts x)))
  (case x
    ((#:not-null) "NOT NULL")
    ((#:null) "NULL")
    ((#:default) (->value 'default x))
    ((#:unique) "UNIQUE")
    ((#:unique-key) "UNIQUE KEY")
    ((#:primary-key) "PRIMARY KEY")
    ((#:key) "KEY")
    ((#:auto-increment) "AUTO_INCREMENT")
    ((#:comment) (->value 'comment x))
    ((#:column-format) "COLUMN_FORMAT")
    ((#:storage) (->value 'storage x))
    ((#:reference-definition) "reference_definition")
    ((#:signed) "signed")
    ((#:unsigned) "unsigned")
    ((#:zerofill) "zerofill")
    (else
     (cond
      ((keyword? x)
       (if (is-exception-opt? x)
           ""
           ;; Throw exception for invalid keyword
           (throw 'artanis-err 500 ->mysql-opts
                  "Invalid opts `~a' for MySQL table definition!" x)))
      (else
       ;; Just ignore the non-keyword, since there're only 2 situations:
       ;; 1. The value of option has already been fetched.
       ;; 2. Invalid item here, neither keyword as options, nor valid value.
       "")))))

(define (->postgresql-opts dbd opts)
  (format #t "PostgreSQL migration hasn't been supported yet!~%"))

(define (->sqlite3-opts x opts)
  (define-syntax-rule (->value o x)
    (format #f "~:@(~a~) ~s" o (kw-arg-ref opts x)))
  (case x
    ((#:not-null) "NOT NULL")
    ((#:null) "NULL")
    ((#:default) "DEFAULT")
    ((#:unique) "UNIQUE")
    ((#:unique-key) (throw 'artanis-err 500 "Unique Key is not supported in sqlite3 during table creation."))
    ((#:primary-key) "PRIMARY KEY")
    ((#:key) (throw 'artanis-err 500 "Key is not supported in sqlite3 during table creation."))
    ((#:auto-increment) "AUTOINCREMENT")
    ((#:auto-now-once) "DEFAULT CURRENT_TIMESTAMP")
    (else
     (cond
      ((keyword? x)
       (if (is-exception-opt? x)
           ""
           (throw 'artanis-err 500 ->mysql-opts
                  (format #f "Invalid opts `~a' for SQLite3 table definition!" x)))
       "")))))

(define *table-builder-opts-handler*
  `((mysql . ,->mysql-opts)
    (postgresql . ,->postgresql-opts)
    (sqlite3 . ,->sqlite3-opts)))

(define (get-table-builder-opts-handler)
  (assoc-ref *table-builder-opts-handler* (get-conf '(db dbd))))

;; NOTE:
;; 1. Use primiary-keys for specifying primary keys, don't specify it in defs directly.
;;    Because we're not going to support foreign keys, so we need to record keys in closures for sync.
;; 2. But, in FPRM, any state should be IMMUTABLE.
;; 3. SOLUTION: use a access-hook for specified table mapping, but there'd be a fine way to avoid
;;              write sql directly each time.
(define* (make-table-builder rc/conn)
  (define conn
    (cond
     ((route-context? rc/conn) (DB-open rc/conn))
     ((<connection>? rc/conn) rc/conn)
     (else (throw 'artanis-err 500 make-table-builder
                  "Invalid rc or conn `~a'!" rc/conn))))
  (define (table-drop! tname)
    (DB-query conn (->sql drop table if exists tname)))
  (define (->opts opts)
    (let ((h (get-table-builder-opts-handler)))
      (string-join
       (srfi-1:fold-right
        (lambda (x p)
          (let ((o (h x opts)))
            (cons o p)))
        '() opts)
       " ")))
  (define (->type/opts x)
    (match x
      ((types ... (opts ...)) (values types (if (null? opts) "" (->opts opts))))
      ((types ...) (values types ""))
      (else (throw 'artanis-err 500 ->type/opts
                   "Invalid definition of the table `~a'!" x))))
  (define (->types x)
    (call-with-values
        (lambda () (->type/opts x))
      (lambda (types opts)
        (format #f "~{~a~^ ~}" (list (car types) (->sql-type (cdr types)) opts)))))
  (define (gen-primary-keys primary-keys)
    (if (null? primary-keys)
        ""
        (format #f "PRIMARY KEY (~{~a~^,~})" primary-keys)))
  ;; TODO: We need a mechanism to sync tables constrained by foreign-keys, since some DB doesn't
  ;;       support foreign keys directly, so we have to provide it outside.
  ;; TODO: who to deal with constrained tables without foreign-keys in stateless?
  (lambda* (tname defs #:key (if-exists? #f) (engine (get-conf '(db engine)))
                  (dump #f) (primary-keys '()))
    (let* ((types (map ->types defs))
           (pks (gen-primary-keys primary-keys))
           (sql (case if-exists?
                  ((overwrite drop)
                   (table-drop! tname)
                   (->sql create table tname (types pks) engine))
                  ((ignore)
                   (->sql create table if not exists tname (types pks) engine))
                  (else (->sql create table tname (types pks) engine)))))
      (cond
       ((not dump)
        (DB-query conn sql)
        (lambda cmd
          (match cmd
            ('(valid?) (db-conn-success? conn))
            ('(primary-keys) primary-keys)
            (`(add-primary-keys ,keys)
             (DB-query conn (->sql alter table tname add primary key keys)))
            ('(drop-primary-keys)
             (DB-query conn (->sql alter table tname drop primary key)))
            ;; TODO
            (_ (throw 'artanis-err 500 make-table-builder "Invalid cmd `~a'!" cmd)))))
       (else sql)))))

;; make-table-setter could be mapped to UPDATE or INSERT, depends on condition.
;; Grammar:
;; UPDATE table_name
;;        SET column1=value1,column2=value2,...
;;        WHERE some_column=some_value;
;;
;; === When there's no conditions ===
;;
;; INSERT INTO table_name
;;        (column1, column1 ...) values (value1, value2 ...)
(define (make-table-setter rc/conn)
  (define (->kvp kargs)
    (let lp((next kargs) (kvs '()) (w ""))
      (match next
        (((? keyword? k) v rest ...)
         (lp rest (cons (list (keyword->symbol k) v) kvs) w))
        (((? string? wcond))
         (lp (cdr next) kvs wcond))
        (() (values kvs w))
        (else (throw 'artanis-err 500 make-table-setter "Invalid kargs `~a'" next)))))
  (define (->kv kvp) (srfi-1:unzip2 kvp))
  (lambda (tname . kargs)
    (let-values (((kvp wcond) (->kvp kargs)))
      (let ((sql (if (string-null? wcond)
                     (let-values (((k v) (->kv kvp)))
                       (->sql insert into tname k values v))
                     ;; NOTE: If there's no cond string (say, `where' clauses), you MUSTN'T
                     ;;       use UPDATE because it'll effect all the records!!! In such case,
                     ;;       you should use INSERT.
                     (->sql update tname set kvp wcond)))
            (conn (cond
                   ((route-context? rc/conn) (DB-open rc/conn))
                   ((<connection>? rc/conn) rc/conn)
                   (else (throw 'artanis-err 500 make-table-setter
                                "Invalid rc or conn `~a'!" rc/conn)))))
        (DB-query conn sql)))))

(define (make-table-getter rc/conn)
  (define (->ret ret)
    (match ret
      ((? integer? n) (format #f " limit ~a " n))
      ('top " limit 1 ")
      ('all "")
      (_ #f)))
  (define (->group-by group-by)
    (match group-by
      ((? list columns)
       (format #f " group by ~{~a~^,~} " columns))
      (_ #f)))
  (define (->order-by order-by)
    (match order-by
      ((columns ... (? (cut memq <> '(asc desc)) m))
       (format #f " order by ~{~a~^,~} ~a " columns m))
      (_ #f)))
  (define (->opts ret group-by order-by cnd foreach)
    (define-syntax-rule (-> x tox)
      (or (and=> x tox) ""))
    (define-syntax-rule (cond-combine c lst)
      (cond
       ((not (string? c))
        (throw 'artanis-err 500 make-table-getter "Invalid #:condition `~a'" c))
       ((not (list? lst))
        (throw 'artanis-err 500 make-table-getter "Invalid #:foreach `~a'" lst))
       (else
        (match lst
          (() c)
          ((column (? list? vals))
           (format #f " ~a ~a in (~{'~a'~^,~}) "
                   (if (string-null? c) " where " (string-append c " and "))
                   column vals))
          (_
           (throw 'artanis-err 500 make-table-getter
                  "Invalid #:foreach `~a', should be (column (val1 val2 val3 ...))"
                  lst))))))
    (string-concatenate
     (list (cond-combine cnd foreach)
           (-> group-by ->group-by)
           (-> order-by ->order-by)
           (-> ret ->ret))))
  (define (->mix columns functions)
    `(,@columns ,@(map (lambda (f) (format #f "~a(~{~a~^,~})" (car f) (cdr f))) functions)))
  (lambda* (tname #:key (columns '(*)) ; get all (*) in default
                  (functions '()) ; put SQL functions here
                  ;; USAGE: #:functions ((funcname1 columns ...) (funcname2 columns ...) ...)
                  ;; e.g:  #:functions '((count Persons.Lastname))
                  ;; ==> count(Persons.Lastname)
                  (ret 'all)
                  ;; three modes for return results:
                  ;; 1. top; 2. all; 3. integer larger than 0
                  (group-by #f)
                  ;; The GROUP BY statement is used in conjunction with the aggregate
                  ;; functions to group the result-set by one or more columns.
                  ;; GRAMMAR:
                  ;; SELECT column_name, aggregate_function(column_name)
                  ;;        FROM table_name
                  ;;        WHERE column_name operator value
                  ;;        GROUP BY column_name
                  (order-by #f)
                  ;; The ORDER BY keyword is used to sort the result-set by one or more columns.
                  ;; The ORDER BY keyword sorts the records in ascending order by default. To sort
                  ;; the records in a descending order, you can use the DESC keyword.
                  ;; GRAMMAR:
                  ;; SELECT column_name(s)
                  ;;        FROM table_name
                  ;;        ORDER BY column_name [ASC|DESC]
                  (condition "")
                  ;; Conditions, for example: #:cond (where #:name "nala")
                  (foreach '())
                  ;; #:foreach accepts an associative list according to this form:
                  ;; (column-name (val1 val2 val3 ...))
                  ;; for example:
                  ;; #:foreach '(city ("sz" "bj" "sh"))
                  ;; NOTE: This is useful to avoid N+1 query problem.
                  (dump #f)
                  ;; Dump SQL to a string as result, when #:dump set to #t, it won't do query operation.
                  (mode 'raw)
                  ;; The return value;
                  ;; 1. 'raw: returns all rows
                  ;; 2. 'getter: return a getter as a function
                  )
    (let ((sql (format #f "select ~{~a~^,~} from ~a ~a;"
                       (->mix columns functions) tname (->opts ret group-by order-by condition foreach)))
          (conn (cond
                 ((route-context? rc/conn) (DB-open rc/conn))
                 ((<connection>? rc/conn) rc/conn)
                 (else (throw 'artanis-err 500 make-table-getter
                              "Invalid rc or conn `~a'!" rc/conn)))))
      (cond
       ((not dump)
        (DB-query conn sql)
        (let ((ret (DB-get-all-rows conn)))
          (case mode
            ((raw) ret)
            ((getter)
             (lambda (k)
               (and (not (null? ret))
                    (assoc-ref (car ret) k))))
            (else (throw 'artanis-err 500 make-table-getter
                         "Invalid mode `~a'" mode)))))
       (else sql)))))

(define (make-table-modifier rc/conn)
  (define (table-add tname col t)
    (->sql alter table tname add col t))
  (define (table-drop tname col)
    (->sql alter table tname drop column col))
  (define (table-alter tname col t)
    (case (get-conf '(db dbd))
      ((mysql) (->sql alter table tname modify column col t))
      ((postgresql) (->sql alter table tname alter column col t))
      ((sqlite3) (throw 'artanis-err 500 table-alter
                        "SQLite3 doesn't support table modification!"))
      (else (throw 'artanis-err 500 table-alter " Unsupported DBD `~a'!"
                   (get-conf '(db dbd))))))
  (define (table-rename tname new)
    (case (get-conf '(db dbd))
      ((mysql) (->sql alter table tname rename new))
      ((postgresql sqlite3) (->sql alter table tname rename to new))
      (else (throw 'artanis-err 500 table-rename "Unsupported DBD `~a'!"
                   (get-conf '(db dbd))))))
  (define (column-drop tname cname)
    (case (get-conf '(db dbd))
      ((mysql postgresql sqlite3) (->sql alter table tname drop column cname))
      (else (throw 'artanis-err 500 column-drop
                   "Unsupported DBD!" (get-conf '(db dbd))))))
  (define (column-rename tname old new . type)
    (case (get-conf '(db dbd))
      ((mysql) (let ((t (car type))) (->sql alter table tname change column old new t)))
      ((postgresql sqlite3) (->sql alter table tname rename column old new))
      (else (throw 'artanis-err 500 column-rename
                   "Unsupported DBD `~a'!" (get-conf '(db dbd))))))
  (define (index-rename tname old new)
    (case (get-conf '(db dbd))
      ((mysql) (->sql alter table tname index old rename to new))
      ((postgresql) (->sql alter table tname index old rename to new))
      ((sqlite3) (throw 'artanis-err 500 index-rename
                        "SQLite3 doesn't support table column modification!"))
      (else (throw 'artanis-err 500 index-rename
                   "Unsupported DBD `~a'!" (get-conf '(db dbd))))))
  (define (row-delete tname t)
    (->sql delete from tname t))
  (define (gen-sql tname op args)
    (case op
      ((add) (apply table-add tname args))
      ((drop) (apply table-drop tname args))
      ((column-drop) (apply column-drop args))
      ((alter) (apply table-alter tname args))
      ((rename) (apply table-rename tname args))
      ((column-rename) (apply column-rename tname args))
      ((index-rename) (apply index-rename args))
      ((row-delete) (apply row-delete tname args))
      (else (throw 'artanis-err 500 make-table-modifier
                   "Invalid op `~a'!" op))))
  (lambda (tname op . args)
    (let ((sql (gen-sql tname op args))
          (conn (cond
                 ((route-context? rc/conn) (DB-open rc/conn))
                 ((<connection>? rc/conn) rc/conn)
                 (else (throw 'artanis-err 500 make-table-modifier
                              "Invalid rc or conn `~a'!" rc/conn)))))
      (DB-query conn sql))))

(define (make-table-counter rc/conn)
  (lambda* (tname #:key (key "*") (column "count") (group-by #f) (condition ""))
    (let ((sql (format #f "select ~acount(~a) as ~a from ~a~a~a;"
                       (if group-by (string-append group-by ",") "")
                       key column tname
                       condition
                       (if group-by (string-append " group by " group-by) "")))
          (conn (cond
                 ((route-context? rc/conn) (DB-open rc/conn))
                 ((<connection>? rc/conn) rc/conn)
                 (else (throw 'artanis-err 500 make-table-counter
                              "Invalid rc or conn `~a'!" rc/conn)))))
      (DB-query conn sql)
      (DB-get-all-rows conn))))

;; NOTE: the name of columns is charactar-caseless, at least in MySQL/MariaDB.
(define (map-table-from-DB rc/conn)
  (define conn
    (cond
     ((route-context? rc/conn) (DB-open rc/conn))
     ((<connection>? rc/conn) rc/conn)
     (else (throw 'artanis-err 500 map-table-from-DB
                  "Invalid rc/conn `~a'" rc/conn))))
  (define getter (make-table-getter conn))
  (define setter (make-table-setter conn))
  (define builder (make-table-builder conn))
  (define dropper (make-table-dropper conn))
  (define modifier (make-table-modifier conn))
  (define counter (make-table-counter conn))
  ;; NOTE:
  ;; It maybe inefficient to fetch table-schema without any cache, because the request session may generate
  ;; table-schema each time. Although we may build a cache or delayed mechanism here, there's one reason
  ;; to give it up: we should keep all the relational-mapping STATELESS, that's why I call it FPRM.
  ;; If you have any doubt between complexity and reliability, I would recommend this cool paper:
  ;; <<Out of the Tar Pit>> Ben Moseley & Peter Marks, February 6, 2006.
  ;; I wish you're interested in stateless someday. Now that you're reading this text, it implies you may
  ;; have chosen Scheme programming language for hacking.
  ;; PS: I'm not boasting that the whole Artanis would be stateless, but FPRM should do it as possible.
  ;; I maybe wrong and fail, but it's worth to try.
  (define (get-table-schema tname)
    (let* ((sql (->sql select '("lcase(column_name)") from
                       (select * from 'information_schema.columns (having #:table_name tname))
                       as 'tmp_alias))
           (sch (DB-get-all-rows (DB-query conn sql))))
      ;; NOTE: The Schema queried from DB is case sensitive, so it's safe to
      ;;       convert all the columns to downcase.
      (map (lambda (x) (string->symbol (cdar x))) sch)))
  (define (checker ci? tname . args)
    (define schema (get-table-schema tname))
    (define-syntax-rule (-> c) (map (cut normalize-column <> ci?) c))
    (and (srfi-1:every (lambda (x) (memq x schema)) (-> args)) #t))
  (define (table-exists? tname)
    (case (get-conf '(db dbd))
      ((mysql) (DB-query rc/conn (format #f "show tables like '~a';" tname)))
      ((postgresql) (DB-query rc/conn (format #f "select from information_schema.tables where table_schema='public' and table_name='~a';" tname)))
      ((sqlite3) (DB-query rc/conn (format #f "select * from sqlite_master where type='table' and name='~a'" tname)))
      (else (throw 'artanis-err 500 table-exists?
                   "Unsupported DBD `~a'!" (get-conf '(db dbd))))
      )
    ;; If there's no result, dbi will return #f as the result.
    (DB-get-top-row rc/conn))
  (lambda (cmd tname . args)
    (define-syntax-rule (->call func)
      (apply func (cons tname args)))
    (case cmd
      ((valid?) (db-conn-success? conn))
      ((get) (->call getter))
      ((set) (->call setter))
      ((table-exists?) (table-exists? tname))
      ((try-create try-build)
       (if (table-exists? tname)
           (format (artanis-current-output) "Table `~a' already exists!~%" tname)
           (->call builder)))
      ((create build) (->call builder))
      ((remove delete drop) (->call dropper))
      ;; (_ exists? 'Persons 'city 'lastname)
      ((check exists?) (apply checker #f tname args))
      ;; (_ ci-exists? 'Persons 'City 'LastName)
      ((ci-check ci-exists?) (apply checker #t tname args))
      ;; schema is always in downcase.
      ((schema) (get-table-schema tname))
      ((mod) (->call modifier))
      ((count) (->call counter))
      (else (throw 'artanis-err 500 map-table-from-DB
                   "Invalid cmd: `~a'" cmd)))))

(define* (fprm-find mt tname val #:key (id 'id))
  (mt 'get tname #:condition (where (symbol->keyword id) val)))

(define *table-mapper-handlers*
  `((find . ,fprm-find)))

(define (create-table-mapper tname rc/conn)
  (let ((mt (map-table-from-DB rc/conn)))
    (lambda (cmd . args)
      (and=> (assoc-ref *table-mapper-handlers* cmd)
             (lambda (h) (apply h mt tname args))))))
