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

(define-module (artanis fprm)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis ssql)
  #:use-module (artanis db)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (table-drop!
            table-builder))

(define (->0 n m)
  (format #f "~a ~{~a~^ ~}" n m))
(define (->1 n m)
  (format #f "~a(~a) ~{~a~^ ~}" n (car m) (cdr m)))
(define (->2 n m)
  (format #f "~a(~a,~a) ~{~a~^ ~}" n (car m) (cadr m) (cddr m)))
(define (->n n m)
  (format #f "~a(~{'~a'~^,~}) ~{~a~^ ~}" n (car m) (cdr m)))

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
    ((tinyint) (->1 name args))
    ;; SMALLINT(size) -32768 to 32767 normal. 0 to 65535 UNSIGNED*.
    ;; The maximum number of digits may be specified in parenthesis
    ((smallint) (->1 name args))
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
    ((float) (->2 name args))
    ;; DOUBLE(size,d) A large number with a floating decimal point.
    ;; The maximum number of digits may be specified in the size parameter.
    ;; The maximum number of digits to the right of the decimal point is specified in the d parameter
    ((double) (->2 name args))
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
    ((year) (->0 name args))
    (else (throw 'artanis-err 500 "->mysql-type: Invalid type name" name))))

(define (->postgresql-type name . args)
  (case name
    ;; Numeric Types
    ((serial) (->0 name args)) ; serial 4 bytes autoincrementing integer 1 to 2147483647
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

    (else (throw 'artanis-err 500 "->postgresql-type: Invalid type name" name))))

(define (->sql-general-type name . args)
  (case name
    ((character) (->1 name args)) ; CHARACTER(n) Character string. Fixed-length n
    ((varchar) (->1 name args)) ; VARCHAR(n)
    ;; CHARACTER VARYING(n) Character string. Variable length. Maximum length n
    ((char-var) (->1 "character varying" args))
    ((binary) (->1 name args)) ; BINARY(n) Binary string. Fixed-length n
    ((boolean) (->0 name args)) ; BOOLEAN Stores TRUE or FALSE values
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
    ((float) (if (null? args) (->0 name args) (->1 name args)))
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
    (else #f)))

;; (map (lambda (x) (->sql-type (cdr x)))
;;      '((name varchar 10) (age int 5) (email varchar 255)))
;; ==> ("varchar(10)" "int(5)" "varchar(255)")
(define-macro (->sql-type name-and-args)
  `(or (apply ->sql-general-type ,name-and-args)
       (apply ,(symbol-append '-> (string->symbol (get-conf '(db dbd))) '-type)
        ,name-and-args)))

(define (table-drop! rc name)
  (DB-query (rc-conn rc) (->sql drop table if exists name)))

;; NOTE:
;; 1. Use primiary-keys for specifying primary keys, don't specify it in defs directly.
;;    Because we're not going to support foreign keys, so we need to record keys in closures for sync.
(define* (table-builder rc)
  (define (->types x pks)
    (->sql-type
     (if (memq (car x) pks)
         `(,@(cdr x) primary key)
         (cdr x))))
  (lambda* (tname defs #:key (if-exists? #f) (primary-keys '()))
    (let* ((types (map (cut ->types <> primary-keys) defs))
           (sql (case if-exists?
                  ((overwrite drop)
                   (table-drop! rc tname)
                   (->sql create table tname (types)))
                  ((ignore)
                   (->sql create table if not exists tname (types)))
                  (else (->sql create table tname (types)))))
           (conn (rc-conn rc)))
      (DB-query conn sql)
      (lambda mode
        (match mode
          ('valid? (db-conn-success? conn))
          (`(add-keys ,keys)
           (DB-query conn (->sql alter table tname add primary key keys)))
          ;; TODO
          (else (throw 'artanis-err 500 "table-builder: Invalid mode!" mode))
          )))))
