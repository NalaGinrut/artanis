;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2017,2018
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

(define-module (artanis db)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis server)
  #:use-module (artanis route)
  #:use-module (artanis ssql)
  #:use-module (artanis env)
  #:use-module (artanis server ragnarok)
  #:autoload (dbi dbi) (dbi-open dbi-query dbi-get_status dbi-close dbi-get_row)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module ((rnrs) #:select (define-record-type))
  #:use-module (srfi srfi-43)
  #:export (DB-open
            DB-close
            DB-query
            DB-result-status
            DB-get-all-rows
            DB-get-top-row
            DB-get-n-rows
            db-conn-success?
            get-conn-from-pool
            current-connection
            init-DB
            connect-db
            make-<connection>
            <connection>?))

;; NOTE:
;; <db> is the temporal info of specific database config, it doens't contain
;; connection object. It's useless when the connection-pool init is finished.
(define-record-type <db>
  (fields username passwd))

(define-record-type <mysql>
  (parent <db>)
  ;; mysql provides two modes, addr:port or socketfile
  ;; the default values:
  ;; port: 3306, addr: localhost, dbname: artanis
  (fields dbname addr socketfile))

(define (->mysql mysql)
  (match mysql
    (($ <mysql> ($ <db> _ username passwd) dbname addr #f)
     (format #f "~a:~a:~a:tcp:~a" username passwd dbname addr))
    (($ <mysql> ($ <db> _ username passwd) dbname #f socketfile)
     (format #f "~a:~a:~a:socket:~a" username passwd dbname socketfile))
    (else (error 'mysql "Wrong connection config!" mysql))))

(define-record-type <sqlite3>
  (parent <db>)
  ;; sqlite3 only need dbname to open
  ;; default dbname: artanis
  (fields dbname))

(define (->sqlite3 sqlite3)
  (match sqlite3
    (($ <sqlite3> ($ <db> _ username passwd) dbname)
     ;; FIXME: is it necessary for sqlite3 to require username/passwd??
     ;; NOTE: sqlite3 requires the dbname to be postfixed by ".db", or it
     ;;       complains that DB open failed.
     (string-append dbname ".db"))
    (else (error 'sqlite3 "Wrong connection config!" sqlite3))))

(define-record-type <postgresql>
  (parent <db>)
  (fields dbname addr socketfile))

(define (->postgresql postgresql)
  (match postgresql
    (($ <postgresql> ($ <db> _ username passwd) dbname addr #f)
     (format #f "~a:~a:~a:tcp:~a" username passwd dbname addr))
    (($ <postgresql> ($ <db> _ username passwd) dbname #f socketfile)
     (format #f "~a:~a:~a:socket:~a" username passwd dbname socketfile))
    (else (error 'postgresql "Wrong connection config!" postgresql))))

;; NOTE:
;; DB-do-conn! returns <connection> object.
(define (DB-do-conn! db)
  (define (process dbdname converter)
    (let ((cstr (converter db)))
      (and (get-conf 'debug-mode) (format #t "<~a> ~a~%" dbdname cstr))
      (dbi-open dbdname cstr)))
  (define conn
    (cond
     ((<mysql>? db) (process "mysql" ->mysql))
     ((<sqlite3>? db) (process "sqlite3" ->sqlite3))
     ((<postgresql>? db) (process "postgresql" ->postgresql))
     (else (error DB-do-conn! "Invalid DB!" db))))
  (make-<connection> 'open conn))

(define-record-type <connection>
  (fields
   ;; status provides a simple way to avoid reopen or reclose
   (mutable status) ; open or closed
   conn))

(define (%do-connect dbd str)
  (define-syntax-rule (-> s) (format #f "~a" s))
  (let ((conn (make-<connection> 'open (dbi-open (-> dbd) str))))
    (if (db-conn-success? conn)
        conn
        (throw 'artanis-err 500 "connect to DB error:" (db-conn-failed-reason conn)))))

;; Connect database from DBI.
;; e.g: (connect-db "mysql" "root:123:artanis:tcp:localhost:3306")
(define connect-db
  (case-lambda*
      ((dbd str) (%do-connect dbd str))
    ((dbd #:key (db-name "artanis") (db-username "root") (db-passwd "")
          (addr "localhost:3306") (proto 'tcp))
     (let ((str (case dbd
                  ((mysql) (format #f "~a:~a:~a:~a:~a"
                                   db-username db-passwd db-name proto addr))
                  ((postgresql) (format #f "~a:~a:~a:~a:~a"
                                        db-username db-passwd db-name proto addr))
                  ((sqlite3) (format #f "~a" db-name)))))
       (%do-connect dbd str)))))

(define (new-DB)
  ;; TODO:
  ;; 1. Implement a new config module
  ;; 2. Add a new global var to hold DB object (or global env table?).
  ;;    Init new DB on the fly is not allowed.
  (let ((db (get-conf 'database)))
    ;; (get-conf 'database) should contain username passwd and connection method
    (match db
      (('mysql username passwd dbname ('socketfile socketfile))
       (make-<mysql> username passwd dbname #f socketfile))
      (('mysql username passwd dbname ('tcp addr))
       (make-<mysql> username passwd dbname addr #f))
      (('sqlite3 username passwd dbname)
       (make-<sqlite3> username passwd dbname))
      (('postgresql username passwd dbname ('tcp addr))
       (make-<postgresql> username passwd dbname addr #f))
      (('postgresql username passwd dbname ('socketfile sock))
       (make-<postgresql> username passwd dbname #f sock))
      (else (error new-DB "Something is wrong, invalid db!" db)))))

(define (create-new-DB-conn)
  (let ((conn (DB-do-conn! (new-DB))))
    (when (not (db-conn-success? conn))
      (error init-connection-pool "Database connect failed: "
             (db-conn-failed-reason conn)))
    (run-hook *DB-conn-init-hook* conn)
    conn))

(define (get-conn-from-pool)
  (if *conn-pool*
      (if (queue-empty? *conn-pool*)
          (case (get-conf '(db pool))
            ((increase) (create-new-DB-conn))
            ((fixed)
             (DEBUG "There's no DB connection from pool, wait for a moment!~%")
             (try-to-recycle-resources) ; must be in front of scheduling
             (schedule-task)
             (get-conn-from-pool))
            (else
             (throw 'artanis-err 500 get-conn-from-pool
                    "BUG: Invalid DB pool mode `~a'" (get-conf '(db pool)))))
          (queue-out! *conn-pool*))
      (error get-conn-from-pool "Seems the *conn-pool* wasn't well initialized!"
             *conn-pool*)))

(define (recycle-DB-conn conn)
  (if *conn-pool*
      (queue-in! *conn-pool* conn)
      (error recycle-DB-conn "Seems the *conn-pool* wasn't well initialized!"
             *conn-pool*)))

(define (%db-conn-stat conn ret)
  (ret (dbi-get_status (<connection>-conn conn))))

(define (db-conn-success? conn)
  (zero? (%db-conn-stat conn car)))

(define (db-conn-failed-reason conn)
  (%db-conn-stat conn cdr))


(define (init-connection-pool)
  (display "connection pools are initilizing...")
  (let ((poolsize (get-conf '(db poolsize))))
    (set! *conn-pool*
      (let ((dbconns
             (map
              (lambda (_) (create-new-DB-conn))
              (iota poolsize))))
        (list->queue dbconns)))
    (display "DB pool init ok!\n")
    (format #t "Now the size of connection pool is ~a.~%" poolsize)))

;; ---------------------conn operations-------------------------------
;; Actually, it's not `open', but get a conn from pool.
(define (DB-open rc)
  (let ((cur-conn (rc-conn rc)))
    (if cur-conn
        cur-conn
        (let ((conn (get-conn-from-pool)))
          (<connection>-status-set! conn 'open)
          (rc-conn! rc conn)
          conn))))

(define (db-query-debug-info sql)
  (when (get-conf 'debug-mode)
    (display sql)(newline)))

;; FIXME: The first level to avoid SQL-injection is that run only one valid statment each time.
;;        So we have to find the index of first valid semi-colon, then use substring.
(define* (DB-query conn sql #:key (check? #f))
  (cond
   ((not (<connection>? conn))
    (throw 'artanis-err 500 DB-query
           "Invalid DB connection ~a!" conn))
   ((not (eq? (<connection>-status conn) 'open))
    (throw 'artanis-err 500 DB-query
           "Can't query from a closed connection ~a!" conn))
   ((not (string? sql))
    (throw 'artanis-err 500 DB-query
           "Invalid SQL string ~a!" sql))
   (else
    (db-query-debug-info sql)
    (dbi-query (<connection>-conn conn) sql)
    (when (not (db-conn-success? conn))
      (format (current-error-port) "SQL: ~a~%" sql)
      (if check?
          (format (current-error-port) "DB-query check failed: ~a"
                  (db-conn-failed-reason conn))
          (throw 'artanis-err 500 DB-query "failed reason: `~a'~%"
                 (db-conn-failed-reason conn))))
    conn)))

;; NOTE: actually it'll never close the connection, just recycle it.
(define (DB-close conn)
  (cond
   ((not (<connection>? conn))
    (throw 'artanis-err 500 DB-close
           "Invalid DB connection ~a!" conn))
   ((eq? (<connection>-status conn) 'closed)
    (throw 'artanis-err 500 DB-close
           "The connection ~a is already closed!" conn))
   (else
    ;; NOTE: Because Artanis uses green-thread, all requests share the same
    ;;       DB connection, so it's dangerous to leave the connection to next
    ;;       request!
    ;;       We use "select NULL;" here to clear the last query, sometimes last
    ;;       request may left some results weren't clear. It's reasonable! Since
    ;;       sometimes we don't use DB-get-all-rows, which means something will
    ;;       be left in the <connection> object.
    ;; NOTE: "select null;" is safe and quickly to clear the last query.
    (DB-query conn "select null;")
    (<connection>-status-set! conn 'closed)
    (recycle-DB-conn conn))))

(define (DB-result-status conn)
  (cond
   ((not (<connection>? conn))
    (throw 'artanis-err 500 DB-result-status
           "Invalid DB connection ~a!" conn))
   ((not (eq? (<connection>-status conn) 'open))
    (throw 'artanis-err 500 DB-result-status
           "Can't query from a closed connection ~a!" conn))
   (else (dbi-get_status (<connection>-conn conn)))))

(define (DB-get-all-rows conn)
  (cond
   ((not (<connection>? conn))
    (throw 'artanis-err 500 DB-get-all-rows
           "Invalid DB connection ~a!" conn))
   ((not (eq? (<connection>-status conn) 'open))
    (throw 'artanis-err 500 DB-get-all-rows
           "Can't query from a closed connection ~a!" conn))
   (else
    (let lp((next (dbi-get_row (<connection>-conn conn))) (result '()))
      (if next
          (lp (dbi-get_row (<connection>-conn conn)) (cons next result))
          (reverse! result))))))

(define (DB-get-top-row conn)
  (cond
   ((not (<connection>? conn))
    (throw 'artanis-err 500 DB-get-top-row
           "Invalid DB connection ~a!" conn))
   ((not (eq? (<connection>-status conn) 'open))
    (throw 'artanis-err 500 DB-get-top-row
           "Can't query from a closed connection ~a!" conn))
   (else (dbi-get_row (<connection>-conn conn)))))

(define (DB-get-n-rows conn n)
  (cond
   ((not (<connection>? conn))
    (throw 'artanis-err 500 DB-get-n-rows
           "Invalid DB connection ~a!" conn))
   ((not (eq? (<connection>-status conn) 'open))
    (throw 'artanis-err 500 DB-get-n-rows
           "Can't query from a closed connection ~a!" conn))
   (else
    (let lp((next (dbi-get_row (<connection>-conn conn))) (cnt 0) (result '()))
      (if (or next (< cnt n))
          (lp (dbi-get_row (<connection>-conn conn)) (1+ cnt) (cons next result))
          (reverse! result))))))
;;--------------------------------------------------------------------

(define (current-connection)
  (get-conn-from-pool))

(define (init-DB)
  (init-connection-pool))
