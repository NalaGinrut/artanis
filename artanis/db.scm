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

(define-module (artanis db)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis server)
  #:use-module (dbi dbi)
  #:use-module (ice-9 match)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (DB-open
            DB-close
            DB-query
            DB-result-status
            DB-get-all-rows
            init-DB
            clear-conn-from-rc!))

;; NOTE:
;; <db> is only used for store connect config info, it doens't contain
;; connection object. It's useless when the connection-pool init is down.
(define-record-type <db>
  (fields 
   username passwd))

(define-record-type <mysql>
  (parent <db>)
  ;; mysql provides two modes, addr:port or socketfile
  ;; the default values:
  ;; port: 3306, addr: localhost, dbname: artanis
  (fields dbname port addr socketfile))

(define-syntax-rule (->mysql mysql)
  (match mysql
    (($ <mysql> ($ <db> _ username passwd) dbname port addr #f)
     (format #f "~a:~a:~a:tcp:~a:~a" username passwd dbname addr port))
    (($ <mysql> ($ <db> _ username passwd) dbname #f #f socketfile)
     (format #f "~a:~a:~a:socket:~a" username passwd dbname socketfile))
    (else (error 'mysql "Wrong connection config!" mysql))))

(define-record-type <sqlite3>
  (parent <db>)
  ;; sqlite3 only need dbname to open
  ;; default dbname: artanis
  (fields dbname))

(define-syntax-rule (->sqlite3 sqlite3)
  (match sqlite3
    (($ <sqlite3> ($ <db> _ username passwd) dbname)
     dbname) ; FIXME: is it necessary for sqlite3 to require username/passwd??
    (else (error 'sqlite3 "Wrong connection config!" sqlite3))))

(define-record-type <postgresql>
  (parent <db>)
  ;; postgresql need addr:port to open
  (fields dbname port addr))

(define-syntax-rule (->postgresql postgresql)
  (match postgresql
    (($ <postgresql> ($ <db> _ username passwd) dbname port addr)
     (format #f "~a:~a:~a:tcp:~a:~a" username passwd dbname addr port))
    (else (error 'postgresql "Wrong connection config!" postgresql))))

;; NOTE:
;; DB-do-conn! returns <connection> object
;; All the conn are initilized as 'closed, set 'open iff it's out of pool
(define (DB-do-conn! db)
  (define conn
    (cond
     ((<mysql>? db) (dbi-open "mysql" (->mysql db)))
     ((<sqlite3>? db) (dbi-open "sqlite3" (->sqlite3 db)))
     ((<postgresql>? db) (dbi-open "postgresql" (->postgresql db)))
     (else (error DB-do-conn! "Invalid DB!" db))))
  (make-<connection> 'closed conn))

(define-record-type <connection>
  (fields 
   ;; status provides a simple way to avoid reopen or reclose
   (mutable status) ; open or closed
   conn))

(define (new-DB)
  ;; TODO: 
  ;; 1. Implement a new config module
  ;; 2. Add a new global var to hold DB object (or global env table?).
  ;;    Init new DB on the fly is not allowed.
  (let ((db (get-conf 'database)))
    ;; (get-conf 'database) should contain username passwd and connection method
    (match db
      (('mysql username passwd dbname ('socketfile socketfile)) 
       (make-<mysql> username passwd #f #f socketfile))
      (('mysql username passwd dbname ('port addr port))
       (make-<mysql> username passwd port addr #f))
      (('sqlite3 username passwd dbname)
       (make-<sqlite3> username passwd dbname))
      (('postgresql username passwd dbname ('port addr port))
       (make-<postgresql> username passwd port addr)))))

(define-record-type <connection-pool>
  (fields 
   (mutable size)
   ;; mutex ; maybe it's unecessary for green-thread
   (mutable pool)))

(define *conn-pools* (make-vector (get-conf '(server workers))))
  
(define (conn-pool-out! cp)
  (let ((size (<connection-pool>-size cp))
        (pool (<connection-pool>-pool cp)))
  (<connection-pool>-size-set! cp (1- size))
  ;; lock ; maybe it's unecessary for green-thread
  (queue-out! pool)))

(define (conn-pool-in! cp conn)
  (let ((size (<connection-pool>-size cp))
        (pool (<connection-pool>-pool cp)))
    (<connection-pool>-size-set! cp (1+ size))
    (queue-in! pool conn)))

(define (new-connection-pool)
  (let* ((pool-size (get-conf 'pool-size))
         (db (new-DB))
         (conns (map (lambda (i) (DB-do-conn! db)) (iota pool-size)))
         (pool (list->queue conns)))
    (make-<connection-pool> pool-size pool)))

(define (init-connection-pool)
  (display "connection pools are initilizing...")
  (for-each 
   (lambda (i)
     (vector-set! *conn-pools* i (new-connection-pool)))
   (iota (get-conf '(server wokers))))
  (display "ok\n")
  (format #t "Each size of pool is ~a, ~a pools in total.~%" 
          (get-conf 'pool-size) (get-conf 'workers)))

;; ---------------------conn operations-------------------------------
;; Actually, it's not `open', but get a conn from pool.
(define (DB-open)
  (let* ((pool (vector-ref *conn-pools* (current-worker)))
         (conn (conn-pool-out! pool)))
    (<connection>-status-set! conn 'open)
    conn))
  
(define (DB-query conn sql)
  (cond
   ((not (<connection>? conn))
    (throw 'artanis-err 500 "DB-query: Invalid DB connection!" conn))
   ((not (eq? (<connection>-status conn) 'open))
    (throw 'artanis-err 500 "DB-query: Can't query from a closed connection!" conn))
   ((not (string? sql))
    (throw 'artanis-err 500 "DB-query: Invalid SQL string!" sql))
   (else (dbi-query (<connection>-conn conn) sql))))

;; NOTE: actually it never close the connection, just recycle it.
(define (DB-close conn)
  (cond
   ((not (<connection>? conn))
    (throw 'artanis-err 500 "DB-close: Invalid DB connection!" conn))   
   ((eq? (<connection>-status conn) 'closed)
    (throw 'artanis-err 500 "DB-close: the connection is already closed!" conn))
   (else
    (<connection>-status-set! conn 'closed)
    (conn-pool-in! (current-connection-pool) conn))))

(define (DB-result-status conn)
  (cond
   ((not (<connection>? conn))
    (throw 'artanis-err 500 "DB-result-status: Invalid DB connection!" conn))
   ((not (eq? (<connection>-status conn) 'open))
    (throw 'artanis-err 500 "DB-result-status: Can't query from a closed connection!" conn))
   (else (dbi-get_status (<connection>-conn conn)))))

(define (DB-get-all-rows conn)
  (cond
   ((not (<connection>? conn))
    (throw 'artanis-err 500 "DB-get-all-rows: Invalid DB connection!" conn))
   ((not (eq? (<connection>-status conn) 'open))
    (throw 'artanis-err 500 "DB-get-all-rows: Can't query from a closed connection!" conn))
   (else
    (let lp((next (dbi-get_row (<connection>-conn conn))) (result '()))
      (if next
          (lp (dbi-get_row (<connection>-conn conn)) (cons next result))
          (reverse result))))))
;;--------------------------------------------------------------------

(define (init-DB)
  (init-connection-pool))

;; Clear auto connection here
(define (clear-conn-from-rc! rc)
  (DB-close ((@ (artanis artanis) rc-conn) rc))
  ((@ (artanis artanis) rc-conn!) rc #f))
