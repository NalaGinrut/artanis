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
  #:use-module (dbi dbi)
  #:use-module (ice-9 match)
  #:use-module ((rnrs) #:select (define-record-type)))

;; TODO: add init-db in init-server for initializing connection pool

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
     (format #f "~a:~a:~a:socket:~a" username passwd dbname socktfile))
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
;; DB-conn! returns dbi connection object
;; Use dbi-query on it!
(define (DB-conn! db)
  (cond
   ((<mysql>? db) (dbi-open "mysql" (->mysql db)))
   ((<sqlite3>? db) (dbi-open "sqlite3" (->sqlite3 db)))
   ((<postgresql>? db) (dbi-open "postgresql" (->postgresql db)))
   (else (error DB-conn! "Invalid DB!" db))))

;; ---------------------conn operations-------------------------------
;; NOTE: conn objects are actually dbi object!
(define DB-query dbi-query)

;; NOTE: actually it never close the connection, just recycle it.
(define (DB-close conn)
  ;; TODO: implemnt current-connection-pool
  (conn-pool-in! (current-connection-pool) conn)) 

(define DB-status dbi-get_status)

(define (DB-get-all-rows conn)
  (let lp((next (dbi-get_row conn)) (result '()))
    (if next
        (lp (dbi-get_row conn) (cons next result))
        (reverse result))))
;;--------------------------------------------------------------------

(define (new-DB)
  ;; TODO: 
  ;; 1. Implement a new config module
  ;; 2. Add a new global var to hold DB object (or global env table?).
  ;;    Init new DB on the fly is not allowed.
  (let ((db (get-conf 'database)))
    ;; (get-conf 'database) should contain username passwd and connect method
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
         (conns (map (lambda (i) (DB-conn! db)) (iota pool-size)))
         (pool (list->queue conns)))
    (make-<connection-pool> pool-size pool)))

(define (init-connection-pool)
  ;; TODO: create pool and add it to global
  )

(define (init-DB)
  ;; TODO:
  ;; 1. create a new db object
  ;; 2. create a new connection pool
  )
