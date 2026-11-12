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

(define-module (artanis db)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis server)
  #:use-module (artanis route)
  #:use-module (artanis ssql)
  #:use-module (artanis env)
  #:use-module (artanis server ragnarok)
  #:use-module (artanis server scheduler)
  #:use-module ((artanis server server-context)
                #:select (current-client ragnarok-client? wake-up-task!))
  #:autoload (dbi dbi) (dbi-open
                        dbi-query
                        dbi-params-query
                        dbi-get_status
                        dbi-close
                        dbi-get_row
                        dbi-affected-rows)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 threads)
  #:use-module ((rnrs) #:select (define-record-type))
  #:use-module (srfi srfi-43)
  #:export (DB-open
            DB-close
            DB-query
            DB-result-status
            DB-get-all-rows
            DB-get-top-row
            DB-get-n-rows
            DB-affected-rows
            db-conn-success?
            db-conn-failed-reason
            db-conn-returned-reason
            db-conn-is-closed?
            get-conn-from-pool!
            init-DB
            make-<connection>
            <connection>?
            recycle-DB-conn))

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
    (($ <mysql> username passwd dbname addr #f)
     (format #f "~a:~a:~a:tcp:~a" username passwd dbname addr))
    (($ <mysql> username passwd dbname #f socketfile)
     (format #f "~a:~a:~a:socket:~a" username passwd dbname socketfile))
    (else (error 'mysql "Wrong connection config!" mysql))))

(define-record-type <sqlite3>
  (parent <db>)
  ;; sqlite3 only need dbname to open
  ;; default dbname: artanis
  (fields dbname))

(define (->sqlite3 sqlite3)
  (match sqlite3
    (($ <sqlite3> username passwd dbname)
     ;; FIXME: is it necessary for sqlite3 to require username/passwd??
     ;; NOTE: sqlite3 requires the dbname to be postfixed by ".db", or it
     ;;       complains that DB open failed.
     (format #f "~a/prv/~a.sqlite3" (current-toplevel) dbname))
    (else (error 'sqlite3 "Wrong connection config!" sqlite3))))

(define-record-type <postgresql>
  (parent <db>)
  (fields dbname addr socketfile))

(define (->postgresql postgresql)
  (match postgresql
    (($ <postgresql> username passwd dbname addr #f)
     (format #f "~a:~a:~a:tcp:~a" username passwd dbname addr))
    (($ <postgresql> username passwd dbname #f socketfile)
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
   (mutable conn))) ; dbi raw connection

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
      (throw 'artanis-err 500 create-new-DB-conn
             "Database connect failed: "
             (db-conn-failed-reason conn)))
    (run-hook *DB-conn-init-hook* conn)
    conn))

;; ---------------------- waiting for a connection -------------------
;; With db.pool = fixed, a caller finding the pool empty waits until a
;; connection is recycled:
;;  - A Ragnarok task registers itself as a waiter and suspends, so the server
;;    goes on serving others (and the tasks holding the connections can finish
;;    and give them back). recycle-DB-conn wakes the waiters up. The task
;;    waits as long as it takes: a bounded pool means queueing, and every
;;    connection comes back, from the before-response hooks or, when the
;;    handler throws, from handler-render (see page.scm).
;;  - Outside a Ragnarok task nothing can wait: on the server thread it would
;;    block the server for good, and runner threads don't see the pool at all
;;    (they run in a clean dynamic state; don't use DB connections in
;;    runners). So it's 503 at once instead of a hang.
;; NOTE: The old code called (schedule-task), but schedule-task is a parameter,
;;       so that only returned the breaker without calling it: the task never
;;       yielded and get-conn-from-pool! recursed in a busy loop, starving the
;;       event loop, so no connection could ever be recycled.
;; NOTE: All waiters are woken up on each recycle, not one: a waiter may have
;;       gone (its connection timed out or was closed) and would swallow the
;;       only wake-up. Each one retries, and the losers wait again.
(define *conn-waiters* '())                    ; clients of suspended tasks

(define (take-conn-or-#f)
  ;; Must be called with *conn-pool-mutex* held.
  (and (not (queue-empty? (*conn-pool*)))
       (let ((conn (queue-out! (*conn-pool*))))
         ;; DB-close marks a connection 'closed before recycling it and
         ;; only DB-open used to flip it back. A caller that takes one
         ;; straight from the pool (models, migrations, scripts) must get
         ;; a connection DB-query accepts.
         (<connection>-status-set! conn 'open)
         conn)))

(define (wake-up-conn-waiters!)
  ;; Must be called with *conn-pool-mutex* held.
  (let ((waiters *conn-waiters*))
    (set! *conn-waiters* '())
    (for-each wake-up-task! waiters)))

(define (no-conn-to-wait-for)
  (throw 'artanis-err 503 get-conn-from-pool!
         "The DB pool is empty (db.poolsize = ~a) and only a request handler can wait for a connection"
         (get-conf '(db poolsize))))

(define (wait-for-conn-in-task client)
  (let lp ()
    (or (with-mutex *conn-pool-mutex*
          (or (take-conn-or-#f)
              (begin
                (unless (memq client *conn-waiters*)
                  (set! *conn-waiters* (cons client *conn-waiters*)))
                #f)))
        (begin
          (DEBUG "There's no DB connection in the pool, suspend the task ~a~%" client)
          (break-task)
          (lp)))))

(define (get-conn-from-pool!)
  (cond
   ((not (*conn-pool*))
    (error get-conn-from-pool! "Seems the *conn-pool* wasn't well initialized!"
           (*conn-pool*)))
   ((with-mutex *conn-pool-mutex* (take-conn-or-#f))
    => identity)
   (else
    (case (get-conf '(db pool))
      ((increase) (create-new-DB-conn))
      ((fixed)
       (let ((client (current-client)))
         (if (ragnarok-client? client)
             (wait-for-conn-in-task client)
             (no-conn-to-wait-for))))
      (else
       (throw 'artanis-err 500 get-conn-from-pool!
              "BUG: Invalid DB pool mode `~a'" (get-conf '(db pool))))))))

(define (%db-conn-stat conn ret)
  (ret (dbi-get_status (<connection>-conn conn))))

(define (db-conn-success? conn)
  (zero? (%db-conn-stat conn car)))

(define (db-conn-is-closed? conn)
  ;; NOTE: This feature requires guile-dbd-mysql-2.1.7 or higher
  ;; https://github.com/opencog/guile-dbi
  (%db-conn-stat conn (lambda (s)
                        (case (get-conf '(db dbd))
                          ((mysql)
                           (= (car s) 2006))
                          ((postgresql)
                           ;; TODO
                           #f)
                          ((sqlite3)
                           ;; TODO
                           #f)
                          (else
                           (throw 'artanis-err 500 db-conn-is-closed?
                                  "Unsupported DBD `~a'" (get-conf '(db dbd))))))))

;; Discard whatever non-transactional session state the previous caller left
;; behind before the connection is handed to the next one: SET variables,
;; temporary tables, prepared statements, LISTEN registrations, session user
;; variables, an unfinished result set, and so on. Returns #t when the
;; connection is clean and may be reused, #f when it must be replaced.
;;
;; Transactions are deliberately not part of the reset. with-transaction
;; commits or rolls back before the connection is returned; a connection that
;; still has one open did not come back through it and is refused by the
;; backend's reset, so the pool replaces it instead of rolling back behind a
;; caller's back.
;;
;; What "session state" means is backend specific, so every DBD implements
;; the reset itself (dbi-conn-reset dispatches to the backend's
;; __<dbd>_conn_reset_g_db_handle); this code stays database agnostic and
;; only reports the result. The guile-dbi patch series is therefore
;; required -- see patches/README.md.
;;
;; NOTE: dbi-conn-reset is resolved by name rather than listed in #:autoload.
;;       A name in the autoload group that the installed (dbi dbi) does not
;;       export makes the whole group fail to resolve, which would break
;;       every other dbi call. A guile-dbi without the primitive is reported
;;       below and its connections are not reused.
;;
;; A failed reset is logged and the connection is replaced instead of being
;; handed out -- but it is not raised: this runs on the way back into the
;; pool (response hook, dynamic-wind cleanup), where throwing would fail a
;; request that already succeeded and would mask the exception being unwound.
;; Reset is also how a dead connection is detected, so there is no separate
;; liveness check here: a closed handle and a dropped backend both end up as
;; an error status from the backend's reset.
(define (reset-DB-conn-session! conn)
  (let* ((dbd (get-conf '(db dbd)))
         (raw (<connection>-conn conn))
         (dbi-conn-reset
          (catch #t
            (lambda ()
              (module-ref (resolve-module '(dbi dbi)) 'dbi-conn-reset))
            (lambda args #f)))
         (result
          (if dbi-conn-reset
              (catch #t
                (lambda ()
                  (dbi-conn-reset raw)
                  (dbi-get_status raw))
                (lambda args (cons 1 (format #f "~a" args))))
              (cons 1 "installed guile-dbi has no dbi-conn-reset"))))
    (if (zero? (car result))
        #t
        (begin
          (format (current-error-port)
                  "[~a] failed to reset a recycled DB connection: ~a; replacing it~%"
                  dbd (cdr result))
          #f))))

(define (recycle-DB-conn conn)
  (cond
   ((*conn-pool*)
    ;; Every recycled connection is reset before it goes back into the pool:
    ;; there is no caller whose connection is safe to reuse untouched. Reset
    ;; runs outside the pool mutex because it is a round trip to the server
    ;; and no other thread should wait on the pool for it.
    (let ((reusable? (reset-DB-conn-session! conn)))
      (with-mutex
       *conn-pool-mutex*
       (if reusable?
           (queue-in! (*conn-pool*) conn)
           (begin
             ;; The connection could not be reset (dead backend, closed
             ;; handle, an open transaction, or a DBD/guile-dbi without the
             ;; primitive): make a new one instead of handing dirty state
             ;; out. Close the old session first -- the handle is dropped,
             ;; and its finalizer skips the backend close once the status is
             ;; an error, so a live session would otherwise stay connected
             ;; for the life of the process.
             (catch #t
               (lambda () (dbi-close (<connection>-conn conn)))
               (lambda args #t))
             (queue-in! (*conn-pool*) (create-new-DB-conn))))
       (wake-up-conn-waiters!))))
   (else
    (error recycle-DB-conn "Seems the *conn-pool* wasn't well initialized!"
           (*conn-pool*)))))

(define (db-conn-failed-reason conn)
  (%db-conn-stat conn cdr))

(define (db-conn-returned-reason conn)
  (%db-conn-stat conn cdr))

(define (init-connection-pool)
  (display "connection pools are initilizing...")
  (let ((poolsize (get-conf '(db poolsize))))
    (*conn-pool* ; hey, don't forget *conn-pool* is a parameter
     (let ((dbconns
            (map
             (lambda (_) (create-new-DB-conn))
             (iota poolsize))))
       (list->queue dbconns)))
    ;; NOTEL: SQLite3 needs WAL to support connections pool.
    (when (eq? 'sqlite3 (get-conf '(db dbd)))
      (display "000here......\n")
      (let ((conn (get-conn-from-pool!)))
        (display "111here......\n")
        (DB-query conn (format #f "
PRAGMA journal_mode = WAL;
PRAGMA synchronous = NORMAL;
PRAGMA busy_timeout = 5000;
"))
        (recycle-DB-conn conn)))
    (display "DB pool init ok!\n")
    (format #t "Now the size of connection pool is ~a.~%" poolsize)))

;; ---------------------conn operations-------------------------------
;; Actually, it's not `open', but get a conn from pool.
(define (DB-open rc)
  (let ((cur-conn (rc-conn rc)))
    (if cur-conn
        cur-conn
        (let ((conn (get-conn-from-pool!)))
          (<connection>-status-set! conn 'open)
          (rc-conn! rc conn)
          conn))))

(define (db-query-debug-info sql params)
  (DEBUG "DB query: ~a~%" sql)
  (when params
    (DEBUG "DB query params: ~a~%" params)))

;; FIXME: The first level to avoid SQL-injection is that run only one valid statment each time.
;;        So we have to find the index of first valid semi-colon, then use substring.
(define* (DB-query conn sql #:key (check? #t) (params #f))
  (define (recreate-DB-conn!)
    (cond
     ((db-conn-is-closed? conn)
      (let ((new-conn (<connection>-conn (create-new-DB-conn))))
        (format (artanis-current-output)
                "The current DB connection was lost, reconnecting it...~%")
        (<connection>-conn-set! conn new-conn)
        (<connection>-status-set! conn 'open)
        (recreate-DB-conn!)))
     (else #t)))
  (let ((dbd (get-conf '(db dbd))))
    (cond
     ((not (<connection>? conn))
      (throw 'artanis-err 500 DB-query
             "Invalid DB connection ~a!~%" conn))
     ((not (eq? (<connection>-status conn) 'open))
      (throw 'artanis-err 500 DB-query
             "Can't query from a closed connection ~a!~%" conn))
     ((db-conn-is-closed? conn)
      (recreate-DB-conn!)
      (DB-query conn sql #:check? check?))
     ((not (string? sql))
      (throw 'artanis-err 500 DB-query
             "Invalid SQL string ~a!~%" sql))
     (else
      (db-query-debug-info sql params)
      (if params
          (dbi-params-query (<connection>-conn conn) sql params)
          (dbi-query (<connection>-conn conn) sql))
      ;; Clear the parameters after use
      (cond
       ((db-conn-success? conn)
        conn)
       (else
        (cond
         ((db-conn-is-closed? conn)
          (DEBUG "[~a] DB connection was closed, reconnect it ...~%" dbd)
          (DB-query (create-new-DB-conn) sql #:check? check?))
         (else
          (format (current-error-port) "SQL: ~a~%" sql)
          (when params
            (format (current-error-port) "Params: ~a~%" params))
          (if check?
              (format (current-error-port) "[~a] DB-query check failed: ~a~%"
                      dbd (db-conn-failed-reason conn))
              (throw 'artanis-err 500 DB-query "[~a] failed reason: `~a'~%"
                     dbd (db-conn-failed-reason conn)))))))))))

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
      (let ((reason (db-conn-returned-reason conn)))
        (match reason
          ("row end"
           (reverse! result))
          ("row fetched"
           (lp (dbi-get_row (<connection>-conn conn)) (cons next result)))
          (else
           (throw 'artanis-err 500 DB-get-all-rows
                  "Failed to get all rows: ~a" reason))))))))

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
    (let lp ((next (dbi-get_row (<connection>-conn conn)))
             (cnt 0)
             (result '()))
      (let ((reason (db-conn-returned-reason conn)))
        (match reason
          ("row end"
           (reverse! result))
          ("row fetched"
           (if (>= cnt n)
               (reverse! result)
               (lp (dbi-get_row (<connection>-conn conn))
                   (1+ cnt)
                   (cons next result))))
          (else
           (throw 'artanis-err 500 DB-get-n-rows
                  "Failed to get rows: ~a" reason))))))))

(define (DB-affected-rows conn)
  (cond
   ((not (<connection>? conn))
    (throw 'artanis-err 500 DB-affected-rows
           "Invalid DB connection ~a!" conn))
   ((not (eq? (<connection>-status conn) 'open))
    (throw 'artanis-err 500 DB-affected-rows
           "Can't query from a closed connection ~a!" conn))
   (else (dbi-affected-rows (<connection>-conn conn)))))

;;--------------------------------------------------------------------

(define (init-DB)
  (init-connection-pool))
