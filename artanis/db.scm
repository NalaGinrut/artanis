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

(define-module (artanis db)
  #:use-module (artanis utils)
  #:use-module (dbi dbi)
  #:use-module (oop goops))

(module-export-all! (current-module))
 
(define-class <artanis-db> ()
  (db #:init-value #f #:getter db-get #:setter db-set!))

(define-generic conn)
(define-generic re-conn)
(define-generic close)
(define-generic insert)
(define-generic query)
(define-generic get-status)

(define-method (insert (self <artanis-db>) (table <string>) . vals)
  (let ((vstr (string-join vals ",")))
    (query self (format #f "insert into ~a values (~a)" table vstr))))

(define-method (close (self <artanis-db>))
  (dbi-close (db-get self))
  (db-set! self #f))

(define-method (query (self <artanis-db>) (sql <string>))
  (dbi-query (db-get self) sql))

(define-method (get-status (self <artanis-db>))
  (dbi-get_status (db-get self)))

(define-method (check-status (self <artanis-db>))
  (let ((st (get-status self)))
    (values (car st) (cdr st))))

(define-method (get-all-rows (self <artanis-db>))
  (let ((db (db-get self)))
    (let lp((next (dbi-get_row db)) (result '()))
      (if next
          (lp (dbi-get_row db) (cons next result))
          (reverse result)))))

(define-method (get-one-row (self <artanis-db>))
  (dbi-get_row (db-get self)))

;; NOTE: don't store passwd
(define-class <mysql> (<artanis-db>)
  (user #:init-value "artanis" #:init-keyword #:user 
        #:getter db-user #:setter db-user!)
  (name #:init-value "artanis" #:init-keyword #:name
        #:getter db-name #:setter db-name!)
  (port #:init-value "3306" #:init-keyword #:port
        #:getter db-port #:setter db-port!)
  (addr #:init-value "tcp:localhost" #:init-keyword #:addr
        #:getter db-addr #:setter db-addr!))
 
(define-method (conn (self <mysql>) (passwd <string>))
  (let ((user (db-user self))
        (name (db-name self))
        (port (db-port self))
        (addr (db-addr self)))
    (db-set! self (dbi-open "mysql"
                            (format #f "~a:~a:~a:~a:~a"
                                    user passwd name addr port)))
    self))

(define-method (re-conn (self <mysql>) (user <string>)
                        (passwd <string>) (addr <string>))
  (and user (db-user! self user))
  (and addr (db-addr! self user))
  (close self)
  (conn self passwd))

(define-class <sqlite3> (<artanis-db>)
  (db-name #:init-value "artanis" #:init-keyword #:db-name #:getter db-name))

(define-method (conn (self <sqlite3>))
  (let ((name (db-name self)))
    (db-set! self (dbi-open "sqlite3" name))
    self))

(define-method (re-conn (self <sqlite3>))
  (close self)
  (conn self))

(define-class <postgresql> (<artanis-db>)
  (user #:init-value "artanis" #:init-keyword #:user
        #:getter db-user #:setter db-user!)
  (name #:init-value "artanis" #:init-keyword #:name
        #:getter db-name #:setter db-name!)
  (addr #:init-value "tcp:localhost"
        #:init-keyword #:addr #:getter db-addr #:setter db-addr!)
  (port #:init-value "5432" #:init-keyword #:port 
        #:getter db-port #:setter db-port!))

(define-method (conn (self <postgresql>) (passwd <string>))
  (let ((user (db-user self))
        (name (db-name self))
        (port (db-port self))
        (addr (db-addr self)))
    (db-set! self (dbi-open "postgresql"
                        (format #f "~a:~a:~a:~a:~a" 
                                user passwd name addr port)))
    self))

(define-method (re-conn (self <postgresql>) (user <string>) (name <string>)
                        (passwd <string>) (addr <string>))
  (and user (db-user! self user))
  (and name (db-name! self name))
  (and addr (db-addr! self user))
  (close self)
  (conn self passwd))
