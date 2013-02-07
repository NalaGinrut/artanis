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
  (db #:init-value #f))

(define-method (close (self <artanis-db>))
  (dbi-close (db self))
  (db! self #f))

(define-method (query (self <artanis-db>) (sql <string>))
  (dbi-query (db self) sql))

(define-method (get-all-rows (self <artanis-db>) (field <string>))
  (query self field)
  (define db (db self))
  (let lp((next (dbi-get_row db)) (result '()))
    (if next
        (lp (dbi-get_row db) (cons next result))
        (reverse result))))

(define-method (get-one-row (self <artanis-db>))
  (dbi-get_row (db self)))

;; NOTE: don't store passwd
(define-class <mysql> (<artanis-db>)
  (user #:init-value "artanis" #:init-keyword #:user 
        #:getter user #:setter user!)
  (name #:init-value "artanis" #:init-keyword #:name
        #:getter name #:setter name!)
  (port #:init-value "3306" #:init-keyword #:port
        #:getter port #:setter port!)
  (addr #:init-value "tcp:localhost" #:init-keyword #:addr
        #:getter addr #:setter addr!))
 
(define-method (open (self <mysql>) (name <string>) (passwd <string>))
  (let ((user (user self))
        (name (name self))
        (port (port self))
        (addr (addr self)))
    (db! self (dbi-open "mysql"
                        (format #f "~a:~a:~a:~a:~a:"
                                user passwd name addr port)))
    self))

(define-method (re-open (self <mysql>) (user <string>)
                        (passwd <string>) (addr <string>))
  (and user (db! self user))
  (and addr (addr! self user))
  (close self)
  (open self passwd))

(define-class <sqlite3> ()
  (db-name #:init-value "artanis" #:init-keyword #:db-name #:getter db-name))

(define-method (open (self <sqlite3>) (name <string>))
  (let ((db-name (db-name self)))
    (db! self (dbi-open "sqlite3" db-name))
    self))

(define-method (re-open (self <sqlite3>))
  (close self)
  (open self))

(define-class <postgresql> ()
  (user #:init-value "artanis" #:init-keyword #:user
        #:getter user #:setter user!)
  (name #:init-value "artanis" #:init-keyword #:name
        #:getter name #:setter name!)
  (addr #:init-value "tcp:localhost"
        #:init-keyword #:addr #:getter addr #:setter addr!)
  (port #:init-value "5432" #:init-keyword #:port 
        #:getter port #:setter port!))

(define-method (open (self <postgresql>) (name <string>) (passwd <string>))
  (let ((user (user self))
        (name (name self))
        (port (port self))
        (addr (addr self)))
    (db! self (dbi-open "postgresql"
                        (format #f "~a:~a:~a:~a:~a" 
                                user passwd name addr port)))
    self))

(define-method (re-open (self <postgresql>) (user <string>) (name <string>)
                        (passwd <string>) (addr <string>))
  (and user (db! self user))
  (and name (name! self name))
  (and addr (addr! self user))
  (close self)
  (open self passwd))
