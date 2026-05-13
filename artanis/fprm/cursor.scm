;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2026
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

(define-module (artanis fprm)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis db)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (prepare-for-cursor
            return-cursor-generator
            cursor-fetch))

(define-record-type db-cursor-generator
    (fields
     name
     size
     conn))

(define (prepare-for-cursor conn cursor-name cursor-size)
  (when (not (eq? 'postgresql (get-conf '(db dbd))))
    (throw 'artanis-err 500 prepare-for-cursor
           "SQL level cursor is only supported in PostgreSQL, but current dbd is `~a'!"
           (get-conf '(db dbd))))
  (let* ((prepare (format #f "DECLARE ~a CURSOR WITH HOLD FOR "
                          cursor-name)))
    (DB-query conn "BEGIN;")
    (DB-query conn prepare)))

(::define (cursor-fetch cursor)
  (:anno: (db-cursor-generator) -> list)
  (let ((conn (cursor-conn cursor))
        (name (cursor-name cursor))
        (size (cursor-size cursor)))
    (DB-query conn "BEGIN;")
    (DB-query conn (format #f "FETCH ~a FROM ~a;" size name))
    (DB-query conn "COMMIT;")
    (DB-get-all-rows conn)))

(::define (return-cursor-generator cursor-name cursor-size conn)
  (:anno: (string integer db-conn) -> (db-cursor-generator))
  (make-db-cursor-generator
   cursor-name
   cursor-size
   conn))
