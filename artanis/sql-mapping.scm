;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014,2015,2017,2018,2019
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

(define-module (artanis sql-mapping)
  #:use-module (artanis sql-mapping fetcher)
  #:use-module (artanis utils)
  #:use-module (ice-9 match)
  #:export (sql-mapping-maker))

;; TODO:
;; 1. sql-mapping should be the only DB abstraction of Artanis(if any possible).
;; 2. There should be Anti SQL-Injection Mechanism (ASIM), a static analysis tool would
;;    be better(challenging).
;; 3. A DSL within ${..} string template, which provides a convinient way to let
;;    user specify ASIM options for the field passed from the client.
;; 4. DSL should handle key-value or list for users easily.
;; 5. DSL should handle Stored-Procedure for specific DB implementation (as possible).
;; 6. There's no ORM, but FPRM. The only difference is that you never see
;;    Classes but Closures. Hmm...what's the essential difference?
;;    We should try to be stateless, it's still experimental.

;; NOTE: sql-mapping-maker returns conn object, users have to get certain rows as will.
(define (sql-mapping-maker mode rule keys)
  (define (path->str path) (string-join (map object->string path) "/"))
  (match mode
    (#t sql-mapping-fetch)
    (`(path ,path ,name)
     (sql-mapping-add-from-path (path->str path) name)
     sql-mapping-fetch)
    (`(add ,name ,sql-tpl)
     (sql-mapping-tpl-add name sql-tpl)
     sql-mapping-fetch)
    ;;('fprm map-table-from-DB)
    (else (throw 'artanis-err 500 sql-mapping-maker "Invalid mode `~a'!" mode))))
