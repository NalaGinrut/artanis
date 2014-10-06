;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014
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

(define-module (artanis sql-mapping reader)
  #:use-module (artanis utils)
  #:export (read-sql-mapping-from-file))

;; The grammar of sql-mapping DSL:
;; e.g:
;; define mmr;
;;
;; options:
;;         check-all = false;
;;         all <- nodash;
;;         $passwd <- nodash;

;; sql-mapping: 
;;         select username,info,addr,email from Persons where passwd=${passwd};


(define (sql-mapping-parser port)
  #t)
  
(define (read-sql-mapping-from-file path name)
  (when (not (file-exists? path))
    (throw 'artanis-err 500 "read-sql-mapping-from-file: file doens't exist!" path))
  (call-with-input-file path sql-mapping-parser))

