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

(define-module (artanis sql-mapping)
  #:use-module (artanis utils)
  #:use-module (artanis db)
  #:use-module (artanis ssql)
  #:use-module (artanis route)
  #:use-module (ice-9 match)
  #:export (sql-mapping-maker
            auth-maker))

;; TODO:
;; 1. sql-mapping should be the only DB abstraction of Artanis(if any possible).
;; 2. There should be Anti SQL-Injection Mechanism (ASIM), a static analysis tool would
;;    be better(challengeable).
;; 3. A DSL within ${..} string template, which provides a convinient way to let
;;    user specify ASIM options for the field passed from the client.
;; 4. DSL should handle key-value or list for users easily.
;; 5. DSL should handle Stored-Procedure for specific DB implementation (as possible).
;; 6. There's no fucking ORM, definitly.

(define (sql-mapping-maker sql-tpl rule keys)
  #f)
;; (define (sql-mapping-maker sql-tpl rule keys)
;;   (define tpl (make-db-string-template sql-tpl))
;;   (define rkey 
;;     (map (lambda (k) (string->keyword (string-concatenate ":" k))) keys))
;;   (lambda (rc . kargs)
;;     (let ((bt (rc-bt rc)) ; binding-table of keys in rule
;;           (sql (apply tpl (append (list (alist->kblist bt) kargs)))))
;;       ;; TODO
;;       #t)))
     
(define (auth-maker val rule keys)
  #f)
;; ;; TODO: Should add user customerized unauth page
;; (define (auth-maker val rule keys)
;;   (define sql
;;     (match val
;;       ((table username passwd)
;;        (->sql select passwd from table where `(= usrname ,username)))
;;       ((? string? tpl)
;;        (make-db-string-template tpl))
;;       (else (error auth-maker "wrong pattern"))))
;;   (define tpl (make-db-string-template sql-tpl))
;;   (lambda (rc . kargs)
;;     (let ((bt (rc-bt rc)))
;;       #t
;;       ;; TODO
;;       )))
