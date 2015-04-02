;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014,2015
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
  #:use-module (artanis db)
  #:use-module (artanis ssql)
  #:use-module (artanis route)
  #:use-module (artanis fprm)
  #:use-module ((artanis page) #:select (params))
  #:use-module (artanis crypto base64)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (sql-mapping-maker
            auth-maker))

;; TODO:
;; 1. sql-mapping should be the only DB abstraction of Artanis(if any possible).
;; 2. There should be Anti SQL-Injection Mechanism (ASIM), a static analysis tool would
;;    be better(challenging).
;; 3. A DSL within ${..} string template, which provides a convinient way to let
;;    user specify ASIM options for the field passed from the client.
;; 4. DSL should handle key-value or list for users easily.
;; 5. DSL should handle Stored-Procedure for specific DB implementation (as possible).
;; 6. There's no ORM, but FPRM. The only difference is that you never see
;;    Classes but Closures. Hmm...what's the essential difference? Stateless, of course!

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
    (else (throw 'artanis-err 500 "sql-mapping-maker: Invalid mode!" mode))))

;; TODO: Should add user customerized unauth page
(define (auth-maker val rule keys)
  (define crypto identity)
  (define mode #f)
  (define passwd "passwd")
  (define username "username")
  (define-syntax-rule (->passwd rc sql)
    (assoc-ref (DB-get-top-row (DB-query (DB-open rc) sql)) passwd))
  (define post-data '())
  (define (init-post rc)
    (and (rc-body rc)
         (set! post-data (generate-kv-from-post-qstr (rc-body rc)))))
  (define (post-ref key) (and=> (assoc-ref post-data key) car))
  (define (table-checker rc sql)
    (string=? (crypto (post-ref passwd)) (->passwd rc sql)))
  (define customed-basic-checker #f)
  (define (basic-checker rc p sql)
    (format #t "~a, ~a~%" p sql)
    (string=? p (->passwd rc sql)))
  (define sql
    (match val
      (`(table ,table ,username-field ,passwd-field)
       (set! mode 'table-specified-fields)
       (set! passwd passwd-field)
       (set! username username-field)
       (lambda (u)
         (->sql select (list passwd) from table (where (string->keyword username) u))))
      (`(table ,table ,username-field ,passwd-field ,crypto-proc)
       (set! mode 'table-specified-fields)
       (set! crypto crypto-proc)
       (set! passwd passwd-field)
       (set! username username-field)
       (lambda (u)
         (->sql select (list passwd) from table (where (string->keyword username) u))))
      (`(table ,table ,crypto-proc)
       (set! mode 'table)
       (set! crypto crypto-proc)
       (lambda (u)
         (->sql select (list passwd) from table (where (string->keyword username) u))))
      (`(basic ,table ,username-field ,passwd-field)
       (set! username username-field)
       (set! passwd passwd-field)
       (set! mode 'basic)
       (lambda (u)
         (->sql select (list passwd) from table (where (string->keyword username) u))))
      (`(basic ,checker)
       (set! mode 'basic)
       (set! customed-basic-checker checker)
       #f)
      ((? string? tpl)
       (set! mode 'tpl)
       (make-db-string-template tpl))
      (else (throw 'artanis-err 500 "auth-maker: wrong pattern" val))))
  (lambda (rc . kargs)
    (define result
      (begin
        (init-post rc) ; init posted data
        (case mode
          ((table) (table-checker rc sql))
          ((table-specified-fields)
           (let ((u (post-ref username))
                 (p (post-ref passwd)))
             (format #t "~a: ~a~%" username u)
             (format #t "~a: ~a~%" passwd p)
             (and u (table-checker rc (sql u)))))
          ((tpl) (table-checker rc (apply sql kargs)))
          ((basic)
           (match (get-header rc 'authorization)
             (`(basic . ,(= base64-decode (= (cut string-split <> #\:) up)))
              (let ((u (car up)) (p (cadr up)))
                (if customed-basic-checker
                    (customed-basic-checker rc u p)
                    (basic-checker rc p (sql u)))))
             (else #f)))
          (else (throw 'artanis-err 500 "auth-maker: Fatal BUG! Invalid mode! Shouldn't be here!" mode)))))
    (if result
        (display "Auth ok!\n")
        (display "Auth failed!\n"))
    result))
