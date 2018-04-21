;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014,2015,2017,2018
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
  #:use-module (artanis env)
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
    (else (throw 'artanis-err 500 sql-mapping-maker "Invalid mode!" mode))))

(define (auth-maker val rule keys)
  (define-syntax-rule (->passwd rc passwd-field sql)
    (assoc-ref (DB-get-top-row (DB-query (DB-open rc) sql)) passwd-field))
  (define-syntax-rule (post-ref post-data key) (and=> (assoc-ref post-data key) car))
  (define (basic-checker rc p sql passwd-field)
    (DEBUG "basic auth:  ~a, ~a~%" p sql)
    (string=? p (->passwd rc passwd-field sql)))
  (define (init-post rc)
    (and (rc-body rc)
         (generate-kv-from-post-qstr (rc-body rc))))
  (define* (gen-result rc mode sql post-data
                       #:key (crypto identity) (checker #f)
                       (passwd-field "passwd"))
    (define (table-checker)
      (let ((pw (->passwd rc passwd-field sql)))
        (and pw (string=? (crypto (post-ref post-data passwd-field)) pw))))
    (case mode
      ((table) (checker))
      ((table-specified-fields) (checker))
      ((tpl) (checker))
      ((basic)
       (match (get-header rc 'authorization)
         ;; NOTE: In match `=' opetator, the order of evaluation is from left to right.
         ;;       So base64-decode will run first.
         (`(basic . ,(= base64-decode (= (cut string-split <> #\:) up)))
          (let ((u (car up)) (p (cadr up)))
            (if checker
                (checker rc u p)
                (basic-checker rc p (sql u) passwd-field))))
         (else #f)))
      (else (throw 'artanis-err 500 auth-maker
                   "Fatal BUG! Invalid mode! Shouldn't be here!" mode))))
  (lambda (rc . kargs)
    (let ((post-data (init-post rc)))
      (match val
        (`(table ,table ,username-field ,passwd-field)
         (let* ((username (post-ref post-data username-field))
                (passwd (post-ref post-data passwd-field))
                (sql (->sql select '("passwd") from table
                            (where (string->keyword username-field)
                                   username))))
           (gen-result rc 'table-specified-fields sql post-data
                       #:passwd-field passwd-field)))
        (`(table ,table ,username-field ,passwd-field ,crypto-proc)
         (let* ((username (post-ref post-data username-field))
                (passwd (post-ref post-data passwd-field))
                (sql (->sql select '("passwd") from table
                            (where (string->keyword username-field)
                                   username))))
           (gen-result rc 'table-specified-fields sql post-data
                       #:crypto crypto-proc #:passwd-field passwd-field)))
        (`(table ,table ,crypto-proc)
         (let* ((username (car kargs))
                (passwd (cadr kargs))
                (sql (->sql select '("passwd") from table
                            (where #:username username))))
           (gen-result rc 'table sql post-data #:crypto crypto-proc)))
        (`(basic ,table ,username-field ,passwd-field)
         (let* ((username (post-ref post-data username-field))
                (passwd (post-ref post-data passwd-field))
                (sql (->sql select (list passwd) from table
                            (where (string->keyword username-field)
                                   username))))
           (gen-result rc 'basic sql post-data #:passwd-field passwd-field)))
        (`(basic ,table ,checker)
         (gen-result rc 'basic #f post-data #:checker checker))
        ((? string? tpl)
         (make-db-string-template tpl))
        (else (throw 'artanis-err 500 auth-maker "Wrong pattern ~a" val))))))
