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
  #:use-module (srfi srfi-11)
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
    (else (throw 'artanis-err 500 sql-mapping-maker "Invalid mode `~a'!" mode))))

(define (auth-maker val rule keys)
  (define-syntax-rule (->passwd rc passwd-field salt-field sql)
    (let ((ret (DB-get-top-row (DB-query (DB-open rc) sql))))
      (values (assoc-ref ret passwd-field)
              (assoc-ref ret salt-field))))
  (define-syntax-rule (post-ref post-data key)
    (let ((ret (assoc-ref post-data key)))
      (if ret
          (car ret)
          "")))
  (define (basic-checker rc p sql passwd-field salt-field)
    (string=? p (->passwd rc passwd-field salt-field sql)))
  (define (init-post rc)
    (DEBUG "post-data: ~a~%" (rc-body rc))
    (and (rc-body rc)
         (generate-kv-from-post-qstr (rc-body rc))))
  (define (default-hmac pw salt)
    ;; We still have sha384, sha512 to use, but I think sha256 is enough.
    (string->sha-256 (string-append pw salt)))
  (define* (gen-result rc mode sql post-data
                       #:key (hmac default-hmac) (checker #f)
                       (passwd-field "passwd") (salt-field "salt"))
    (define (table-checker)
      (let-values (((pw salt) (->passwd rc passwd-field salt-field sql)))
        (string=? (hmac (post-ref post-data passwd-field)
                        salt)
                  pw)))
    (define (run-checker)
      (if checker (checker) (table-checker)))
    (case mode
      ((table) (run-checker))
      ((table-specified-fields) (run-checker))
      ((tpl) (run-checker))
      ((basic)
       (match (get-header rc 'authorization)
         ;; NOTE: In match `=' opetator, the order of evaluation is from left to right.
         ;;       So base64-decode will run first.
         (`(basic . ,(= base64-decode-as-string (= (cut string-split <> #\:) up)))
          (let ((u (car up)) (p (cadr up)))
            (if checker
                (checker rc u p)
                (basic-checker rc p (sql u) passwd-field salt-field))))
         (else #f)))
      (else (throw 'artanis-err 500 auth-maker
                   "Fatal BUG! Invalid mode! Shouldn't be here!" mode))))
  (match val
    (`(table ,table ,username-field ,passwd-field)
     (lambda (rc . kargs)
       (let* ((post-data (init-post rc))
              (username (post-ref post-data username-field))
              (passwd (post-ref post-data passwd-field))
              (sql (->sql select `(,passwd-field salt) from table
                          (where (string->keyword username-field)
                                 username))))
         (gen-result rc 'table-specified-fields sql post-data
                     #:passwd-field passwd-field))))
    (`(table ,table ,username-field ,passwd-field ,salt-field ,hmac)
     (lambda (rc . kargs)
       (let* ((post-data (init-post rc))
              (username (post-ref post-data username-field))
              (passwd (post-ref post-data passwd-field))
              (sql (->sql select (list passwd-field salt-field) from table
                          (where (string->keyword username-field)
                                 username))))
         (gen-result rc 'table-specified-fields sql post-data
                     #:hmac hmac #:passwd-field passwd-field
                     #:salt-field salt-field))))
    (`(table ,table ,username-field ,passwd-field ,hmac)
     (lambda (rc . kargs)
       (let* ((post-data (init-post rc))
              (username (post-ref post-data username-field))
              (passwd (post-ref post-data passwd-field))
              (sql (->sql select `(,passwd-field salt) from table
                          (where (string->keyword username-field)
                                 username))))
         (gen-result rc 'table-specified-fields sql post-data
                     #:hmac hmac #:passwd-field passwd-field))))
    (`(table ,table ,hmac)
     (lambda (rc . kargs)
       (let* ((post-data (init-post rc))
              (username (car kargs))
              (passwd (cadr kargs))
              (sql (->sql select '(passwd salt) from table
                          (where #:username username))))
         (gen-result rc 'table sql post-data #:hmac hmac))))
    (`(basic ,table ,username-field ,passwd-field)
     (lambda (rc . kargs)
       (let* ((post-data (init-post rc))
              (username (post-ref post-data username-field))
              (passwd (post-ref post-data passwd-field))
              (sql (->sql select `(,passwd-field salt) from table
                          (where (string->keyword username-field)
                                 username))))
         (gen-result rc 'basic sql post-data #:passwd-field passwd-field))))
    (`(basic ,checker)
     (lambda (rc)
       (gen-result rc 'basic #f #f #:checker checker)))
    ((? string? tpl)
     (lambda _
       (make-db-string-template tpl)))
    (else (throw 'artanis-err 500 auth-maker "Wrong pattern ~a" val))))
