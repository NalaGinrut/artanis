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

(define-module (artanis sql-mapping fetcher)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis db)
  #:use-module ((rnrs) #:select (define-record-type get-string-all))
  #:use-module (ice-9 rdelim)
  #:export (sql-mapping-fetch
            sql-mapping-add-from-path
            sql-mapping-tpl-add))

(define-record-type <sql-mapping> (fields type name path sm))

(define sm-ref hash-ref)
(define sm-set! hash-set!)
(define-syntax-rule (sql-mapping-ref name)
  (and=> (sm-ref *sql-mapping-lookup-table* name) <sql-mapping>-sm))

(define (sql-mapping-fetch rc name . kargs)
  (let ((sm (sql-mapping-ref name))
        (conn (DB-open rc)))
    (DB-query conn (apply sm kargs))))

(define (sql-mapping-tpl-add name tpl)
  (sm-set! *sql-mapping-lookup-table*
           name
           (make-<sql-mapping> 'str name #f (make-db-string-template tpl))))

(define (sql-mapping-add-from-path path name)
  (sm-set! *sql-mapping-lookup-table*
           name
           (make-<sql-mapping> 'file name path
                                (read-sql-mapping-from-file path name))))

;; The grammar of sql-mapping DSL:
;; e.g:
;; define mmr;
;;
;; options:
;;         check-all = false;
;;         all <- nodash;
;;         $passwd <- nodash;
;;
;; sql-mapping:
;;         select username,info,addr,email from Persons where passwd=${passwd}

(define *delimiters* "\n=<:;")
(define *delim-set* (string->char-set *delimiters*))

(define (get-token port)
  (and=> (read-delimited *delimiters* port) string-trim-both))

(define (sm-define port)
  `(define ,(get-token port)))

(define (sm-options-get port)
  (define (skip-endline)
    (read-char port) ; skip #\;
    (let lp ()
      (cond
       ((eof-object? (peek-char port))
        (throw 'artanis-err 500 "skip-endline: wrong syntax when skipping endline!")) 
       ((char-set-contains? char-set:whitespace (peek-char port))
        (read-char port) ; skip whitespace
        (lp)))))
  (let lp((tk (get-token port)) (opt '()) (constrain '()))
    (cond
     ((eof-object? tk)
      (throw 'artanis-err 500 "sm-options-get: wrong syntax since you don't specify enough fields!"))
     ((string=? "" tk) ; end
      (read-char port) ; skip #\np
      `((opt ,opt) (constrain ,constrain)))
     ((char=? #\= (peek-char port))
      (read-char port) ; skip #\=
      (let ((tkv (get-token port)))
        (and (char=? #\; (peek-char port)) (skip-endline)) ; skip #\; #\np
        (lp (get-token port) (cons (cons tk tkv) opt) constrain)))
     ((char=? #\< (peek-char port))
      (read-char port) ; skip #\<
      (and=> (read-char port) ; skip #\-
             (lambda (c) (if (char=? #\- c) #t (throw 'artanis-err 500 "sm-options-get: wrong syntax here!" c))))
      (let ((tkv (get-token port)))
        (and (char=? #\; (peek-char port)) (skip-endline)) ; skip #\; #\np
        (lp (get-token port) opt (cons (cons tk tkv) constrain))))
     (else (throw 'artanis-err 500 "sm-options-get: Fatal! Shouldn't be here!" (peek-char port))))))

;; NOTE: semi-colon shouldn't be delimiter in sql string anyway. So we'll use get-string-all.
(define (sm-get port)
  (let ((sm-str (get-string-all port)))
    (cond
     ((eof-object? sm-str)
      (throw 'artanis-err 500 "sm-get: wrong sytax, you must spacify a sql-mapping string!" sm-str))
     (else (make-db-string-template (string-trim-both sm-str))))))

(define *tk-handlers*
  `(("define" . ,sm-define)
    ("options" . ,sm-options-get)
    ("sql-mapping" . ,sm-get)))

(define (sql-mapping-parser port)
  (let lp((c (peek-char port)) (ret '()))
    (cond
     ((eof-object? c) ret)
     ((char-set-contains? char-set:whitespace c)
      (read-char port) ; skip whitespace
      (lp (peek-char port) ret))
     ((char-set-contains? *delim-set* c)
      (read-char port) ; skip
      (and (char=? #\< c)
           (char=? #\- (peek-char port))
           (read-char port)) ; skip #\- after #\<
      (lp (peek-char port) ret))
     ((get-token port)
      => (lambda (tk)
           (let ((h (assoc-ref *tk-handlers* tk)))
             (if h
                 (lp (peek-char port) (cons (h port) ret))
                 (throw 'artanis-err 500 "sql-mapping-parser: Invalid token" tk)))))
     (else (throw 'artanis-err 500 "sql-mapping-parser: Wrong syntax" (get-string-all port))))))

(define (read-sql-mapping-from-file path name)
  (when (not (file-exists? path))
    (throw 'artanis-err 500 "read-sql-mapping-from-file: file doens't exist!" path))
  (let ((sml (call-with-input-file path sql-mapping-parser)))
    ;; TODO: how to connect options to str template??
    #t))
