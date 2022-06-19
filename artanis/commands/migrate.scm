;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015,2016,2019,2022
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

(define-module (artanis commands migrate)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis commands)
  #:use-module (artanis irregex)
  #:use-module (artanis config)
  #:use-module (artanis db)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-1))

(define (print-options)
  (display "\nOPTIONS:\n")
  (format #t "~2t--all~%")
  (format #t "~2tVERSION=version~%")
  (format #t "~2t--debug~%"))

(define *operators* '(up down create))

(define (print-operators)
  (display "\nOperators:\n")
  (for-each (lambda (op) (format #t "~2t~a~%" op)) *operators*))

(define (show-help)
  (display announce-head)
  (display "\nUsage:\n  art migrate operator name [OPTIONS]\n")
  (print-operators)
  (print-options)
  (display announce-foot))

(define *verstr-re* (string->irregex "VERSION=(\\d{14})"))
(define *debug-re* (string->irregex "--debug"))

;; TODO: add more options
(define (opts-parser opts)
  (let lp((next opts) (ret '()))
    (cond
     ((null? next) ret)
     ((irregex-search *debug-re* (car next))
      => (lambda (m)
           (conf-set! 'debug-mode #t)
           (lp (cdr next) ret)))
     ((irregex-search *verstr-re* (car next))
      => (lambda (m)
           (lp (cdr next)
               (cons (cons 'version (irregex-match-substring m 1)) ret))))
     (else (lp (cdr next) ret)))))

(define (%migrate op name opts)
  (define *mfile-re*
    (string->irregex (format #f "^(~a)_(\\d{14})\\.scm$" name)))
  (define (compare-mfile x y)
    (let ((mx (irregex-match-substring (irregex-search *mfile-re* x) 2))
          (my (irregex-match-substring (irregex-search *mfile-re* y) 2)))
      (>= (string->number mx) (string->number my))))
  (define (is-mfile s) (irregex-search *mfile-re* s))
  (define path (format #f "~a/db/migration" (current-toplevel)))
  (define (gen-migrate-file)
    (cond
     ((assoc-ref opts 'version)
      => (lambda (v) (format #f "~a/~a_~a.scm" path name v)))
     (else
      (let ((fl (scandir path is-mfile)))
        (match (sort fl compare-mfile)
          (() (format #t "Migration: No migrations of `~a' were found!~%" name) #f)
          ((f . rest) (format #f "~a/~a" path f))
          (else (throw 'artanis-err 500 %migrate "Unknown error!")))))))
  (define (handle-one-migrate f)
    (format #t "[Migrating ~a]~%" (basename f))
    (use-modules (artanis mvc migration)) ; trick to make migrator happy
    (load f)
    (let ((m (resolve-module
              `(db migration ,(string->symbol (gen-migrate-module-name f))))))
      ((module-ref m 'migrator) op)))
  (define (find-all-latest-migration)
    (define re (string->irregex  "^([^_]+)_(\\d{14})\\.scm$"))
    (define (is-valid-mfile? f)
      (irregex-match re f))
    (let ((fl (sort (scandir path is-valid-mfile?) string-ci<?)))
      (fold (lambda (f p)
              (cond
               ((irregex-match re f)
                => (lambda (m)
                     (let ((mname (irregex-match-substring m 1))
                           (timestamp (string->number (irregex-match-substring m 2))))
                       (match p
                         (() (cons (cons mname timestamp) p))
                         (((mname-last . timestamp-last) rest ...)
                          (cond
                           ((string=? mname mname-last)
                            (if (> timestamp timestamp-last)
                                (cons (cons mname timestamp) (cdr p))
                                p))
                           (else
                            (cons (cons mname timestamp) p))))))))

               (else p)))
            '()
            fl)))
  (define (gen-migrate)
    (match name
      ("--all" (find-all-latest-migration))
      (else (gen-migrate-file))))
  (let ((f (gen-migrate)))
    (cond
     ((string? f) (handle-one-migrate f))
     ((list? f)
      (for-each handle-one-migrate f))
     (else (throw 'artanis-err 500 %migrate
                  "Invalid migration name `~a'!"
                  f)))))

(define (valid-operator? op)
  (case (string->symbol op)
    ((up down create) #t)
    (else #f)))

(define (show-operator-str)
  (for-each (lambda (op) (display op) (display " ")) *operators*))

(define (show-options-str)
  (display "VERSION= --debug\n"))

(define (show-scandir-str)
  (define (filter_fun k)
    (cond ((irregex-search "(.*)_[0-9]+[.]scm" k) => (lambda (m) (irregex-match-substring m 1)))
          (else #f)))

  (let* ((scandir_list (scandir "db/migration"))
         (filter_list (filter-map filter_fun scandir_list))
         (list_to_hash (alist->hash-table (map (lambda(x) (cons x x)) filter_list))))
    (hash-for-each (lambda (k v) (display k) (newline)) list_to_hash)
    (display "--all\n")))

(define (do-migrate . args)
  (define (validname? x)
    (irregex-search "^-.*" x))
  (match args
    (("migrate" "--operators") (show-operator-str))
    (("migrate" "--options-list") (show-options-str))
    (("migrate" "--scandir-list") (show-scandir-str))
    (("migrate" (or () (? validname?) "help" "--help" "-help" "-h")) (show-help))
    (("migrate" (? valid-operator? op) name . opts)
     (add-to-load-path (current-toplevel))
     (parameterize ((current-conf-file (gen-local-conf-file)))
       (init-config)) ; needs to load config
     ((@@ (artanis config) init-inner-database-item))
     (init-DB) ; needs to use DB
     (%migrate (string->symbol op) (string->symbol name) (opts-parser opts)))
    (else (show-help))))

(define %summary "DB migration tools.")
(define main do-migrate)
