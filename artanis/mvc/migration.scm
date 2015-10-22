;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015
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

(define-module (artanis mvc migration)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis irregex)
  #:use-module (artanis fprm)
  #:use-module (artanis db)
  #:use-module (ice-9 format)
  #:export (create-artanis-migration
            do-migration-create
            migration-field-add!
            create-table
            change-table
            drop-table
            add-column
            change-column
            rename-column
            remove-column
            add-index
            remove-index))

(define-syntax create-artanis-migration
  (lambda (x)
    (syntax-case x ()
      ((_ name) (identifier? #'name)
       #`(begin
           ;; NOTE: we have to encapsulate them to a module for protecting namespaces
           ;; NOTE: we're not going to imort (artanis env) directly to avoid revealing global
           ;;       env vars to users.
           (define-module (db migration name)
             #:use-module (artanis mvc migration)
             #:use-module (artanis artanis)
             #:use-module (artanis utils)
             #:use-module (artanis db)
             #:use-module (artanis fprm))
           (define *migrate-handlers* '())
           (define (migrate-add! cmd handler)
             (set! *migrate-handlers*
                   (assoc-set! *migrate-handlers* cmd handler)))
           (define-syntax migrate-up
             (syntax-rules ::: ()
               ((_ body :::)
                (migrate-add! 'up (lambda () body :::)))))
           (define-syntax migrate-down
             (syntax-rules ::: ()
               ((_ body :::)
                (migrate-add! 'down (lambda () body :::)))))
           (define-public migrator
             (lambda (cmd . args)
               (cond
                ((assoc-ref *migrate-handlers* cmd)
                 => (lambda (h) (h)))
                (else (throw 'artanis-err 500 "Migrate: Invalid cmd!"))))))))))

(define (create-table name . fl)
  (let ((raw ((@@ (artanis mvc model) parse-raw-fields) fl))
        (mt (map-table-from-DB (get-conn-from-pool 0))))
    (format #t "Creating table `~a'............." name)
    (mt 'create name raw)
    (flush-to-migration-cache name fl)
    (display "done.\n")))

;; TODO: how to change multi columns elegantly?
(define (change-table name . cl)     
  #t)

(define (drop-table)
  #t)

(define (add-column)
  #t)

(define (change-column)
  #t)

(define (rename-column)
  #t)

(define (remove-column)
  #t)

(define (add-index)
  #t)

(define (remove-index)
  #t)

(define (gen-migration-header name)
  (call-with-output-string
   (lambda (port)
     (format port ";; Migration ~a definition of ~a~%" name (current-appname))
     (display ";; Please add your license header here.\n" port)
     (display ";; This file is generated automatically by GNU Artanis.\n" port)
     (format port "(create-artanis-migration ~a) ; DO NOT REMOVE THIS LINE!!!~%~%" name))))

(define (do-migration-create name fields port)
  (display (gen-migration-header (gen-migrate-module-name (port-filename port))) port)
  (format port "(migrate-up~%~2t(display \"Add your up code\\n\"))~%")
  (format port "(migrate-down~%~2t(display \"Add your down code\\n\"))~%"))
