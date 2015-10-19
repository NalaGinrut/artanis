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
  #:use-module (ice-9 format)
  #:export (do-migration-create
            migration-field-add!))

;; For example:
;; (define-person
;;   (id auto (#:not-null #:primary-key))
;;   (first_name char-field (#:maxlen 30 #:not-null))
;;   (last_name char-field (#:maxlen 30 #:not-null)))
(define-syntax create-artanis-migration
  (lambda (x)
    (syntax-case x ()
      ((_ name) (identifier? #'name)
       #`(begin
           ;; NOTE: we have to encapsulate them to a module for protecting namespaces
           ;; NOTE: we're not going to imort (artanis env) directly to avoid revealing global
           ;;       env vars to users.
           (define-module (db migration name)
             #:use-module (artanis artanis)
             #:use-module (artanis utils)
             #:use-module (artanis db)
             #:use-module (artanis fprm))
           (define *migrate-handlers* '())
           (define (migrate-add! cmd handler)
             (set! *migrate-handlers*
                   (assoc-set! *migrate-handlers* cmd handler)))
           (define-syntax-rule (migrate-up body :::)
             (migrate-add! 'up (lambda () body :::)))
           (define-syntax-rule (migrate-down body :::)
             (migrate-add! 'down (lambda () body :::)))
           (define-public #,(datum->syntax #'name (symbol-append 'migrate- (syntax->datum #'name)))
             (lambda (cmd . args)
               (cond
                ((assoc-ref *migrate-handlers* cmd)
                 => (lambda (h) (apply h args)))
                (else (throw 'artanis-err 500 "Migrate: Invalid cmd!"))))))))))

(define (mg:create-table)
  #t)

(define (mg:change-table)
  #t)

(define (mg:drop-table)
  #t)

(define (mg:add-column)
  #t)

(define (mg:change-column)
  #t)

(define (mg:rename-column)
  #t)

(define (mg:remove-column)
  #t)

(define (mg:add-index)
  #t)

(define (mg:remove-index)
  #t)

(define (with-migration-file name proc)
  (let* ((t (strftime "%Y%m%d%H%M%S" (localtime (current-time))))
         (f (format #f "~a_create_" t)))
    (when (file-exists? f)
          (throw 'artanis-err 500 "Migrate: File exists?!" f))
    (call-with-output-file f proc)))

(define (gen-migration-header name)
  (call-with-output-string
   (lambda (port)
     (format port ";; Migration ~a definition of ~a~%" name (current-appname))
     (display ";; Please add your license header here.\n" port)
     (display ";; This file is generated automatically by GNU Artanis.\n" port)
     (format port "(create-artanis-migration ~a) ; DO NOT REMOVE THIS LINE!!!~%~%" name))))

(define (do-migration-create name fields port)
  (display (gen-migration-header name) port)
  (format port "(migrate-up~%~2t\"Add your up code\")~%")
  (format port "(migrate-down~%~2t\"Add your down code\")~%")
  )
