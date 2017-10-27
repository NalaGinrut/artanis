;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015,2017
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
    (syntax-case x (migrate-create)
      ((_ name) (identifier? #'name)
       #`(begin
           ;; NOTE: we have to encapsulate them to a module for protecting namespaces
           ;; NOTE: we're not going to imort (artanis env) directly to avoid revealing global
           ;;       env vars to users.
           (define-module (db migration name)
             #:use-module (artanis mvc migration)
             #:use-module (artanis artanis)
             #:use-module (artanis config)
             #:use-module (artanis utils)
             #:use-module (artanis db)
             #:use-module (artanis fprm))
           (define #,(datum->syntax x '*migrate-handlers*) '())
           (define (#,(datum->syntax x 'migrate-add!) cmd handler)
             (set! *migrate-handlers*
               (assoc-set! *migrate-handlers* cmd handler)))
           (define-syntax #,(datum->syntax x 'migrate-create)
               (syntax-rules ::: ()
                 ((_ do-migrate-create :::)
                  (migrate-add! 'create (lambda () do-migrate-create :::)))))
           (define-syntax #,(datum->syntax x 'migrate-up)
             (syntax-rules ::: ()
               ((_ do-migrate-up :::)
                (migrate-add! 'up (lambda () do-migrate-up :::)))))
           (define-syntax #,(datum->syntax x 'migrate-down)
             (syntax-rules ::: ()
               ((_ do-migrate-down :::)
                (migrate-add! 'down (lambda () do-migrate-down :::)))))
           (define-syntax #,(datum->syntax x 'migrate-change)
             (syntax-rules ::: ()
               ((_ do-migrate-change :::)
                (migrate-add! 'change (lambda () do-migrate-change :::)))))
           (define-public #,(datum->syntax x 'migrator)
             (lambda (cmd . args)
               (cond
                ((assoc-ref *migrate-handlers* cmd)
                 => (lambda (h) (h)))
                (else (throw 'artanis-err 500 migrator
                             "Migrate: Invalid cmd `~a'!" cmd))))))))))

(define (create-table name . fl)
  (let ((raw ((@@ (artanis mvc model) parse-raw-fields) fl))
        (mt (map-table-from-DB (get-conn-from-pool))))
    (format #t "Creating table `~a'......" name)
    (mt 'try-create name raw)
    (format (artanis-current-output) "Regenerating migration cache......")
    (flush-to-migration-cache name fl)
    (display "DONE.\n")))

(define (change-table name cl)
  (let ((mt (map-table-from-DB (get-conn-from-pool))))
    (format (artanis-current-output) "Changing table `~a'......" name)
    (mt 'mod 'alter name cl)
    (display "DONE.\n" (artanis-current-output))))

(define (drop-table name)
  (let ((mt (map-table-from-DB (get-conn-from-pool))))
    (format (artanis-current-output) "Dropping table `~a'.........." name)
    (mt 'drop name)
    (display "DONE.\n" (artanis-current-output))))

(define (rename-table old new)
  (let ((mt (map-table-from-DB (get-conn-from-pool))))
    (format (artanis-current-output)
            "Renaming table `~a' to `~a'.........." old new)
    (mt 'mod 'rename old new)
    (display "DONE.\n" (artanis-current-output))))
  
(define (add-column tname . cl)
  (let ((mt (map-table-from-DB (get-conn-from-pool))))
    (format (artanis-current-output)
            "Adding columns `(~{~a~^,~})' to table `~a'.........." 
            (map car cl) tname)
    (apply mt 'mod 'add tname cl)
    (display "DONE.\n" (artanis-current-output))))

(define (change-column tname col type)
  (let ((mt (map-table-from-DB (get-conn-from-pool))))
    (format (artanis-current-output)
            "Changing column `~a' of table `~a'.........."
            (list col type) tname)
    (mt 'mod 'alter tname col type)
    (display "DONE.\n" (artanis-current-output))))

(define (rename-column tname oldcol newcol . type)
  (let ((mt (map-table-from-DB (get-conn-from-pool))))
    (format (artanis-current-output)
            "Renaming column `~a' to `~a' from `~a'.........."
            oldcol newcol tname)
    (apply mt 'mod 'alter tname oldcol newcol type)
    (display "DONE.\n" (artanis-current-output))))

(define (remove-column tname cname)
  (let ((mt (map-table-from-DB (get-conn-from-pool))))
    (format (artanis-current-output)
            "Removing column `~a' in table `~a'.........."
            cname tname)
    (apply mt 'mod 'column-drop tname cname)
    (display "DONE.\n" (artanis-current-output))))

(define (add-index)
  ;; TODO
  #t)

(define (remove-index)
  ;; TODO
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
  (format port "(migrate-create~%~2t(display \"Add your create DB code\\n\"))~%")
  (format port "(migrate-up~%~2t(display \"Add your up code\\n\"))~%")
  (format port "(migrate-down~%~2t(display \"Add your down code\\n\"))~%"))
