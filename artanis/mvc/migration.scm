;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015,2017,2018,2019,2022,2023
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
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
            remove-index
            set-row))

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
             (set! #,(datum->syntax x '*migrate-handlers*)
                   (assoc-set! #,(datum->syntax x '*migrate-handlers*) cmd handler)))
           (define-syntax #,(datum->syntax x 'migrate-create)
             (syntax-rules ::: ()
                           ((_ do-migrate-create :::)
                            (#,(datum->syntax x 'migrate-add!)
                             'create (lambda () do-migrate-create :::)))))
           (define-syntax #,(datum->syntax x 'migrate-up)
             (syntax-rules ::: ()
                           ((_ do-migrate-up :::)
                            (#,(datum->syntax x 'migrate-add!)
                             'up (lambda () do-migrate-up :::)))))
           (define-syntax #,(datum->syntax x 'migrate-down)
             (syntax-rules ::: ()
                           ((_ do-migrate-down :::)
                            (#,(datum->syntax x 'migrate-add!)
                             'down (lambda () do-migrate-down :::)))))
           (define-syntax #,(datum->syntax x 'migrate-change)
             (syntax-rules ::: ()
                           ((_ do-migrate-change :::)
                            (#,(datum->syntax x 'migrate-add!)
                             'change (lambda () do-migrate-change :::)))))
           (define-public #,(datum->syntax x 'migrator)
             (lambda (cmd . args)
               (cond
                ((assoc-ref #,(datum->syntax x '*migrate-handlers*) cmd)
                 => (lambda (h) (h)))
                (else
                 (format (current-error-port) "[HINT] Did you remove `migrate-~a'?~%" cmd)
                 (throw 'artanis-err 500 #,(datum->syntax x 'migrator)
                        "Migrate: Invalid cmd `~a'!"
                        cmd))))))))))

(define (create-table name . fl)
  (let* ((raw ((@@ (artanis mvc model) parse-raw-fields) fl))
         (conn (get-conn-from-pool!))
         (mt (map-table-from-DB conn)))
    (format (artanis-current-output) "Creating table `~a'......" name)
    (apply mt 'try-create name raw)
    (recycle-DB-conn conn)
    (format (artanis-current-output) "Regenerating migration cache......")
    (display "DONE.\n" (artanis-current-output))))

(define (change-table name cl)
  (let* ((conn (get-conn-from-pool!))
         (mt (map-table-from-DB conn)))
    (format (artanis-current-output) "Changing table `~a'......" name)
    (mt 'mod 'alter name cl)
    (recycle-DB-conn conn)
    (display "DONE.\n" (artanis-current-output))))

(define (drop-table name)
  (let* ((conn (get-conn-from-pool!))
         (mt (map-table-from-DB conn)))
    (format (artanis-current-output) "Dropping table `~a'.........." name)
    (mt 'drop name)
    (recycle-DB-conn conn)
    (display "DONE.\n" (artanis-current-output))))

(define (rename-table old new)
  (let* ((conn (get-conn-from-pool!))
         (mt (map-table-from-DB conn)))
    (format (artanis-current-output)
            "Renaming table `~a' to `~a'.........." old new)
    (mt 'mod 'rename old new)
    (recycle-DB-conn conn)
    (display "DONE.\n" (artanis-current-output))))

(define (add-column tname . cl)
  (let* ((conn (get-conn-from-pool!))
         (mt (map-table-from-DB conn)))
    (format (artanis-current-output)
            "Adding columns `(~{~a~^,~})' to table `~a'.........."
            (map car cl) tname)
    (apply mt 'mod tname 'add cl)
    (recycle-DB-conn conn)
    (display "DONE.\n" (artanis-current-output))))

(define (change-column tname col type)
  (let* ((conn (get-conn-from-pool!))
         (mt (map-table-from-DB conn)))
    (format (artanis-current-output)
            "Changing column `~a' of table `~a'.........."
            (list col type) tname)
    (mt 'mod 'alter tname col type)
    (recycle-DB-conn conn)
    (display "DONE.\n" (artanis-current-output))))

(define (rename-column tname oldcol newcol . type)
  (let* ((conn (get-conn-from-pool!))
         (mt (map-table-from-DB conn)))
    (format (artanis-current-output)
            "Renaming column `~a' to `~a' from `~a'.........."
            oldcol newcol tname)
    (apply mt 'mod 'alter tname oldcol newcol type)
    (recycle-DB-conn conn)
    (display "DONE.\n" (artanis-current-output))))

(define (remove-column tname cname)
  (let* ((conn (get-conn-from-pool!))
         (mt (map-table-from-DB conn)))
    (format (artanis-current-output)
            "Removing column `~a' in table `~a'.........."
            cname tname)
    (apply mt 'mod 'column-drop tname cname)
    (recycle-DB-conn conn)
    (display "DONE.\n" (artanis-current-output))))

(define (set-row tname . fl)
  (let* ((conn (get-conn-from-pool!))
         (mt (map-table-from-DB conn)))
    (format (artanis-current-output) "Setting row of table `~a'......" tname)
    (apply mt 'set tname fl)
    (recycle-DB-conn conn)
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
