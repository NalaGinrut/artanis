;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014
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

(define-module (artanis artanis)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis env)
  #:use-module (artanis tpl)
  #:use-module (artanis db)
  #:use-module (artanis ssql)
  #:use-module (artanis oht)
  #:use-module (artanis route)
  #:use-module (artanis page)
  #:use-module (artanis third-party csv)
  #:use-module (artanis third-party json)
  #:use-module (web server)
  #:re-export (;; page module
               params
               response-emit
               throw-auth-needed
               tpl->html
               redirect-to
               generate-response-with-file
               emit-response-with-file
               tpl->response
               reject-method
               response-error-emit
               run-after-request!
               run-before-response!

               ;; oht module
               get
               post
               put
               patch
               page-delete
               :sql-mapping
               :str
               :conn
               :raw-sql
               :cookies
               :cache
               :cookies-set!
               :cookies-ref
               :cookies-update!
               :cookies-remove!
               :mime

               ;; db module
               DB-open
               DB-close
               DB-query
               DB-result-status
               DB-get-all-rows
               DB-get-top-row
               DB-get-n-rows
               init-DB

               ;; ssql module
               ->sql
               where
               /or
               /and

               ;; route module
               rc-handler rc-handler!
               rc-keys rc-keys!
               rc-re rc-re!
               rc-req rc-req!
               rc-path rc-path!
               rc-qt rc-qt!
               rc-method rc-method!
               rc-rhk rc-rhk!
               rc-bt rc-bt!
               rc-body rc-body!
               rc-mtime rc-mtime!
               rc-cookie rc-cookie!
               rc-set-cookie rc-set-cookie!

               ;; csv
               make-csv-reader
               csv->xml
               sxml->csv
               csv-write
               sxml->csv-string

               ;; json
               scm->json
               scm->json-string
               json->scm
               json-string->scm
               json-parser?
               json-parser-port
               json)
  #:export (init-server
            run))

(define (default-route-init)
  ;; avoid a common warn
  (get "/$" (lambda () "no index.html but it works!"))
  (get "/.+\\.(png|jpg|jpeg|ico|html|js|css)" 
   (lambda (rc) 
     (emit-response-with-file (static-filename (rc-path rc))))))

;; make sure to call init-server at the beginning
(define (init-server)
  (default-route-init)
  (init-hook)
  (init-config))

(define (check-if-not-run-init-server)
  ;; Just check if the conf table is empty
  (is-hash-table-empty? *conf-hash-table*))

;; Invalid use-db? must be (dbd username passwd) or #f
(define* (run #:key (host #f) (port #f) (debug #f) (use-db? #f)
              (dbd #f) (db-username #f) (db-passwd #f) (db-name #f))
  (when (check-if-not-run-init-server)
    (error "Sorry, but you have to run (init-server) in the begining of you main program!"))
  (format #t "Anytime you want to Quit just try Ctrl+C, thanks!~%")
  ;; Since config file was handled in (init-server), users' config can override it.
  (and host (conf-set! '(host addr) host))
  (and port (conf-set! '(host port) port))
  (when debug
    (display "DEBUG: ON\n")
    (conf-set! 'debug-mode #t))
  (when use-db?
    (conf-set! 'use-db? #t)
    (display "Users want to use Database, initializing...\n")
    (init-database-config dbd db-username db-passwd db-name)
    (init-DB)
    (display "DB init done!\n"))
  (format #t "~a~%" (current-myhost))
  (run-server
   (if debug
       (lambda (r b) (format #t "~a~%~a~%" r b) (server-handler r b))
       server-handler)
   'http `(#:host ,(get-conf '(host addr)) #:port ,(get-conf '(host port)))))
