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
  #:use-module (srfi srfi-1)
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
               :cookies-setattr!
               :mime
               :auth
               :session
               :from-post

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
               get-header

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

;; When you don't want to use cache, use static-page-emitter.
(define (static-page-emitter rc)
  (emit-response-with-file (static-filename (rc-path rc))))

(define (default-route-init statics cache-statics? exclude)
  ;; avoid a common warn
  (get "/$" (lambda () "no index.html but it works!"))
  (let ((srule (format #f "/.+\\.(~{~a~^|~})" (lset-difference eq? statics exclude))))
    (if cache-statics?
        (get srule #:cache 'static (lambda (rc) (:cache rc)))
        (get srule static-page-emitter))))

;; make sure to call init-server at the beginning
(define* (init-server #:key (statics '(png jpg jpeg ico html js css))
                      (cache-statics? #f) (exclude '()))
  (default-route-init statics cache-statics? exclude)
  (init-hook)
  (init-config))

(define (check-if-not-run-init-server)
  ;; Just check if the conf table is empty
  (is-hash-table-empty? *conf-hash-table*))

;; Invalid use-db? must be (dbd username passwd) or #f
(define* (run #:key (host #f) (port #f) (debug #f) (use-db? #f)
              (dbd #f) (db-username #f) (db-passwd #f) (db-name #f))
  (define (->proper-body-display body)
    (cond
     ((not body) "No body in the request!")
     ((string? body)
      (if debug
          body
          (substring body
                     (and=> (string-lengh body)
                            (lambda (len) (if (> len 100) 100 len))))))
     (((@ (rnrs bytevectors) bytevector?) body) "Body is bytevectors!")
     (else (throw 'artanis-err 500 "->proper-body-display: Invalid body type!" body))))
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
       (lambda (r b)
         (format #t "[Request] ~a~%[Body] ~a~%" r (->proper-body-display b))
         (server-handler r b))
       server-handler)
   'http `(#:host ,(get-conf '(host addr)) #:port ,(get-conf '(host port)))))
