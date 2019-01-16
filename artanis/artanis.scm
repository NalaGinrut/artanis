;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2016,2017,2018
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

(define-module (artanis artanis)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis env)
  #:use-module (artanis tpl)
  #:use-module (artanis db)
  #:use-module (artanis fprm)
  #:use-module (artanis ssql)
  #:use-module (artanis session)
  #:use-module (artanis oht)
  #:use-module (artanis route)
  #:use-module (artanis page)
  #:use-module (artanis upload)
  #:use-module (artanis debug)
  #:use-module (artanis third-party csv)
  #:use-module (artanis third-party json)
  #:use-module (artanis server)
  #:use-module (artanis websocket)
  #:use-module (artanis lpc)
  #:use-module (artanis version)
  #:use-module (web server)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:re-export (;; page module
               params
               response-emit
               throw-auth-needed
               tpl->html
               redirect-to
               tpl->response
               reject-method
               response-error-emit
               run-after-request!
               run-before-response!
               run-when-sigint!
               emit-response-with-file
               static-page-emitter

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
               :websocket
               :lpc

               ;; db module
               DB-open
               DB-close
               DB-query
               DB-result-status
               DB-get-all-rows
               DB-get-top-row
               DB-get-n-rows
               db-conn-success?
               init-DB
               connect-db
               make-<connection>
               <connection>?
               current-connection

               ;; fprm module
               map-table-from-DB
               make-table-getter
               make-table-setter
               make-table-builder
               make-table-dropper

               ;; ssql module
               ->sql
               where
               having
               /in
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
               rc-conn rc-conn!
               new-route-context
               route-context?
               get-header
               get-from-qstr
               get-referer

               ;; csv
               make-csv-reader
               csv->xml
               sxml->csv
               csv-write
               sxml->csv-string

               ;; json
               ->json-string
               scm->json
               scm->json-string
               json->scm
               json-string->scm
               json-parser?
               json-parser-port

               ;; upload
               mfd-simple-dump
               make-mfd-dumper
               content-type-is-mfd?
               parse-mfd-body
               mfd
               call-with-mfd-data
               find-mfd
               make-mfd
               mfd?
               is-mfds?
               mfds-count
               mfd-dispos
               mfd-name
               mfd-filename
               mfd-begin
               mfd-end
               mfd-type
               mfd-simple-dump-all
               store-uploaded-files
               upload-files-to

               ;; websocket
               named-pipe-subscribe

               ;; server
               schedule-task

               ;; version
               artanis-version)
  #:export (result-ref
            init-server
            form-tag
            label-tag
            a-tag
            p-tag
            div-tag
            run))

(define* (result-ref alst key #:key (decode? #t))
  (and=> (assoc-ref alst key) (if decode? uri-decode identity)))

(define (default-route-init statics cache-statics? exclude)
  ;; avoid a common warn
  (get "/" #:cache #t (lambda () "no index.html but it works!"))
  (let ((srule (format #f "^/.+\\.(~{~a~^|~})$" (lset-difference eq? statics exclude))))
    (if cache-statics?
        (get srule #:cache '(static) (lambda (rc) (:cache rc)))
        (get srule static-page-emitter))))

(define (check-invalid-config)
  (when (and (not (linux-version>=? "3.9")) (get-conf '(server multi)))
    (error "It's required to have Linux-3.9+ to enable server.multi feature!")))

;; make sure to call init-server at the beginning
(define* (init-server #:key (statics '(png jpg jpeg ico html js css))
                      (cache-statics? #f) (exclude '()))
  (default-route-init statics cache-statics? exclude)
  (init-hook)
  (init-config)
  (init-lpc)
  (check-invalid-config)
  (sigaction SIGPIPE SIG_IGN) ; surpass SIGPIPE signal since we want to handle EPIPE by self
  (sigaction SIGINT (lambda (i)
                      (run-when-sigint-hook)
                      (format (artanis-current-output)
                              "~%Fare you well, you server is cold.~%")
                      (quit)))
  (set! is-init-server-run? #t))

(define* (form-tag #:key (controller #f) (action #f) (method #f)
                   (class #f) (tag-class #f) (tag-id #f) (form-method "get"))
  (lambda tags
    (call-with-output-string
      (lambda (port)
        (format port "<form accept-charset='~a'" (get-conf '(server charset)))
        (format port " action='~a'"
                (call-with-output-string
                  (lambda (port2)
                    (display "/" port2)
                    (and controller (format port2 "~a/" controller))
                    (and action (format port2 "~a?" action))
                    (and method (format port2 "method=~a" method))
                    (and class (format port2 "&class=~a" class)))))
        (format port " method='~a'" method)
        (and tag-class (format port " class='~a'" tag-class))
        (and tag-id (format port " id='~a'" tag-id))
        (display ">\n" port)
        (for-each (lambda (tag) (format port "~a~%" tag)) tags)
        (format port "</form>~%")))))

(define (make-general-tag tag)
  (lambda attrs
    (lambda (contents)
      (call-with-output-string
        (lambda (port)
          (format port "<~a" tag)
          (let lp((next attrs))
            (cond
             ((null? next)
              (display ">\n" port)
              (display contents port)
              (newline port)
              (format port "</~a>" tag))
             (else
              (format port " ~a='~a'" (keyword->symbol (car next)) (cadr next))
              (lp (cddr attrs))))))))))

(define label-tag (make-general-tag 'label))
(define a-tag (make-general-tag 'a))
(define p-tag (make-general-tag 'p))
(define div-tag (make-general-tag 'div))

(define (debug-mode:file-monitoring rc body)
  (reload-monitored-files))

(define (init-debug-mode)
  (conf-set! 'debug-mode #t)
  (cond
   ((current-toplevel)
    ;; NOTE: If (current-toplevel) is #f, then it means you're not running under
    ;;       application folder, then the file monitoring feature is disabled automatically.
    (init-debug-monitor)
    (run-after-request! debug-mode:file-monitoring))
   (else (format (artanis-current-output)
                 "[WARN] You're not in application folder, the file motoring is disabled!~%"))))

;; Invalid use-db? must be (dbd username passwd) or #f
(define* (run #:key (host #f) (port #f) (debug #f) (use-db? #f) (db-proto #f) (server #f)
              (dbd #f) (db-username #f) (db-passwd #f) (db-name #f) (db-addr #f))
  (define (->proper-body-display body)
    (cond
     ((not body) "No body in the request!")
     ((string? body)
      (if debug
          body
          (substring body
                     (and=> (string-length body)
                            (lambda (len) (if (> len 100) 100 len))))))
     (((@ (rnrs bytevectors) bytevector?) body) "Body is bytevectors!")
     ((websocket-frame? body)
      (if debug
          (print-websocket-frame body)
          "<websocket-frame>"))
     (else (throw 'artanis-err 500 ->proper-body-display "Invalid body type `~a'!" body))))
  (when (not is-init-server-run?)
    (error "Sorry, but you have to run (init-server) in the begining of you main program!"))
  (and host (conf-set! '(host addr) host))
  (and port (conf-set! '(host port) port))
  (and server (conf-set! '(server engine) (string->symbol server)))
  (init-server-core)
  (when debug
    (display "DEBUG: ON\n")
    (init-debug-mode))
  (when (or use-db? (get-conf '(db enable)))
    (conf-set! '(db enable) #t)
    (display "User wants to use Database, initializing...\n")
    (init-database-config dbd db-username db-passwd db-name db-addr db-proto)
    (init-DB)
    (display "DB init done!\n")
    (when (eq? 'db (get-conf '(session backend)))
      (session-init)
      (display "Session with DB backend init done!\n")))
  (case (get-conf '(session backend))
    ((db)
     (when (not (get-conf '(db enable)))
       (error "Session with DB backend init failed because you didn't enable DB!")))
    (else
     (session-init)
     (format #t "Session with ~:@(~a~) backend init done!~%"
             (get-conf '(session backend)))))
  (run-hook *before-run-hook*)
  (format #t "Server core: ~a~%" (get-conf '(server engine)))
  (format #t "~a~%" (current-myhost))
  (format #t "Anytime you want to quit just try Ctrl+C, thanks!~%")
  (let ((handler (if debug
                     (lambda (r b . _)
                       (format #t "[Request] ~a~%[Body] ~a~%" r (->proper-body-display b))
                       (server-handler r b))
                     server-handler)))
    (establish-http-gateway handler)))
