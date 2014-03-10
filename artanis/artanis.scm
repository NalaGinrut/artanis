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
  #:use-module (artanis cookie)
  #:use-module (artanis tpl)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 regex)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web http)
  #:use-module (web server)
  #:use-module (sxml simple)
  #:export (get post put patch delete params header run response-emit
            throw-auth-needed tpl->html redirect-to init-server
            generate-response-with-file emit-response-with-file
            tpl->response reject-method response-error-emit
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
            rc-set-cookie rc-set-cookie!))

;; table structure:
;; '((rule-handler-key (handler . keys)) ...)
;; for example:
;; `(("GET \"/photo/:id/edit\"" (,(lambda (req ..) ...) . id)))  
(define *handlers-table* (make-hash-table))

(define-record-type handler-rc
  (make-handler-rc handler keys oht)
  handler-rc?
  (hander handler-rc-handler)
  (keys handler-rc-keys)
  (oht handler-rc-oht))

(define (get-handler-rc handler-key)
  (hash-ref *handlers-table* handler-key))

(define (define-handler method rule opts-and-handler)
  (let ((keys (rule->keys rule))
        (path-regexp (compile-rule rule))
        (opts (oah->opts opts-and-handler))
        (handler (oah->handler opts-and-handler)))
    (hash-set! *handlers-table*
               (string-append method " " path-regexp)
               (make-handler-rc handler keys (new-oht ots #:rule rule #:keys keys)))))

(define (get rule opts-and-handler) (define-handler "GET" rule opts-and-handler))
(define (post rule opts-and-handler) (define-handler "POST" rule opts-and-handler))
(define (put rule opts-and-handler) (define-handler "PUT" rule opts-and-handler))
(define (patch rule opts-and-handler) (define-handler "PATCH" rule opts-and-handler))
(define (delete rule opts-and-handler) (define-handler "DELETE" rule opts-and-handler))

(define-record-type route-context
  (make-route-context handler keys regexp request path 
                      qt method rhk bt body date cookie set-cookie)
  route-context?
  (handler rc-handler rc-handler!) ; reqeust handler
  (keys rc-keys rc-keys!) ; rule keys
  (regexp rc-re rc-re!) ; regexp to parse key-bindings
  (request rc-req rc-req!) ; client request
  (path rc-path rc-path!) ; path from uri
  (qt rc-qt rc-qt!) ; query table
  (method rc-method rc-method!) ; request method
  (rhk rc-rhk rc-rhk!) ; rule handler key in handlers-table
  (bt rc-bt rc-bt!) ; bindings table
  (body rc-body rc-body!) ; request body
  (date rc-mtime rc-mtime!) ; modified time, users need to set it in handler
  (cookie rc-cookie rc-cookie!) ; the cookie parsed from header string
  (set-cookie rc-set-cookie rc-set-cookie!)) ; the cookies needed to be set as response

;; compiled regexp for optimization
(define *rule-regexp* (make-regexp ":[^\\/]+"))    
(define *path-keys-regexp* (make-regexp "/:([^\\/]+)"))
(define *key-regexp* (make-regexp "([^ ]+) (.+)"))

;; parse rule-string and generate the regexp to parse keys from path-string
(define (rule->keys rule)
  (map (lambda (m) (match:substring m 1))
       (list-matches *path-keys-regexp* rule)))

(define (compile-rule rule)
  (string-append "^" 
                 (regexp-substitute/global 
                  #f *rule-regexp* rule 'pre "([^\\/\\?]+)" 'post)
                 "[^ ]?"))

;; find & set the key of rule-handler,
;; which is used to find the (handler . keys)
;; FIXME: each method should have a own table
(define (init-rule-handler-key! rc)
  (define rmtd (rc-method rc))
  (define path (rc-path rc))
  (define (key-matches-route? pattern)
    (let* ((ml (regexp-split *key-regexp* pattern))
           (method (cadr ml))
           (path-regexp (caddr ml)))
      (and (eq? rmtd (string->symbol method))
           (regexp-exec (make-regexp path-regexp) path))))
  (rc-rhk! rc (find key-matches-route? (hash-keys *handlers-table*))))

;; find&set! the rule handler to rc
(define (init-rule-handler-and-keys! rc)
  (let* ((handler-key (rc-rhk rc))
         (hrc (if handler-key  ; get handler-keys pair
                  (get-handler-rc handler-key)
                  (throw 'artanis-err 404 "invalid handler key" handler-key))))
    (rc-handler! rc (handler-rc-handler hrc))
    (rc-keys! rc (handler-rc-keys hrc))))

(define (init-rule-path-regexp! rc)
  (rc-re! rc (caddr (regexp-split *key-regexp* (rc-rhk rc)))))

;; init key-bindings table
(define (init-rule-key-bindings! rc)
  (let ((m (string-match (rc-re rc) (rc-path rc))))
    (rc-bt! rc
            (map (lambda (k i) (cons k (match:substring m i))) 
                 (rc-keys rc) (iota (1- (match:count m)) 1)))))

(define (init-query! rc)
  (let ((str (case (rc-method rc)
                ((GET) (uri-query (request-uri (rc-req rc))))
                ((POST) ((@ (rnrs) utf8->string) (rc-body rc)))
                (else (throw 'artanis-err 405 
                             "wrong method for query!" (rc-method rc))))))
    (if str
        (rc-qt! rc (map (lambda (x) (string-split x #\=))
                        (string-split str #\&)))
        '())))

;; parse query or posted data while needed
;; ENHANCE: do we need query hashtable?
(define (get-from-qstr/post rc key)
  (unless (rc-qt rc) (init-query! rc))
  (and (rc-qt rc)
       (let ((v (assoc-ref (rc-qt rc) key)))
         (and v (car v)))))
      
;; parse params while needed
;; the params will be searched in param-list first, then search from qstr/post
;; ENHANCE: do we need query hashtable?
(define (params rc key)
  (unless (rc-bt rc) (init-rule-key-bindings! rc))
  (or (assoc-ref (rc-bt rc) key)
      (get-from-qstr/post rc key)))

(define sys-page-path (make-parameter "./"))
(define (page-show file port)
  (bv-cat (string-append (sys-page-path) "/" file) port))

;; ENHANCE: use colored output
(define* (log status mime req #:optional (port (current-error-port)))
  (let* ((uri (request-uri req))
         (path (uri-path uri))
         (qstr (uri-query uri))
         (method (request-method req)))
    (format port "[Remote] ~a @ ~a~%" (remote-info req) (local-time-stamp))
    (format port "[Request] method: ~a, path: ~a, qeury: ~a~%" method path qstr)
    (format port "[Response] status: ~a, MIME: ~a~%~%" status mime)))

(define (render-sys-page status request)
  (log status 'text/html request)
  (values
   (build-response #:code status
                   #:headers `((server . ,server-info)
                               (content-type . (text/html))
                               (charset . ,(current-charset))))
   (page-show (get-sys-page status) #f)))

(define (handler-render handler rc)
  (call-with-values
      (lambda ()
        (if (thunk? handler) 
            (handler) 
            (handler rc)))
    (lambda* (body #:key (pre-headers (prepare-headers body '()))
                   (status 200) 
                   (mtime (let ((t (current-time))) 
                            (cons (time-second t) (time-nanosecond t)))))
      (let ((type (assoc-ref pre-headers 'content-type)))
        (and type (log status (car type) (rc-req rc))))
      (values
       (build-response #:code status
                       #:headers `((server . ,server-info)
                                   (date . ,(get-global-date))
                                   (last-modified . ,(get-local-date mtime))
                                   ,@pre-headers 
                                   ,@(generate-cookies (rc-set-cookie rc))))
       ;; NOTE: For inner-server, sanitize-response will handle 'HEAD method
       ;;       though rc-method is 'GET when request-method is 'HEAD,
       ;;       sanitize-response only checks method from request
       body))))

(define (new-route-context request body)
  (let* ((uri (request-uri request))
         (path (uri-path uri))
         (m (valid-method? (request-method request))) 
         ;; NOTE: sanitize-response will handle 'HEAD method
         ;;       though rc-method is 'GET when request-method is 'HEAD,
         ;;       sanitize-response only checks method from request
         (method (if (eq? m 'HEAD) 'GET m))
         (cookies (request-cookies request))
         (rc (make-route-context #f #f #f request path #f 
                                 method #f #f body #f cookies '())))
    ;; FIXME: maybe we don't need rhk? Throw it after get handler & keys
    (init-rule-handler-key! rc) ; set rule handler key
    (init-rule-handler-and-keys! rc) ; set handler and keys
    (init-rule-path-regexp! rc) ; set regexp
    rc))

(define (format-status-page status request)
  (format (current-error-port) "[EXCEPTION] ~a is abnormal request, status: ~a, "
          (uri-path (request-uri request)) status)
  (display "rendering a sys page for it...\n") 
  (render-sys-page status request))

(define (format-updating-page)
  (display "site is temporarily down!\n" (current-error-port))
  (values
   (build-response #:code 200
                   #:headers `((server . ,server-info)
                               (content-type . (text/html))))
   (lambda (port)
     (page-show (current-update-page) port))))

(define (work-with-request request body)
  (catch 'artanis-err
    (lambda ()
      (let* ((rc (new-route-context request body))
             (handler (rc-handler rc)))
        (if handler 
            (handler-render handler rc)
            (render-sys-page 404 rc))))
    (lambda (k . e)
      (let ((status (car e))
            (reason (cadr e))
            (info (caddr e)))
        (format (current-error-port) "[ERR Reason]: ~a,~a~%" reason info)
        (format-status-page status request)))))

(define (response-emit-error status)
  (response-emit "" #:status status))

(define* (response-emit body #:key (status 200) 
                        (headers '())
                        (mtime (current-time)))
  ;;(format #t "headers: ~a~%" headers)
  (values body #:pre-headers (prepare-headers body headers) #:status status 
          #:mtime (cons (time-second mtime) (time-nanosecond mtime))))

(define (throw-auth-needed)
  (values 401 '((WWW-Authenticate . "Basic realm=\"Secure Area\"")) ""))

(define site-workable? #t)

(define (server-handler request request-body)
  (if site-workable?
      (work-with-request request request-body)
      (format-updating-page)))

;; proc must return the content-in-bytevector
(define (generate-response-with-file filename proc)
  (if (file-exists? filename)
      (let* ((st (stat filename))
             ;; NOTE: we use ctime for last-modified time
             (mtime (make-time time-utc (stat:ctime st) (stat:ctimensec st)))
             (port (open-input-file filename))
             (mime (guess-mime filename)))
        (values mtime 200 (proc port) mime))
      (values #f 404 "" #f)))

(define (emit-response-with-file filename)
  (call-with-values
      (lambda ()
        (generate-response-with-file filename (lambda (p) (bv-cat p #f))))
    (lambda (mtime status bv mime)
      (cond
       ((= status 200) 
        (response-emit bv #:status status 
                       #:headers `((content-type . ,(list mime)) 
                                   ,@(generate-ETag filename))
                       #:mtime mtime))
       (else (response-emit bv #:status status))))))
      
(define (default-route-init)
  ;; avoid a common warn
  (get "/$" (lambda () "no index.html but it works!"))
  (get "/.+\\.(png|jpg|jpeg|ico|html|js|css)$" 
   (lambda (rc) 
     (emit-response-with-file (static-filename (rc-path rc))))))

(define (site-disable msg)
  (set! site-workable? #f))

(define (site-enable msg)
  (set! site-workable? #t))

(define-syntax-rule (tpl->response sxml/file ...)
  (let ((html (tpl->html sxml/file ...)))
    (if html
        (response-emit html)
        (response-emit "" #:status 404))))

(define* (tpl->html sxml/file #:optional (e (current-module)))
  (cond
   ((string? sxml/file) ; it's tpl filename
    (tpl-render-from-file sxml/file e))
   ((list? sxml/file) ; it's sxml tpl
    (call-with-output-string (lambda (port) (sxml->xml sxml/file port))))
   (else #f))) ; wrong param causes 404

;; 301 is good for SEO and avoid some client problem
(define* (redirect-to rc path #:optional (status 301))
  (response-emit
   ""
   #:status status
   #:headers `((location . ,(build-uri 'http #:path path));;,(string->uri (string-append (current-myhost) path)))
               (content-length . 0)
               (content-type . (text/html)))))

(define (reject-method method)
  (throw 'artanis-err 405 "Method is not allowed" method))

;; make sure to call init-server at the beginning
(define (init-server)
  (sigaction SIGUSR1 site-disable)
  (sigaction SIGCONT site-enable)
  (default-route-init)
  (init-config))

(define* (run #:key (host #f) (port #f) (debug #f))
  (format #t "Anytime you want to Quit just try Ctrl+C, thanks!~%")
  (and host (set! *host-addr* host))
  (and port (set! *host-port* port))
  (format #t "~a~%" (current-myhost))
  (run-server
   (if debug
       (lambda (r b) (format #t "~a~%~a~%" r b) (server-handler r b))
       server-handler)
   'http `(#:host ,*host-addr* #:port ,*host-port*)))
