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

(define-module (artanis page)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis cookie)
  #:use-module (artanis tpl)
  #:use-module (artanis oht)
  #:use-module (artanis db)
  #:use-module (artanis route)
  #:use-module (artanis env)
  #:use-module (srfi srfi-19)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web http)
  #:use-module (sxml simple)
  #:export (params
            response-emit
            throw-auth-needed
            tpl->html
            redirect-to
            generate-response-with-file
            emit-response-with-file
            tpl->response
            reject-method
            response-error-emit
            server-handler
            run-after-request!
            run-before-response!
            init-hook))

;; the params will be searched in param-list first, then search from qstr/post
;; TODO: qstr/post should be independent from rules binding.
(define (params rc key)
  (or (assoc-ref (rc-bt rc) key)
      (get-from-qstr/post rc key)))

(define (page-show file port)
  (bv-cat (string-append (get-conf '(server syspage path)) "/" file) port))

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
  (define-syntax-rule (status->page s)
    (format #f "~a.html" s))
  (log status 'text/html request)
  (values
   (build-response #:code status
                   #:headers `((server . ,(get-conf '(server info)))
                               (content-type . (text/html))
                               (charset . ,(get-conf '(server charset)))))
   (page-show (status->page status) #f)))

(define (rc-conn-recycle rc body)
  (and=> (rc-conn rc) DB-close))

(define (run-after-request-hooks rq body)
  (run-hook *after-request-hook* rq body))

(define (run-before-response-hooks rc body)
  (run-hook *before-response-hook* rc body))

(define (init-after-request-hook)
 #t)

(define (init-before-response-hook)
  (run-before-response! rc-conn-recycle))

(define (run-after-request! proc)
  (add-hook! *after-request-hook* proc))

(define (run-before-response! proc)
  (add-hook! *before-response-hook* proc))

;; NOTE: If you want to add hook during initialization time, put them here.
(define (init-hook)
  (init-after-request-hook)
  (init-before-response-hook))

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
      (run-before-response-hooks rc body)
      (let ((type (assoc-ref pre-headers 'content-type)))
        (and type (log status (car type) (rc-req rc))))
      (values
       (build-response #:code status
                       #:headers `((server . ,(get-conf '(server info)))
                                   (date . ,(get-global-date))
                                   (last-modified . ,(get-local-date mtime))
                                   ,@pre-headers 
                                   ,@(generate-cookies (rc-set-cookie rc))))
       ;; NOTE: For inner-server, sanitize-response will handle 'HEAD method
       ;;       though rc-method is 'GET when request-method is 'HEAD,
       ;;       sanitize-response only checks method from request
       body))))

(define (format-status-page status request)
  (format (current-error-port) "[EXCEPTION] ~a is abnormal request, status: ~a, "
          (uri-path (request-uri request)) status)
  (display "rendering a sys page for it...\n" (current-error-port)) 
  (render-sys-page status request))

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
        (format (current-error-port) "[ERR Reason]: ~a ~a~%" reason info)
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

(define (server-handler request request-body)
  ;; ENHANCE: could put some stat hook here
  (run-after-request-hooks request request-body)
  (work-with-request request request-body))

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

;; emit static file with no cache(ETag)
(define* (emit-response-with-file filename #:optional (headers '()))
  (call-with-values
      (lambda ()
        (generate-response-with-file filename (lambda (p) (bv-cat p #f))))
    (lambda (mtime status bv mime)
      (cond
       ((= status 200) 
        (response-emit bv #:status status 
                       #:headers `((content-type . ,(list mime))
                                   ,@headers)
                       #:mtime mtime))
       (else (response-emit bv #:status status))))))

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
