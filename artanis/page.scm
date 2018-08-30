;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014,2015,2016,2017,2018
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

(define-module (artanis page)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis config)
  #:use-module (artanis cookie)
  #:use-module (artanis lpc)
  #:use-module (artanis tpl)
  #:use-module (artanis tpl sxml)
  #:use-module (artanis db)
  #:use-module (artanis route)
  #:use-module (artanis websocket)
  #:use-module (artanis server http)
  #:use-module (artanis server server-context)
  #:use-module (srfi srfi-19)
  #:use-module (web uri)
  #:use-module (web http)
  #:use-module (ice-9 match)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 futures)
  #:use-module ((rnrs) #:select (bytevector-length bytevector?))
  #:export (params
            response-emit
            throw-auth-needed
            tpl->html
            redirect-to
            tpl->response
            reject-method
            response-error-emit
            server-handler
            init-hook
            emit-response-with-file
            static-page-emitter))

;; the params will be searched in binding-list first, then search from qstr
;; TODO: qstr should be independent from rules binding.
(define (params rc key)
  ((current-encoder)
   (or (assoc-ref (rc-bt rc) key)
       (get-from-qstr rc key))))

(define (rc-conn-recycle rc body)
  (and=> (rc-conn rc) DB-close))

(define (rc-lpc-recycle rc body)
  (and=> (rc-lpc rc) lpc-instance-recycle))

(define (try-to-register-websocket-pipe! req new-client)
  (let ((name (detect-pipe-name req)))
    (cond
     ((get-named-pipe name)
      => (lambda (named-pipe)
           (let ((old-client (named-pipe-client named-pipe))
                 (server (current-server)))
             ;; NOTE: If the same name was specified, we close the old one then register the
             ;;       new one. This is because some clients/browswers have bugs to not close
             ;;       connection when refresh the page/webapi, so we close it positively to
             ;;       avoid further problem.
             ;; FIXME: We should detect secure token here first.
             (%%raw-close-connection server old-client #t)
             (closing-websocket-handshake server old-client #t)
             (named-pipe-client-set! named-pipe new-client)
             (register-websocket-pipe! named-pipe))))
     (else (register-websocket-pipe! (new-named-pipe name new-client))))))

(define (run-after-request-hooks rq body)
  (run-hook *after-request-hook* rq body))

(define (run-before-response-hooks rc body)
  (run-hook *before-response-hook* rc body))

(define (init-after-request-hook)
  #t)

(define (init-before-response-hook)
  (run-before-response! rc-conn-recycle)
  (run-before-response! rc-lpc-recycle))

(define (init-after-websocket-hook)
  (run-after-websocket-handshake! try-to-register-websocket-pipe!))

(define (init-startup-hook)
  #t)

;; NOTE: If you want to add hook during initialization time, put them here.
(define (init-hook)
  (init-startup-hook)
  (init-after-request-hook)
  (init-before-response-hook)
  (init-after-websocket-hook))

(define (handler-render handler rc)
  (define (->bytevector body)
    (cond
     ((bytevector? body) body)
     ((string? body) (string->bytevector body (get-conf '(server charset))))
     ((not body) #vu8())
     (else body))) ; just let it be checked by http-write
  (call-with-values
      (lambda ()
        (if (thunk? handler)
            (handler)
            (handler rc)))
    (lambda* (body #:key (pre-headers (prepare-headers '()))
                   (status 200)
                   (mtime (generate-modify-time (current-time)))
                   (request-status 'ok))
      (let* ((reformed-body (->bytevector body))
             (response
              (build-response #:code status
                              #:headers `((server . ,(get-conf '(server info)))
                                          (last-modified . ,mtime)
                                          ,(gen-content-length reformed-body)
                                          ,@pre-headers
                                          ,@(generate-cookies (rc-set-cookie rc))))))
        (run-before-response-hooks rc body)
        (let ((type (assq-ref pre-headers 'content-type)))
          (and type (artanis-log 'client status (car type) #:request (rc-req rc))))
        ;; NOTE: For inner-server, sanitize-response will handle 'HEAD method
        ;;       though rc-method is 'GET when request-method is 'HEAD,
        ;;       sanitize-response only checks method from request.
        (if (is-guile-compatible-server-core? (get-conf '(server engine)))
            (values response reformed-body)
            (values response reformed-body
                    ;; NOTE: return the status while handling the request.
                    request-status))))))

(define (work-with-request request body)
  ;;(DEBUG "work with request~%")
  (catch 'artanis-err
    (lambda ()
      (let* ((rc (new-route-context request body))
             (handler (rc-handler rc)))
        (if handler
            (handler-render handler rc)
            (render-sys-page 'client 404 rc))))
    (make-unstop-exception-handler (exception-from-client request))))

(define (response-emit-error status)
  (response-emit "" #:status status))

;; NOTE: last-modfied in #:headers will be ignored, it should be in #:mtime
(define* (response-emit body #:key (status 200)
                        (headers '())
                        (mtime (current-time))
                        (request-status 'ok))
  (DEBUG "Response emit headers: ~a~%" headers)
  (values body #:pre-headers (prepare-headers headers) #:status status
          #:mtime (generate-modify-time mtime)
          #:request-status request-status))

(define (throw-auth-needed)
  (response-emit
   ""
   #:status 401
   #:headers '((WWW-Authenticate . "Basic realm=\"Secure Area\""))))

(define (server-handler request request-body . _)
  ;; ENHANCE: could put some stat hook here
  (run-after-request-hooks request request-body)
  (work-with-request request request-body))

(define-syntax-rule (tpl->response sxml/file ...)
  (let ((html (tpl->html sxml/file ...)))
    (if html
        (response-emit html)
        (response-emit "" #:status 404))))

(define* (tpl->html sxml/file #:optional (env (current-module)) (escape? #f))
  (cond
   ((string? sxml/file) ; it's tpl filename
    (tpl-render-from-file sxml/file env))
   ((list? sxml/file) ; it's sxml tpl
    (call-with-output-string (lambda (port) (sxml->xml sxml/file port escape?))))
   (else #f))) ; wrong param causes 404

;; 301 is good for SEO and avoid some client problem
;; Use `URL scheme' incase users need to redirect to HTTPS or others.
(define* (redirect-to rc path #:key (status 301) (scheme 'http))
  (response-emit
   ""
   #:status status
   #:headers `((location . ,(build-uri scheme #:path path))
               (content-length . 0)
               (content-type . (text/html)))))

(define (reject-method method)
  (throw 'artanis-err 405 "Method is not allowed" method))

;; proc must return the content-in-bytevector
(define (generate-response-with-file filename file-sender)
  (let* ((st (stat filename))
         ;; NOTE: we use ctime for last-modified time
         (mtime (make-time time-utc (stat:ctime st) (stat:ctimensec st)))
         (mime (guess-mime filename)))
    (values mtime 200 file-sender mime)))

;; emit static file with no cache(ETag)
(define* (emit-response-with-file filename out #:optional (headers '()))
  (when (not (file-exists? filename))
    (throw 'artanis-err 404 emit-response-with-file
           "Static file `~a' doesn't exist!" filename))
  (call-with-values
      (lambda ()
        (cond
         ((or (not (get-conf '(server sendfile)))
              (is-guile-compatible-server-core? (get-conf '(server engine))))
          (generate-response-with-file
           filename
           ;; FIXME: For now, guile compatable server-core doesn't provide good method to
           ;;        support sendfile, so we read then send it. This is OK for small files,
           ;;        but bad for larger files.
           (bv-cat filename #f)))
         (else
          (let* ((in (open-input-file filename))
                 (size (stat:size (stat filename))))
            (generate-response-with-file
             filename
             (make-file-sender
              size
              (lambda ()
                (catch #t
                  (lambda ()
                    ;; NOTE: In Linux, non-blocking for regular file (not a socket) is
                    ;;       basically unsupported!!! So we have to find a way to make sure
                    ;;       the regular file reading is non-blocking, or the whole Ragnarok
                    ;;       will be blocked.
                    ;; TODO: use splice to make a real non-blocking version.
                    ;; TODO: support trunked length requesting for continously downloading.
                    (sendfile out in size)
                    (force-output out)
                    ;;(DEBUG "File `~a' sent over!" filename)
                    (close in))
                  (lambda e
                    (close in)
                    (apply throw e))))))))))
    (lambda (mtime status body mime)
      (cond
       ((= status 200)
        (response-emit body #:status status
                       #:headers `((content-type . ,(list mime))
                                   ,@headers)
                       #:mtime mtime
                       #:request-status 'downloading))
       (else (response-emit body #:status status))))))

;; When you don't want to use cache, use static-page-emitter.
(define* (static-page-emitter rc #:key (dir #f))
  (let ((filename (if dir
                      (format #f "~a/~a" dir (rc-path rc))
                      (static-filename (rc-path rc)))))
    (emit-response-with-file filename (request-port (rc-req rc)))))
