;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2026
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
;;  Artanis is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  Artanis is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.
;;  If not, see <http://www.gnu.org/licenses/>.

(define-module (artanis exception)
  #:use-module (artanis env)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis logger)
  #:use-module (ice-9 iconv)
  #:use-module (web uri)
  #:use-module (srfi srfi-19)
  #:export (syspage-show
            render-sys-page
            exception-from-client
            exception-from-server))

(define (get-syspage file)
  (let ((local-syspage (format #f "~a/sys/pages/~a"
                               (current-toplevel) file)))
    (if (file-exists? local-syspage)
        local-syspage
        (let ((sys-syspage (format #f "~a/~a" (get-conf '(server syspage path)) file)))
          (and (file-exists? sys-syspage)
               sys-syspage)))))

;; ENHANCE: use a cache.
(define (syspage-show status)
  (let* ((file (format #f "~a.html" status))
         (syspage (get-syspage file)))
    (if syspage
        (bv-cat syspage #f)
        #vu8())))

(define (render-sys-page blame-who? status request)
  (artanis-log blame-who? status 'text/html #:request request)
  (let* ((charset (get-conf '(server charset)))
         (mtime (generate-modify-time (current-time)))
         (guile-compt-serv? (is-guile-compatible-server-core? (get-conf '(server engine))))
         (response
          (build-response #:code status
                          #:headers `((server . ,(get-conf '(server info)))
                                      (last-modified . ,mtime)
                                      (content-type . (text/html (charset . ,charset))))))
         (body (cond
                ((resources-collecting?)
                 #vu8(0)) ; Don't open any file since we don't have resources now
                ((get-syspage-handler status)
                 => (lambda (thunk)
                      (let ((body (thunk)))
                        (cond
                         ((string? body)
                          (string->bytevector body (get-conf '(server charset))))
                         ((bytevector? body) body)
                         (else (syspage-show status))))))
                (else (syspage-show status)))))
    (if guile-compt-serv?
        (values response body)
        (values response body 'exception))))

(define (format-status-page/client status request)
  (format (current-error-port) (ERROR-TEXT "[EXCEPTION] ~a is abnormal request, status: ~a, ")
          (uri-path (request-uri request)) status)
  (display "rendering a sys page for it...\n" (current-error-port))
  (render-sys-page 'client status request))

(define (format-status-page/server status)
  (format (current-error-port) "[SERVER ERROR] Internal error from server-side, ")
  (format (current-error-port) "rendering a ~a page for client ...~%" status)
  (render-sys-page 'server status #f))

(define (exception-from-client request)
  (lambda (status)
    (format-status-page/client status request)))

(define (exception-from-server status)
  (format-status-page/server status))
