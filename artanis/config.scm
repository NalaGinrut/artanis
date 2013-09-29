;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013
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

(define-module (artanis config))

(module-export-all! (current-module))

(define server-info "Artanis-0.0.1")

(define use-Nginx? #f)

;; FIXME: *myhost* should be dynamically generated
(define *host-addr* "0.0.0.0")
(define *host-port* 3000)
(define (current-myhost) (format #f "http://~a:~a" *host-addr* *host-port*))
(define *error-page-path* "../pages")
(define *updating-page* "updating.html")
(define *session-path* "session/")

;; TODO: support dynamic rule for sys page
(define (get-sys-page status)
  (format #f "~a/~a.html" *error-page-path* status)) 

(define current-update-page
  (make-parameter
   (format #f "~a/~a" *error-page-path* *updating-page*)))

(define current-upload-path
  (make-parameter "upload/"))   
  
(define current-start-sign (make-parameter "<%"))
(define current-startd-sign (make-parameter "<%="))
(define current-end-sign (make-parameter "%>"))

(define current-mail-sender (make-parameter "/usr/sbin/sendmail"))

(define current-charset (make-parameter "utf-8"))

(define (init-config)
  (unless (file-exists? *session-path*) (mkdir *session-path*)))
