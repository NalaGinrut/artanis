;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015
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

(define-module (artanis mvc controller)
  #:use-module (artanis utils)
  #:use-module (artanis tpl)
  #:use-module (artanis env)
  #:use-module (ice-9 format)
  #:export (do-controller-create
            define-artanis-controller
            load-app-controllers
            register-controllers))

(define-macro (define-artanis-controller name)
  `(begin
     ;; NOTE: we have to encapsulate them to a module for protecting namespaces
     ;; NOTE: we're not going to imort (artanis env) directly to avoid revealing global
     ;;       env vars to users.
     (define-module (app controller ,name)
       #:use-module (artanis artanis)
       #:use-module (artanis utils))
     ;; NOTE: it's important to use macro here, or it'll eval (option ...) later, which
     ;;       will throw error while calling another macro to expand expr.
     (define-macro (__!$$^d!%_set-app-controller rule meta)
       `(hash-set! (@ (artanis env) *controllers-table*) ,rule ,meta))
     (define-macro (view-render method)
       `(tpl->response ,(format #f "~a/app/view/~a/~a.html.tpl" ((@ (artanis env) current-toplevel)) ',name method)
                       (the-environment)))
     (define-syntax ,(symbol-append name '-define)
       (syntax-rules ()
         ((_ method rest rest* ...)
          (__!$$^d!%_set-app-controller (format #f "/~a/~a" ',name 'method)
                                        (draw-expander rest rest* ...)))))))

(define-syntax-rule (scan-controllers) (scan-app-components 'controller))

(define (load-app-controllers)
  (define toplevel (current-toplevel))
  (display "Loading controllers...\n" (artanis-current-output))
  (use-modules (artanis mvc controller))
  (let ((cs (scan-controllers)))
    (for-each (lambda (s)
                (load (format #f "~a/app/controller/~a.scm" toplevel s)))
              cs)))

(define (register-controllers)
  (hash-for-each cache-this-route! *controllers-table*))

(define (gen-controller-header cname)
  (call-with-output-string
   (lambda (port)
     (format port ";; Controller ~a definition of ~a~%" cname (current-appname))
     (display ";; Please add your license header here.\n" port)
     (display ";; This file is generated automatically by GNU Artanis.\n" port)
     (format port "(define-artanis-controller ~a)~%~%" cname))))

(define (do-controller-create name methods port)
  (display (gen-controller-header name) port)
  (for-each (lambda (method)
              (format port "(~a-define ~a~%" name method)
              (format port "~2t(lambda (rc)~%")
              (format port "\"<h1>This is ~a#~a</h1><p>Find me in app/views/~a/~a.html.tpl</p>\"~%"
                      name method name method)
              (format port "~2t;; TODO: add controller method `~a'~%" method)
              (format port "~2t;; uncomment this line if you want to render view from template~%")
              (format port "~2t;; (view-render \"~a\")~%" method)
              (format port "~2t))~%~%"))
            methods))
