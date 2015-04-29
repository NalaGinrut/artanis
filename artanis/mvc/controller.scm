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
  #:export (do-controller-create
            define-controller))

(define-macro (define-artanis-controller name)
  `(begin
     ;; NOTE: we have to encapsulate them to a module for protecting namespaces
     ;; NOTE: we're not going to imort (artanis env) directly to avoid revealing global
     ;;       env vars to users.
     (define-module (app controller ,name)
       #:use-module (artanis artanis)
       #:use-module (artanis utils))
     ;; NOTE: it's importan to use macro here, or it'll eval (option ...) later, which
     ;;       will throw error while calling another macro to expand expr.
     (define-syntax-rule (__!@$$^d!@%_set-app-controller name handler)
       (hash-set! (@ (artanis env) *controllers-table*) name handler))
     (define-syntax ,(symbol-append name '-define)
       (syntax-rules ()
         ((_ method rest rest* ...)
          (__!@$$^d!@%_set-app-controller (symbol-append ',name '- 'method)
                                          (draw-expander rest rest* ...)))))))

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
              (format port "~2t;; TODO: add controller method `~a'~%" method)
              (format port "~2t))~%~%"))
            methods))
