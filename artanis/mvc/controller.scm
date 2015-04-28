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
  #:use-module (artanis env)
  #:export (do-controller-create
            define-controller))

(define (get-app-controller name)
  (hash-ref *controllers-table* name))

(define-macro (define-artanis-controller name)
  `(define-syntax ,(symbol-append 'define- name)
     (syntax-rules (rule option)
       ((_ (option oht mode) rest rest* ...)
        `(,oht ,mode ,@(draw-expander rest rest* ...)))
       ((_ (rule url) rest rest* ...)
        `((rule ,url) ,@(draw-expander rest rest* ...)))
       ((_ handler) (list handler)))))

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
              (format port "(define-~a ~a~%" name method)
              (format port "~2t(lambda (rc)~%")
              (format port "~2t;; TODO: add controller method `~a'~%" method)
              (format port "~2t))~%~%"))
            methods))
