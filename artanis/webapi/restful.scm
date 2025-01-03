;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2019
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
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

(define-module (artanis webapi restful)
  #:use-module (artanis utils)
  #:use-module (artanis tpl)
  #:use-module (artanis env)
  #:use-module (ice-9 format)
  #:export (do-restful-api-create
            define-restful-api
            load-app-restful-api
            register-restful-api))

(define-syntax define-restful-api
  (lambda (x)
    (syntax-case x ()
      ((_ version) (identifier? #'version)
       #`(begin
           ;; NOTE: we have to encapsulate them to a module for protecting namespaces
           ;; NOTE: we're not going to imort (artanis env) directly to avoid revealing global
           ;;       env vars to users.
           (define-module (app api version)
             #:use-module (artanis artanis)
             #:use-module (artanis env)
             #:use-module (artanis utils))
           (define-syntax #,(datum->syntax x 'api-define)
             (syntax-rules ::: ()
               ((_ method rest rest* :::)
                (hash-set!
                 (@ (artanis env) *controllers-table*)
                 (format #f "/api/~a/~a" 'version 'method)
                 (draw-expander rest rest* :::))))))))))

(define-syntax-rule (scan-restful-api)
  (scan-app-components 'api))

(define (load-app-restful-api)
  (display "Loading restful API...\n" (artanis-current-output))
  (use-modules (artanis webapi restful)) ; black magic to make Guile happy
  (for-each
   (lambda (s)
     (load (format #f "~a/app/api/~a.scm" (current-toplevel) s)))
   (scan-restful-api)))

(define (register-restful-api)
  ;; NOTE: restful api is a special controller
  (hash-for-each cache-this-route! *controllers-table*))

(define (gen-restful-api-header version)
  (call-with-output-string
   (lambda (port)
     (format port ";; RESTful API ~a definition of ~a~%" version (current-appname))
     (display ";; Please add your license header here.\n" port)
     (display ";; This file is generated automatically by GNU Artanis.\n" port)
     (format port "(define-restful-api ~a) ; DO NOT REMOVE THIS LINE!!!~%~%" version))))

(define (do-restful-api-create version port)
  (format (artanis-current-output) "create ~10t app/api/~a.scm~%"
          version)
  (display (gen-restful-api-header version) port))
