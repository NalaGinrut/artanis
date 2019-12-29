;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2019
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

(define-module (artanis ffi)
  #:use-module (system foreign)
  #:export (define-c-function ffi-binding))

(define %%clib (make-parameter (dynamic-link)))

(define-syntax define-c-function
  (lambda (x)
    (syntax-case x ()
      ((_ type name)
       #`(module-define!
          (current-module)
          '#,(datum->syntax #'name (symbol-append '% (syntax->datum #'name)))
          (pointer->procedure type
                              (dynamic-func (symbol->string 'name) (%%clib))
                              '()
                              #:return-errno? #t)))
      ((_ type name (para ...))
       #`(module-define!
          (current-module)
          '#,(datum->syntax #'name (symbol-append '% (syntax->datum #'name)))
          (pointer->procedure type
                              (dynamic-func (symbol->string 'name) (%%clib))
                              (list para ...)
                              #:return-errno? #t))))))

(define-syntax ffi-binding
  (syntax-rules ()
    ((_ () body ...)
     (begin
       body ...
       #t))
    ((_ libname body ...)
     (parameterize ((%%clib (dynamic-link libname)))
       body ...
       #t))))
