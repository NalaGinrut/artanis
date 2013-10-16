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

;;  Django style embedded-template, it's different from tpl->html which 
;;  is based on SXML.
;;  If you want to embedded the template into HTML code, say:
;;  <html>
;;    <% (define var 123) %>
;;    <p> <%= (let ((x (expt 2 100))) (+ 1 x)) %> </p>
;;    <% (tpl->html `(p ,var)) %>
;;  </html>

(define-module (artanis tpl)
  #:use-module (artanis utils)
  #:use-module (artanis tpl parser)
  #:use-module (artanis config)
  #:use-module (artanis irregex)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:export (tpl-render tpl-render-from-file))

(define (tpl->expr tpl)
  (call-with-input-string tpl tpl-read))

(define-syntax-rule (tpl-render tpl e)
  (let ((expr (tpl->expr tpl)))
    (call-with-output-string
     (lambda (port)
       (parameterize ((current-output-port port))
         (local-eval-string expr e))))))

(define-syntax-rule (tpl-render-from-file file e)
  (cond
   ((file-exists? file)
    (tpl-render (cat file #f) e))
   (else (error 'artanis-err 500 "No such a tpl file" file))))
