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
  #:use-module (artanis config)
  #:export (tpl-render tpl-render-from-file))

(define start-sign (current-start-sign))
(define startd-sign (current-startd-sign))
(define end-sign (current-end-sign))

(define tpl-outport (make-parameter #f))

(define *tpl-irx* 
  (sre->irregex `(or (: ,startd-sign (=> disp-code (+ (~ #\% #\>))) ,end-sign)
                     (: ,start-sign (=> code (+ (~ #\% #\>))) ,end-sign))))
(define (tpl->expr tpl)
  (define (optimize rev-items tail)
    (cond ((null? rev-items) tail)
          ((not (string? (car rev-items)))
           (optimize (cdr rev-items)
                     (cons (car rev-items) tail)))
          (else (receive (strings rest) (span string? rev-items)
                  (let ((s (string-concatenate-reverse strings)))
                    (if (string-null? s)
                        (optimize rest tail)
                        (optimize rest (cons s tail))))))))
  (define* (emit-code idx m code #:optional (disp #f))
    (let ((pre (substring tpl idx (irregex-match-start-index m))))
      (if disp 
          (string-concatenate `("(display \"" ,pre "\")(display " ,code ")"))
          (string-concatenate `("(display \"" ,pre "\")" ,code)))))
  (define (match->item idx m)
    (cond
     ((irregex-match-substring m 'disp-code)
      => (lambda (code)
           (emit-code idx m code 1)))
     ((irregex-match-substring m 'code)
      => (lambda (code)
           (emit-code idx m code)))
     (else (error 'artanis-err 500 "wrong template!" tpl))))
  (let* ((rev-items
          (irregex-fold 
           *tpl-irx*  
           (lambda (idx m tail)
             (cons* (match->item idx m) "" tail))
           '() tpl
           (lambda (idx tail) tail)))             ;;(cons (substring tpl idx) tail))))
         (items (optimize rev-items '())))
    (car items)))

(define-syntax-rule (tpl-render tpl e)
  (let ((expr (tpl->expr tpl)))
    (call-with-output-string
     (lambda (port)
       (parameterize ((current-output-port port))
         (local-eval-string expr e))))))

(define-syntax-rule (tpl-render-from-file file e)
  (and (file-exists? file)
       (tpl-render (cat file #f) e)))
