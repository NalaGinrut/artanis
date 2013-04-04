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
  #:export (tpl-render))

(define start-sign (current-start-sign))
(define startd-sign (current-startd-sign))
(define end-sign (current-end-sign))

(define ss-len (string-length start-sign))
(define sd-len (string-length startd-sign))
(define es-len (string-length end-sign))

(define tpl-outport (make-parameter #f))

(define tpl->expr
  (lambda (tpl)
    (call-with-output-string
     (lambda (port)
       (letrec*
	;; template parser
	((tpl-parser
	  (lambda args
	    (let* ((str-in (apply substring/shared `(,tpl ,@args)))
		   (pos (car args))
		   (get-position
		    (lambda (sign)
		      (let ((p (string-contains str-in sign)))
			(if p
			    (+ p pos)
			    p))))
		   (s (get-position start-sign))
		   (e (get-position end-sign))
		   (sd (get-position startd-sign))
		   (in-len (string-length str-in)))
	      (cond
	       ((= in-len 0) #t) ;; recursive exit
	       ((and (or s sd) 
		     (not e))
		(error tpl-parser "can't find ending!"))
	       ((and (and (not s) (not sd)) 
		     e)
		(error tpl-parser "invalid template file!"))
	       ((and (and (not s) (not sd))
		     (not e))
		(write-html-to-out-buf pos))
	       ((and sd 
		     (or (not s) (< sd s))) ;; <%= situation FIXME: I didn't consider sd-s-e, say, no sd-e 
		(write-html-to-out-buf pos sd)
		(write-script-display-to-out-buf (+ sd sd-len) e)
		(tpl-parser (+ e es-len)))
	       (else
		(write-html-to-out-buf pos s)
		(write-script-to-out-buf (+ s ss-len) e)
		(tpl-parser (+ e es-len)))))))
	 ;; handle script display part
	 (write-script-display-to-out-buf
	  (lambda args
	    (let ((script-in (apply substring/shared `(,tpl ,@args))))
              ;; FIXME: do it more elegant
              (format port "~a" 
                      (string-append 
                       " (format #t \"~a\" "
                       script-in " ) ")))))
	 ;; handle script part
	 (write-script-to-out-buf
	  (lambda args
	    (let ((script-in (apply substring/shared `(,tpl ,@args))))
	      (display script-in port))))
	 ;; handle html part
	 (write-html-to-out-buf
	  (lambda args
	    (let ((html-str (apply substring/shared `(,tpl ,@args))))
	      (format port " (display ~s) " html-str)))))
	(tpl-parser 0))))))

(define (tpl-render tpl)
  (let ((expr (tpl->expr tpl)))
    (call-with-output-string
     (lambda (port)
       (parameterize ((current-output-port port))
         (eval-string expr))))))
