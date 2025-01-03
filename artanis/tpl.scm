;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2016,2017,2018
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

;;  Django style embedded-template, it's different from tpl->html which
;;  is based on SXML.
;;  If you want to embed the template into HTML code, say:
;;  <html>
;;    <% (let ((var "123")) %>
;;    <p> <%= (let ((x (expt 2 100))) (+ 1 x)) %> </p>
;;    <% (tpl->html `(p ,var))) %>
;;  </html>

(define-module (artanis tpl)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis tpl parser)
  #:export (tpl-render tpl-render-from-file))

(define (load-from-tpl-cache file)
  ;; check if original file is newer than cache file
  (define (cache-is-old? cfile ofile)
    (let ((ost (stat ofile))
          (cst (stat cfile)))
      (> (stat:mtime ost) (stat:mtime cst))))
  (let ((cfile (format #f "~a/cache/tpl/~a" (current-tmp) file)))
    (and (file-exists? cfile)
         (cache-is-old? cfile file)
         (cat file #f))))

(define (tpl->expr tpl)
  (call-with-input-string tpl tpl-read))

(define (cache-the-file expr ofile)
  (let* ((cdir (format #f "~a/cache/tpl/~a/"
                       (current-tmp) (dirname ofile)))
         (cfile (string-append cdir (basename ofile) ".cache")))
    (when (not (file-exists? cdir))
      (DEBUG "Create cache directory ~a~%" cdir)
      (checkout-the-path cdir))
    (when (file-exists? cfile) (delete-file cfile))
    (DEBUG "Refresh tpl cache ~a~%" cfile)
    (call-with-output-file cfile
      (lambda (port) (display expr port)))))

;; NOTE:
;; cache-file is #f, mean no need to recache, otherwise is the cache filename
(define-syntax-rule (tpl-render tpl e cache-file)
  (let ((expr (tpl->expr tpl)))
    (cache-the-file expr cache-file)
    (call-with-output-string
     (lambda (port)
       (parameterize ((current-output-port port))
         (local-eval-string expr e))))))

(define-syntax-rule (tpl-render-from-file file e)
  (cond
   ((load-from-tpl-cache file)
    => (lambda (str)
         (DEBUG "Load tpl cache ~a~%" file)
         (tpl-render str e #f)))
   ((file-exists? file)
    (DEBUG "Render tpl from file ~a~%" file)
    (tpl-render (cat file #f) e file))
   (else (throw 'artanis-err 500 'tpl-render-from-file
                "No such a tpl file `~a'" file))))
