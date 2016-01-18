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

(define-module (artanis mvc view)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis artanis)
  #:use-module (ice-9 format)
  #:export (do-view-create
            load-app-views))

(define (generate-view-header vname mname)
  (call-with-output-string
   (lambda (port)
     (format port "<!-- ~a#~a view template of ~a~%" vname mname (current-appname))
     (display "Please add your license header here.\n" port)
     (display "This file is generated automatically by GNU Artanis. -->\n\n" port))))

(define (generate-view-template vname mname mpath)
  (format #f
          "<html><head><title><%= (current-appname) %></title></head>
<body><h1>~a#~a</h1>
<p>Rendered from ~a.</p>
</body></html>" vname mname mpath))

(define (generate-view m)
  (define vname (basename (dirname m)))
  (define mname (car (string-split (basename m) #\.)))
  (define filename (format #f "~a/~a" (current-toplevel) m)) 
  (when (file-exists? filename)
    (handle-existing-file filename #t))
  (format (artanis-current-output) "create ~10t ~a~%" m)
  (let ((file (if (draw:is-dry-run?) *null-device* filename)))
    (call-with-output-file file
      (lambda (port)
        (display (generate-view-header vname mname) port)
        (display (generate-view-template vname mname m) port)))))

(define (do-view-create path methods)
  (for-each generate-view
            (map (lambda (m) (format #f "app/views/~a/~a.html.tpl" path m))
                 (check-drawing-method methods))))

;; NOTE: files put in "pub" directory will be public for any visitor!
(define (load-app-views)
  (get "/pub/:type/:static"
   (lambda (rc)
     (emit-response-with-file
      (format #f "pub/~a/~a" (params rc "type") (params rc "static")))))
  (get "/pub/:static"
   (lambda (rc)
     (emit-response-with-file
      (format #f "pub/~a" (params rc "static"))))))
