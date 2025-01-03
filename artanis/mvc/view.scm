;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015,2017,2018,2019
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
      (format port "~10tPlease add your license header here.\n")
      (format port "~10tThis file is generated automatically by GNU Artanis. -->\n\n"))))

(define (generate-view-template vname mname mpath)
  (format #f
          "<html>
~2t<head>
~4t<title><%= (current-appname) %>
~4t</title>
~4t~a
~2t</head>
~2t<body>
~4t<h1>~a#~a</h1>
~4t<p>Rendered from ~a.</p>
~2t</body>
</html>" free-JS-announcement vname mname mpath))

(define (generate-view m)
  (define vname (basename (dirname m)))
  (define mname (car (string-split (basename m) #\.)))
  (define filename (format #f "~a/~a" (current-toplevel) m))
  (when (file-exists? filename)
    (handle-existing-file filename #t))
  (format (artanis-current-output) "create ~10t ~a~%" m)
  (let ((file (if (cmd:is-dry-run?) *null-device* filename)))
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
      (DEBUG "pub/~a/~a" (params rc "type") (params rc "static"))
      (static-page-emitter rc)))

  (get "/pub/:static"
    (lambda (rc)
      (DEBUG "pub/~a" (params rc "static"))
      (static-page-emitter rc))))
