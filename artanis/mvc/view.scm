;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015,2017,2018
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

;; Story: When I released GNU Artanis-0.2.1, RMS had asked me if I can
;;        support LibreJS for freeing Javascript code in the generated code.
;;        I promised I will. This took almost one year since I was very
;;        busy on developing a new product (partly use GNU Artanis, of course),
;;        so it's delayed. Till few months ago, RMS sent mail to ask me
;;        if I'm ready for it, I realized that the release of GNU Artanis
;;        is much delayed.
;;        LibreJS is a way to detect non-free JS code to help you to avoid
;;        the non-trivial JS code for certain hidden features. Some of them
;;        are dangerous, some are stolen your privacy, some are just don't
;;        let you know what's going on.
;;        GNU Artanis may generate JS code automatically, they're licensed as
;;        free software definitely. This free-JS-announcement is used to
;;        license all the generated JS code in the page to GPLv3.
;;        This announcement is useful when you have LibreJS plugin in your
;;        browser to detect the JS automatically.
;;        Although you're free to relicense the code to whatever you prefer,
;;        I wish you could free the code, no matter what license name it is.
(define free-JS-announcement
  "
    <script>
       /*
        @licstart  The following is the entire license notice for the
        JavaScript code in this page.

        Copyright (C) 2014  Loic J. Duros

        The JavaScript code in this page is free software: you can
        redistribute it and/or modify it under the terms of the GNU
        General Public License (GNU GPL) as published by the Free Software
        Foundation, either version 3 of the License, or (at your option)
        any later version.  The code is distributed WITHOUT ANY WARRANTY;
        without even the implied warranty of MERCHANTABILITY or FITNESS
        FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.

        As additional permission under GNU GPL version 3 section 7, you
        may distribute non-source (e.g., minimized or compacted) forms of
        that code without the copy of the GNU GPL normally required by
        section 4, provided you include this license notice and a URL
        through which recipients can access the Corresponding Source.


        @licend  The above is the entire license notice
        for the JavaScript code in this page.
        */
    </script>
")

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
     (DEBUG "pub/~a/~a" (params rc "type") (params rc "static"))
     (static-page-emitter rc)))

  (get "/pub/:static"
    (lambda (rc)
      (DEBUG "pub/~a" (params rc "static"))
      (static-page-emitter rc))))
