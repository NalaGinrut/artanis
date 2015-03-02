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

(define-module (artanis commands create)
  #:use-module (ice-9 match))

(define %summary "Create a new Artanis project.")

(define (show-help)
  (display "[USAGE] art create proj-path\n"))

(define conf-header
"##  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
##  Copyright (C) 2014
##      \"Mu Lei\" known as \"NalaGinrut\" <NalaGinrut@gmail.com>
##  Artanis is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.

##  Artanis is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.

##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <http://www.gnu.org/licenses/>.

## ---------------------------------------------------------------------
## The skeleton of config file, you may modify it on your own purpose.
## DON'T TYPE `;' AT THE END OF LINE!!!
## ---------------------------------------------------------------------

## Please read the manual or /etc/artanis/default.conf if you have problem
## to understand these items.\n
")

(define conf-footer "\n\n## End Of Artanis conf.\n")

(define (create-local-config)
  (define (->proper v)
    (match v
      ((or #t 'true 'on 'yes) 'enable)
      ((or #f 'false 'off 'no) 'disable)
      ((? list?) (format #f "~{~a~^,~}" v))
      (else v)))
  (define (->cstr ctb)
    (call-with-output-string
     (lambda (port)
       (for-each (lambda (c)
                   (match c
                     ((ns val)
                      (format port "~{~a~^.~} = ~a~%" ns (->proper val)))
                     (else (error create-local-config "BUG: Invalid conf value!" c))))
                 ctb))))
  (let* ((ctb (@@ (artanis config) *default-conf-values*))
         (cstr (->cstr ctb))
         (fp (open-file "artanis.conf" "w")))
    (display conf-header fp)
    (display cstr fp)
    (display conf-footer fp)
    (close fp)))

(define *dir-arch*
  '((app (model controller view)) ; MVC stuff
    (sys (pages i18n)) ; system stuff
    (fprm) ; FPRM scripts
    (log) ; log files
    (lib) ; libs
    (pub ((img (upload)) css js)) ; public assets
    (prv) ; private stuff, say, something dedicated config or tokens
    (tmp (cache)) ; temporary files
    (test (unit functional benchmark)))) ; tests stuffs

;; Simple recursive depth-first order traverser for generic tree (in list).
;; We use this function for making *dir-arch* directory tree, it's little data,
;; so we don't care about the performance very much.
(define (dfs t p l)
  (match t
    (() #t)
    (((r (children ...)) rest ...)
     (p r l)
     (for-each (lambda (x) (dfs (list x) p (cons r l))) children)
     (dfs rest p l))
    (((r) rest ...)
     (p r l)
     (dfs rest p l))
    ((children ...)
     (p (car children) l)
     (dfs (cdr children) p l))
    (else (error dfs "BUG: Impossible pattern! Please report it!" t))))

(define (create-framework)
  (define (->path x l)
    (format #t "~{~a~^/~}~%" (reverse (cons x l))))
  (dfs *dir-arch* (lambda (x l) (mkdir (->path x l))) '()))

(define (create-project name)
  (cond
   ((file-exists? name)
    (format #t
            "`~a' exists, please choose a new name or remove the existed one!~%"
            name))
   (else
    (format #t "creating ~a......" name)
    (mkdir name)
    (chdir name)
    (create-local-config)
    (create-framework)
    (create-entry)
    )))

(define (create . args)
  (match args
    (((or () "help" "-h")) (show-help))
    ((name) (create-project name))
    (else (show-help))))

(define main create)
