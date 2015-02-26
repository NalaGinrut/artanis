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
  (display "[USAGE] art create proj-name\n"))

(define (create-project name)
  (define fullname (string-append (getcwd) "/" name))
  (cond
   ((file-exists? fullname)
    (format #t
            "`~a' exists, please choose a new name or remove the existed one!~%"
            name))
   (else
    (format #t "creating ~a...~%" name))))

(define (create . args)
  (match args
    (((or () "help" "-h")) (show-help))
    ((name) (create-project name))
    (else (show-help))))

(define main create)
