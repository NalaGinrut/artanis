;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015
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

(define-module (artanis commands)
  #:use-module (artanis version)
  #:export (find-command
            no-command?
            no-command-args?
            announce-head
            announce-foot))

(define (no-command? args)
  (< (length args) 2))

(define (no-command-args? args)
  (< (length args) 3))

(define command-path '(artanis commands))

(define (find-command name)
  (and=> (resolve-module `(,@command-path ,(string->symbol name)) #:ensure #f)
         (lambda (m) (values name m))))

(define announce-head
  "
GNU Artanis is a monolithic framework written in Guile Scheme.
NalaGinrut <mulei@gnu.org>
")

(define announce-foot
  (format #f "~%~a~%Version: ~a.~%God bless hacking.~%~%" "GPLv3+ & LGPLv3+" artanis-version))
