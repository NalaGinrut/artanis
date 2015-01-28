#! /usr/bin/env guile
!#
;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
;;  This file is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License and GNU
;;  Lesser General Public License published by the Free Software
;;  Foundation, either version 3 of the License, or (at your option)
;;  any later version.

;;  This file is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License and GNU Lesser General Public License
;;  for more details.

;;  You should have received a copy of the GNU General Public License
;;  and GNU Lesser General Public License along with this program.
;;  If not, see <http://www.gnu.org/licenses/>.

;; NOTE: It's not fair to check GNU Texinfo in configure, since users
;;       don't have to install it unless they want to generate manuals.
(use-modules (ice-9 match))

(define *tpl*
"version: ~a
directory: artanis
filename: artanis-~a.tar.~a
symlink: artanis-~a.tar.~a artanis-latest.tar.~a
comment: make a release")

(define (main args)
  (define (->f e v) (format #f "artanis-~a.tar.~a.directive" v e))
  (define (->t e v) (format #f *tpl* v v e v e e))
  (match args
    ((cmd ext ver)
     (let* ((f (->f ext ver))
            (t (->t ext ver))
            (fp (open-file f "w")))
       (display t fp)
       (close fp)))
    (else (error "[usage] gen-directive.scm ext version"))))

(main (command-line))
