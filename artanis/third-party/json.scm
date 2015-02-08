;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014,2015
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

;;; Commentary:

;; JSON module for Guile

;;; Code:

(define-module (artanis third-party json)
  #:use-module (artanis third-party json upstream builder)
  #:use-module (artanis third-party json upstream parser)
  #:use-module (artanis third-party json upstream syntax)
  #:export (->json-string)
  #:re-export (scm->json
               scm->json-string
               json->scm
               json-string->scm
               json-parser?
               json-parser-port
               json))

(define* (->json-string sxml #:key (jsonp #f))
  (if jsonp
      (format #f "~a(~a)" jsonp (scm->json-string sxml))
      (scm->json-string sxml)))
