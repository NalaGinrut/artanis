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

(define-module (artanis commands version)
  #:use-module (artanis version)
  #:use-module (ice-9 command-line)
  #:export (show-version))

(define copyright-year "2013~2015")
(define author-str "NalaGinrut <mulei@gnu.org>\n")

(define version-str
  (string-append
   (with-output-to-string
    (lambda ()
      (version-etc "GNU Artanis is a lightweight web framework written in Guile Scheme.\n" ""
		   #:copyright-holder "Mu Lei known as NalaGinrut" #:license "GPLv3+ & LGPLv3+" #:copyright-year copyright-year)))
;;   (show-commands)
   author-str
   "Version: " artanis-version ".\n"
   "God bless hacking.\n\n")) 

(define (show-version) (display version-str))

(define %summary "Show the version info")
(define main show-version)
