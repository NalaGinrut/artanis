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

(define-module (artanis mvc route)
  #:use-module (artanis artanis)
  #:use-module (artanis utils)
  #:export (define-artanis-customized-router))

(define-syntax-rule (define-artanis-customized-router)
  (begin
    ;; NOTE: We use __fake to note that it's a `fake' module, since we are not going to
    ;;       load it as module, but we need to use module for encapsulating symbols.
    (define-module (app __fake router)
      #:use-module (artanis artanis)
      #:use-module (artanis utils)
      #:use-module (artanis mvc route))))

;; TODO: customized router helper functions
