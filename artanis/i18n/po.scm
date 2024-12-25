;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2025
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

(define-module (artanis i18n po)
  #:export (i18n-po-init))

;; NOTE:
;; Gettext is not thread safe, even worse, it is not reentrant due to
;; relying on global locale configuration, so it's NEITHER coroutine safe.
;; If we still want to take advantage of PO, we have to implement our own
;; PO parser and translator.
;;
;; NOTE:
;; It's meaningless to use MO since you still have to convert it to
;; Scheme hashtable.

(define (i18n-po-ref lang key)
  #f)

;; TODO:
;; 1. Detect PO with the locale manually;
;; 2. Parse PO file and convert it to Scheme hashtable;
(define (i18n-po-init)
  ;; not implemented yet
  (error "i18n PO mode is not implemented yet")
  i18n-po-ref)
