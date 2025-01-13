;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2025
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

(define-module (artanis i18n locale)
  #:use-module (artanis env)
  #:use-module (artanis utils)
  #:use-module (artanis irregex)
  #:use-module (artanis cli)
  #:use-module (ice-9 threads) ; for monitor
  #:export (i18n-locale-init
            i18n-locale-mo-dir
            i18n-locale-domain))

(define i18n-locale-mo-dir
  (make-parameter (format #f "~a/sys/i18n/locale" (current-toplevel))))

(define i18n-locale-domain (make-parameter "translate"))

(define *num-re* (string->irregex "%d"))

(define (fix-num str)
  (irregex-replace/all *num-re* str "~a"))

;; NOTE:
;; gettext is not thread safe, so we need monitor to protect it.
(::define (i18n-locale-single-ref lang key)
  (:anno: (string string) -> string)
  (monitor
   (let ((old-locale ""))
     (dynamic-wind
         (lambda ()
           (let ((cur-locale (setlocale LC_MESSAGES "")))
             (set! old-locale cur-locale)
             (setlocale LC_MESSAGES lang)))
         (lambda ()
           (fix-num (gettext key (current-locale-domain) LC_MESSAGES)))
         (lambda ()
           (setlocale LC_MESSAGES old-locale))))))

(::define (i18n-locale-plural-ref lang single plural num)
  (:anno: (string string +int) -> string)
  (monitor
   (let ((old-locale ""))
     (dynamic-wind
         (lambda ()
           (let ((cur-locale (setlocale LC_MESSAGES "")))
             (set! old-locale cur-locale)
             (setlocale LC_MESSAGES lang)))
         (lambda ()
           (fix-num (ngettext single plural num (i18n-locale-domain) LC_MESSAGES)))
         (lambda ()
           (setlocale LC_MESSAGES old-locale))))))

(define (i18n-locale-init)
  (let ((dir (i18n-locale-mo-dir))
        (domain (i18n-locale-domain)))
    (cli-run* mkdir -p ,dir)
    (bindtextdomain domain dir)
    (textdomain domain)
    (values i18n-locale-single-ref i18n-locale-plural-ref)))
