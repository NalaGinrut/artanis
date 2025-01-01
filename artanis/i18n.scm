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

(define-module (artanis i18n)
  #:use-module (artanis config)
  #:use-module (artanis i18n json)
  #:use-module (artanis i18n po)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 match)
  #:export (make-i18n-handler
            i18n-init
            current-lang
            i18n-handler))

(define i18n-getter (make-parameter #f))

(define current-lang (make-parameter "BUG: the current-lang is not set!"))

(define* (make-i18n-handler)
  (define (->fix lang)
    (let ((encoding (get-conf '(server charset))))
      (string-concatenate (list lang "." encoding))))
  (let* ((lang (current-lang))
         (locale (make-locale (list LC_ALL) (->fix lang))))
    (lambda (pattern)
      (match pattern
        ((? string? key)
         (cond
          ((string-null? lang) key)
          ((i18n-getter)
           => (lambda (getter)
                (or (getter lang key)
                    key)))
          (else key)))
        (('money money)
         ;; NOTE: We recommend the name of currency rather than the symbol.
         ;;       Say, JPY rather than ¥. Because some of the symbols are
         ;;       impossible to distinguish from each other. For example,
         ;;       the symbol of CNY RMB and JPY Yen are both ¥.
         ;; NOTE: Of course I know CNY is actually ￥, and JPY is actually ¥.
         ;;       But they are the same string detected from GNU libc.
         (let ((num (if (number? money) money (string->number money))))
           (monetary-amount->locale-string num #t locale)))
        (('moneysign money)
         (let ((num (if (number? money) money (string->number money))))
           ;; NOTE: The GNU lib on Linux will add "-" automatically for
           ;;       historical reasons. Say, "-$", so we have to drop it.
           (string-trim
            (monetary-amount->locale-string num #f locale)
            #\-)))
        (('number number fraction)
         (number->locale-string number fraction locale))
        (('local-date seconds)
         (strftime (locale-date-format locale) (localtime seconds)))
        (('global-date seconds)
         (strftime (locale-date-format locale) (gmtime seconds)))
        (('local-time seconds)
         (strftime (locale-time-format locale) (localtime seconds)))
        (('global-time seconds)
         (strftime (locale-time-format locale) (gmtime seconds)))
        (('weekday weekday)
         (locale-day weekday locale))
        (('month month)
         (locale-month month locale))
        (else (throw 'artanis-error 500 make-i18n-handler
                     "Unknown i18n pattern" pattern))))))

(define (i18n-init)
  (let ((i18n-mode (get-conf '(session i18n))))
    (case i18n-mode
      ((json) (i18n-getter (i18n-json-init)))
      ((po) (i18n-getter (i18n-po-init)))
      (else (error "Unknown i18n mode" i18n-mode)))))
