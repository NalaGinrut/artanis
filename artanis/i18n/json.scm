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

(define-module (artanis i18n json)
  #:use-module (artanis utils)
  #:use-module (artanis third-party json)
  #:use-module (artanis irregex)
  #:use-module (ice-9 ftw)
  #:export (i18n-json-init))

(define (i18n-json-dir) (format #f "~a/sys/i18n/json/" (current-toplevel)))

(define *i18n-json-table* (make-hash-table))
(::define (i18n-json-ref lang key)
  (:anno: (string string) -> ((or string #f)))
  (let ((trans (hash-ref *i18n-json-table* lang)))
    (and trans (json-ref trans key))))

(define *i18n-json-file-re* (string->irregex "(.*)\\.json$"))

(define (i18n-json-init)
  (define (load-json-file file)
    (let ((locale (irregex-match *i18n-json-file-re* file)))
      (cond
       ((not locale) (error "i18n: Invalid json file name: ~a" file))
       (hash-set! *i18n-json-table*
                  (irregex-match-substring locale 1)
                  (call-with-input-file file json->scm)))))
  (let ((i18n-json-dir (i18n-json-dir)))
    (when (not (file-exists? i18n-json-dir))
      (mkdir i18n-json-dir))
    (scandir i18n-json-dir
             (lambda (file)
               (cond
                ((or (string=? ".") (string=? "..")) #f)
                (else (load-json-file file)))))
    ;; return the i18n-json-ref function for registering
    i18n-json-ref))
