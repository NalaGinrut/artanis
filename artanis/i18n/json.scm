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

(define-module (artanis i18n json)
  #:use-module (artanis env)
  #:use-module (artanis utils)
  #:use-module (artanis third-party json)
  #:use-module (artanis irregex)
  #:use-module (artanis config)
  #:use-module (ice-9 ftw)
  #:export (i18n-json-init
            i18n-json-dir))

(define (i18n-json-dir)
  (format #f "~a/sys/i18n/json"
          (or (default-i18n-path) (current-toplevel))))

(define *i18n-json-table* (make-hash-table))

(::define (i18n-json-single-ref lang key)
  (:anno: (string string) -> ((or string #f)))
  (let ((trans (hash-ref *i18n-json-table* lang)))
    (and trans (json-ref trans key))))

(::define (i18n-json-plural-ref lang key)
  (:anno: (string string +int) -> string)
  (throw 'artanis-error 500 i18n-json-plural-ref
         "Function is not implemented yet."))

(define *i18n-json-file-re* (string->irregex "(.*)\\.json$"))

(define (i18n-json-init)
  (define (load-json-file file)
    (let ((path (string-append (i18n-json-dir) "/" file))
          (locale (irregex-match *i18n-json-file-re* file)))
      (cond
       ((not locale) (error "i18n: Invalid json file name: ~a" file))
       (else
        (hash-set! *i18n-json-table*
                   (string-append (irregex-match-substring locale 1)
                                  "." (get-conf '(server charset)))
                   (call-with-input-file path json->scm))))))
  (let ((dir (i18n-json-dir)))
    (when (not (file-exists? dir))
      (mkdir dir))
    (for-each load-json-file
              (scandir dir
                       (lambda (f)
                         (and
                          (not (or (string=? f ".") (string=? f "..")))
                          (irregex-match *i18n-json-file-re* f)))))
    ;; return the i18n-json-ref function for registering
    (values i18n-json-single-ref
            i18n-json-plural-ref)))
