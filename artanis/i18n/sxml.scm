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

(define-module (artanis i18n sxml)
  #:use-module (artanis env)
  #:use-module (artanis utils)
  #:use-module (artanis irregex)
  #:use-module (artanis config)
  #:use-module (ice-9 ftw)
  #:export (i18n-sxml-init
            i18n-sxml-dir))

(define (i18n-sxml-dir)
  (format #f "~a/sys/i18n/sxml"
          (or (default-i18n-path) (current-toplevel))))

(define *i18n-sxml-table* (make-hash-table))

(::define (i18n-sxml-single-ref lang key)
  (:anno: (string string) -> ((or string #f)))
  (let ((trans (hash-ref *i18n-sxml-table* lang)))
    (and trans (assoc-ref trans key))))

(::define (i18n-sxml-plural-ref lang key)
  (:anno: (string string +int) -> string)
  (throw 'artanis-error 500 i18n-sxml-plural-ref
         "Function is not implemented yet."))

(define *i18n-sxml-file-re* (string->irregex "(.*)\\.scm$"))

(define (i18n-sxml-init)
  (define (load-sxml-file file)
    (let ((path (string-append (i18n-sxml-dir) "/" file))
          (locale (irregex-match *i18n-sxml-file-re* file)))
      (cond
       ((not locale) (error "i18n: Invalid sxml file name: ~a" file))
       (else
        (hash-set! *i18n-sxml-table*
                   (string-append (irregex-match-substring locale 1)
                                  "." (get-conf '(server charset)))
                   (call-with-input-file path read))))))
  (let ((dir (i18n-sxml-dir)))
    (when (not (file-exists? dir))
      (mkdir dir))
    (for-each load-sxml-file
              (scandir dir
                       (lambda (f)
                         (and
                          (not (or (string=? f ".") (string=? f "..")))
                          (irregex-match *i18n-sxml-file-re* f)))))
    ;; return the i18n-sxml-ref function for registering
    (values i18n-sxml-single-ref
            i18n-sxml-plural-ref)))
