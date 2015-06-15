#!/usr/bin/env guile
!#

;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015
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

(use-modules (artanis artanis))

;; the example of multi files upload
(init-server)

(define upload-form
  '(form (@ (method "POST") (enctype "multipart/form-data") (action "upload"))
         "File to upload: " (input (@ (type "file") (name "upfile"))) (br)
         "Notes about the file: " (input (@ (type "text") (name "note")))
         (br) (br)
         (input (@ (type "submit") (value "Press")) "to upload the file!")))

(get "/upload" (lambda () (tpl->response upload-form)))

(post "/upload" #:from-post '(store #:path "upload")
  (lambda (rc)
    (case (:from-post rc 'store)
      ((success) (response-emit "upload succeeded!"))
      ((none) (response-emit "No uploaded files!"))
      (else (response-emit "Impossible! please report bug!")))))

(run)
