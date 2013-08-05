;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  Artanis is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  Artanis is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (artanis upload)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-1)
  #:use-module ((rnrs) #:select (get-bytevector-all utf8->string put-bytevector))
  #:use-module (web request)
  #:export (mfd-simple-dump make-mfd-dumper content-type-is-mfd?
            <mfd> get-mfd-data fine-mfd make-mfd is-mfd? mfds-count
            mfd-dispos mfd-name mfd-filename mfd-data mfd-type
            mfd-simple-dump-all))

;; NOTE: mfd stands for "Multipart Form Data"

(define-record-type <mfd>
  (make-mfd dispos name filename data type)
  is-mfd?
  (dispos mfd-dispos) ; content disposition
  (name mfd-name) ; mfd name
  (filename mfd-filename) ; mfd filename, if not a FILE, then #f
  (data mfd-data) ; the actual data
  (type mfd-type)) ; MIME type

(set-record-type-printer! <mfd>
  (lambda (record port)
    (format port "~%#<mfd dispos: ~a~%      name: ~a~%      filename: ~a~%      type: ~a~%      data: ..."
            (mfd-dispos record) (mfd-name record) (mfd-filename record) (mfd-type record))))

(define (find-mfd name mfd-table)
  (any (lambda (mfd) (and (string=? (mfd-name mfd) name) mfd)) mfd-table))

(define* (get-mfd-data name mfdl #:optional (proc identity))
  (let* ((mfd (find-mfd name mfdl))
         (data (mfd-data mfd)))
    (proc data)))

(define (read-body-line port)
  (read-line port 'concat))

(define (get-mfd-data-string name mfd)
  (get-mfd-data name mfd utf8->string))

(define (%content-type-is-mfd? req)
  (let* ((ct (request-content-type req))
         (type (eq? (car ct) 'multipart/form-data)))
    (if type
        (assoc-ref ct 'boundary) ; is mfd, return the boundary
        #f))) ; not mfd

(define (content-type-is-mfd? rc)
  (%content-type-is-mfd? ((@ (artanis artanis) rc-req) rc)))

(define *valid-meta-header* (make-regexp "Content-Disposition:"))

(define (header-trim s)
  (string-trim-both s (lambda (c) (member c '(#\sp #\" #\return)))))

(define (headline? line)
  (if (regexp-exec *valid-meta-header* line)
      (map (lambda (p) (map header-trim (string-split p #\=)))
                (string-split line #\;))
      #f))

(define (get-type port)
  (let ((type (map header-trim (string-split (read-line port) #\:))))
    (read-line port) ; skip one blank line
    type))

(define (get-data port)
  (get-bytevector-all port)) ; all the rest is the data

(define (blank-line? line)
  (string=? line "\r"))

(define (end-line? line)
  (string=? line "--\r"))

(define (parse-mfd-data str)
  (call-with-input-string str
   (lambda (port)
     (let lp((line (read-line port)) (ret '()))
       (cond
        ((eof-object? line) ret)
        ((or (end-line? line) (blank-line? line)) 
         ;; jump first blank line, it shouldn't effect the blank line in data
         (lp (read-line port) ret))
        ((headline? line)
         => (lambda (ll)
              (let* ((dispos-line (list (map header-trim (string-split (caar ll) #\:))))
                     (dispos (car (assoc-ref dispos-line "Content-Disposition")))
                     (name (car (assoc-ref ll "name")))
                     (filename (cond ((assoc-ref ll "filename") 
                                      => (lambda (x) (and x (car x))))
                                     (else #f)))
                     (type (and filename (get-type port)))
                     (data (get-data port))
                     (mfd (make-mfd dispos name filename data type)))
              (lp (read-line port) (cons mfd ret)))))
        (else (error 'artanis-err 500 "invalid MFD header!" (read-line port))))))))

;; result: (len . parsed-data)
(define (mfd-parser ll)
  (let lp((next ll) (ret '()))
    (cond 
     ((null? next) (cons (length ret) ret))
     ((or (string-null? (car next)) ; boundary itself    
          (string=? (car next) "--")) ; the end
      (lp (cdr next) ret))
     ((parse-mfd-data (car next))
      => (lambda (mfd)
           (lp (cdr next) `(,@mfd ,@ret))))
     (else (error 'artanis-err 422 "Wrong multipart form body!"))))) 

;; utf8->string will allocate a new string, which is inefficient for large upload
;; file, maybe optimize later, but it's better to write a brand new uploader from
;; scratch for a new project.
(define (parse-mfd-body boundary body)
  (let* ((str (utf8->string body))
         (ll (regexp-split boundary str)))
    (mfd-parser (cdr ll)))) ; use cdr skip the first "--"

(define* (make-mfd-dumper #:key (path (current-upload-path)))
  (lambda* (mfd #:key (rename #f) (repath #f))
    (let* ((filename (or rename (mfd-filename mfd)))
           (target-path (or repath path))
           (data (mfd-data mfd)))
      (when filename ; if the mfd is a file
       (let ((real-path (format #f "~a/~a" target-path (dirname filename))))
         (checkout-the-path real-path)     
         (call-with-output-file 
             (format #f "~a/~a" real-path filename)
          (lambda (port)
            (put-bytevector port data))))))))

(define (mfds-count mfds)
  (car mfds))

;; mfd-simple-dump will choose current-upload-path, 
;; with default filename and path
(define (mfd-simple-dump mfd)
  ((make-mfd-dumper) mfd))

(define (mfd-simple-dump-all mfds)
  (for-each mfd-simple-dump (cdr mfds)))
