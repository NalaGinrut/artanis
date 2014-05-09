;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014
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
  #:use-module (artanis irregex)
  #:use-module (artanis mime)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-1)
  #:use-module ((rnrs)
                #:select (get-bytevector-all utf8->string put-bytevector
                          call-with-bytevector-output-port
                          bytevector-length))
  #:use-module (web request)
  #:use-module (web client)
  #:use-module (web uri)
  #:export (mfd-simple-dump
            make-mfd-dumper
            content-type-is-mfd?
            parse-mfd-body
            <mfd>
            get-mfd-data
            fine-mfd
            make-mfd
            is-mfd?
            mfds-count
            mfd-dispos
            mfd-name
            mfd-filename
            mfd-data
            mfd-type
            mfd-simple-dump-all
            store-uploaded-files
            upload-files-to))

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
    (format port "~%#<mfd dispos: ~a~%      name: ~a~%      filename: ~a~%      type: ~a~%      data: ...>"
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

(define *valid-meta-header* (string->irregex "Content-Disposition:"))

(define (header-trim s)
  ;; NOTE: We need to trim #\return.
  ;;       Since sometimes we encounter "\n\r" rather than "\n" as newline.
  (string-trim-both s (lambda (c) (member c '(#\sp #\" #\return)))))

(define (headline? line)
  (irregex-search *valid-meta-header* line))

(define (get-data port)
  (get-bytevector-all port)) ; all the rest is the data

(define (blank-line? line)
  (string-null? (string-trim-both line)))

(define (end-line? line)
  (string=? "--" (string-trim-both line)))

(define (->mfd-headers line port)
  (define (-> l)
    (define (-? x) (= 1 (length x)))
    (let lp((n l) (ret '()))
      (cond
       ((null? n) (reverse! ret))
       (else
        (let* ((p (car n))
               (z (string-split p #\=))
               (y (if (-? z)
                      (string-trim-both p)
                      (map header-trim z))))
          (lp (cdr n) (cons y ret)))))))
  (define (->p ll)
    (match (string-split ll #\:)
      ((k v)
       (-> `(,k ,@(string-split v #\;))))
      (else (throw 'artanis-err 400 "->mfd-headers: Invalid MFD header!" ll))))
  (let lp((l (read-line port)) (ret (list (->p line))))
    (cond
     ((blank-line? l) ret)
     (else
      (lp (read-line port) (cons (->p l) ret))))))

(define (parse-mfd-data str)
  (define-syntax-rule (-> h k) (and=> (assoc-ref h k) car))
  (call-with-input-string str
   (lambda (port)
     (let lp((line (read-line port)) (ret '()))
       (cond
        ((eof-object? line) ret)
        ((or (end-line? line) (blank-line? line)) 
         ;; jump first blank line, it shouldn't effect the blank line in data
         (lp (read-line port) ret))
        ((headline? line)
         (let* ((headers (->mfd-headers line port))
                (data (get-data port))
                (dispos (assoc-ref headers "Content-Disposition"))
                (filename (-> dispos "filename"))
                (name (-> dispos "name"))
                (type (-> headers "Content-Type"))
                (mfd (make-mfd (car dispos) name filename data type)))
           (lp (read-line port) (cons mfd ret))))
        (else (throw 'artanis-err 400 "Invalid Multi Form Data header!" line)))))))

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
     (else (throw 'artanis-err 422 "Wrong multipart form body!"))))) 

;; bytevector->string will allocate a new string, which is inefficient for large upload
;; file, maybe optimize later, but it's better to write a brand new uploader from
;; scratch for a new project.
;; NOTE: we use iso8859-1 or we can't handle general binary file
(define (parse-mfd-body boundary body)
  (let* ((str (bytevector->string body "iso8859-1"))
         (ll (irregex-split (format #f "(\n|\r\n)?(--)?~a" boundary) str)))
    (mfd-parser ll)))

(define (handle-proper-owner file uid gid)
  (chown file (or uid (getuid)) (or gid (getgid))))

(define* (make-mfd-dumper #:key (path (current-upload-path))
                          (uid #f) (gid #f)
                          (mode #o664) (path-mode #o775) (sync #f))
  (lambda* (mfd #:key (rename #f) (repath #f))
    (let ((filename (or rename (mfd-filename mfd)))
          (target-path (or repath path))
          (data (mfd-data mfd)))
      (when filename ; if the mfd is a file
       (let* ((real-path (format #f "~a/~a" target-path (dirname filename)))
              (des-file (format #f "~a/~a" real-path filename)))
         (checkout-the-path real-path path-mode)
         (when (file-exists? des-file)
           (delete-file des-file))
         (call-with-output-file des-file
          (lambda (port)
            (put-bytevector port data)
            (and sync (force-output port))))
         (handle-proper-owner des-file uid gid)
         (chmod des-file mode))))))

(define (mfds-count mfds)
  (car mfds))

;; mfd-simple-dump will choose current-upload-path, 
;; with default filename and path
(define (mfd-simple-dump mfd)
  ((make-mfd-dumper) mfd))

(define (mfd-simple-dump-all mfds)
  (for-each mfd-simple-dump (cdr mfds)))

;; NOTE: we won't limit file size here, since it should be done in server reader
(define* (store-uploaded-files rc #:key (path (current-upload-path))
                               (uid #f) (gid #f)
                               (mode #o664) (path-mode #o775) (sync #f))
  (cond
   ((content-type-is-mfd? rc)
    => (lambda (boundry)
         (let ((mfds (parse-mfd-body boundry ((@ (artanis artanis) rc-body) rc)))
               (dumper (make-mfd-dumper #:path path #:mode mode #:uid uid #:gid gid
                                        #:path-mode path-mode #:sync sync)))
           (catch #t
             (lambda () (for-each dumper (cdr mfds)))
             (lambda e (throw 'artanis-err 500 "Failed to dump mfds!" e)))
           'success)))
   (else 'none)))

(define (mfds->body mfds boundary)
  (call-with-bytevector-output-port
   (lambda (port)
     (for-each
      (lambda (mfd)
        (when (not (is-mfd? mfd))
          (throw 'artanis-err 500 "mfds->body: Invalid <mfd> type!" mfd))
        (display (mfd-dispos mfd) port)
        (put-bytevector port (mfd-data mfd)))
      mfds)
     (display (string-append "\r\n--" boundary "--\r\n") port))))

;; FIXME: This is a temp path, should use (get-conf "upload") instead
(define current-upload-path (make-parameter "upload/"))

;; Usage: the pattern should be:
;; '((file filelist ...) (data datalist ...))
;; e.g
;;  '((data ("data1" "hello world"))
;;    (file ("file1" "filename") ("file2" "filename2")))
;; * You may ignore `data' part if you just want to upload files.
;; * The name field, say, "file1" is optional.
(define (upload-files-to uri pattern)
  (define boundary "----------Artanis0xDEADBEEF")
  (define-syntax-rule (->dispos name filename mime)
    (call-with-output-string
     (lambda (port)
       (format port "--~a\r\n" boundary)
       (format port "Content-Disposition: form-data; name=~s" name)
       (and filename
            (format port "; filename=~s" filename))
       (and mime (format port "\r\nContent-Type: ~a" mime))
       (display "\r\n\r\n" port))))
  (define-syntax-rule (->file file)
    (if (file-exists? file)
        (call-with-input-file file get-bytevector-all)
        (throw 'artanis-err 500 "This file doesn't exist!" file)))
  (define (->mfds pattern)
    (define-syntax-rule (-> w builder)
      (if w (map builder w) '()))
    (let ((files (assoc-ref pattern 'file))
          (datas (assoc-ref pattern 'data)))
      (append (-> files build-file-mfd)
              (-> datas build-data-mfd))))
  (define (build-data-mfd p)
    (match p
      ((name value)
       (make-mfd (->dispos name #f #f)
                 name
                 #f
                 value
                 #f))
      (else (throw 'artanis-err 500 "build-data-mfd: Invalid pattern"))))
  (define (build-file-mfd p)
    (match p
      ((name filename)
       (let ((mime (mime-guess (get-file-ext filename))))
         (make-mfd (->dispos name filename mime)
                   name
                   filename
                   (->file filename)
                   mime)))
      ((filename)
       ;; If name field is ingored, use "file" as its name in default.
       (build-file-mfd `("file" ,filename)))
      (else (throw 'artanis-err 500 "build-file-mfd: Invalid pattern"))))
  (let* ((mfds (->mfds pattern))
         (ct (format #f "multipart/form-data; boundary=~a" boundary))
         (body (mfds->body mfds boundary)))
    ;; NOTE:
    ;; Content-Length contains body length only
    ;; Guile web module will count Content-Length for you.
    (http-post uri
               #:body body
               #:headers `((Content-Type . ,ct)))))
