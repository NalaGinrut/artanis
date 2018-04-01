;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2017,2018
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

(define-module (artanis upload)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis irregex)
  #:use-module (artanis mime)
  #:use-module (artanis route)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module ((rnrs)
                #:select (get-bytevector-all utf8->string put-bytevector
                                             call-with-bytevector-output-port string->utf8
                                             bytevector-length define-record-type
                                             bytevector-u8-ref))
  #:use-module (web request)
  #:use-module (web client)
  #:use-module (web uri)
  #:export (mfd-simple-dump
            make-mfd-dumper
            content-type-is-mfd?
            parse-mfd-body
            mfd
            call-with-mfd-data
            find-mfd
            make-mfd
            mfd?
            is-mfds?
            mfds-count
            mfd-dispos
            mfd-name
            mfd-filename
            mfd-begin
            mfd-end
            mfd-type
            mfd-simple-dump-all
            store-uploaded-files
            get-mfds-op-from-post
            mfds-operator?
            mfds-operator-mfds
            mfds-operator-dumper
            mfds-op-ref
            mfds-op-store
            upload-files-to
            current-upload-path))

;; NOTE: mfd stands for "Multipart Form Data"

(define-record-type mfd
  (fields
   dispos ; content disposition
   name ; mfd name
   filename ; mfd filename, #f for not-a-file
   begin ; beginning of data
   end ; end of data
   type)) ; MIME type

(define (mfd->utf8 rc mfd)
  (subbv->string (rc-body rc) "utf-8" (mfd-begin mfd) (- (mfd-end mfd) 2)))
(define (find-mfd rc name mfds)
  (any (lambda (mfd)
         (cond
          ((string=? (mfd-name mfd) name)
           (mfd->utf8 rc mfd))
          (else #f)))
       mfds))

(define (call-with-mfd-data rc name mfds proc)
  (let ((mfd (find-mfd rc name mfds)))
    (proc rc (mfd-begin mfd) (mfd-end mfd))))

(define (%content-type-is-mfd? req)
  (let* ((ct (request-content-type req))
         (type (eq? (car ct) 'multipart/form-data)))
    (if type
        (assoc-ref ct 'boundary) ; is mfd, return the boundary
        #f))) ; not mfd

(define (content-type-is-mfd? rc)
  (%content-type-is-mfd? (rc-req rc)))

(define *valid-meta-header* (string->utf8 "Content-Disposition:"))
(define meta-header-length (bytevector-length *valid-meta-header*))
(define (headline? body boundary from)
  (define blen (bytevector-length boundary))
  (let ((start (+ from blen 2)))
    (and (subbv=? body boundary from (+ from blen -1)) ; first line is boundary
         (subbv=? body *valid-meta-header* start
                  (+ start meta-header-length -1)))))

(define (header-trim s)
  ;; NOTE: We need to trim #\return.
  ;;       Since sometimes we encounter "\n\r" rather than "\n" as newline.
  (string-trim-both s (lambda (c) (member c '(#\sp #\" #\return)))))

(define (->mfd-header line)
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
  (match (string-split line #\:)
    ((k v)
     (-> `(,k ,@(string-split v #\;))))
    (else (throw 'artanis-err 400 ->mfd-header
                 "->mfd-headers: Invalid MFD header!" line))))

;; NOTE: convert boundary to bytevector before pass in.
(define (parse-mfds boundary body)
  (define-syntax-rule (-> h k) (and=> (assoc-ref h k) car))
  (define len (bytevector-length body))
  (define blen (bytevector-length boundary))
  (define (is-boundary? from)
    (subbv=? body boundary from (+ from blen -1)))
  (define (is-end-line? from)
    (and (is-boundary? from)
         (subbv=? body #vu8(45 45) (+ from blen) (+ from blen 1))))
  (define (blankline? from)
    (subbv=? body #u8(13 10) from (+ from 1)))
  (define (get-headers from)
    (cond
     ((headline? body boundary from)
      (let lp((start (+ from blen 2)) (end (bv-read-line body (+ from blen 2))) (ret '()))
        (cond
         ((blankline? start) (cons ret (1+ end))) ; end of headers
         (else
          (let ((line (subbv->string body "utf-8" start end))
                (llen (- end start -1)))
            (lp (+ start llen) (bv-read-line body (+ start llen))
                (cons (->mfd-header line) ret)))))))
     (else (throw 'artanis-err 400 get-headers
                  "Invalid Multi Form Data header!" body))))
  (define (get-content-end from)
    (define btable (build-bv-lookup-table boundary))
    (let lp((i from))
      (cond
       ((and (< (+ i blen) len)
             (not (hash-ref btable (bytevector-u8-ref body (+ i blen -1)))))
        (lp (+ i blen)))
       ((is-boundary? i) i)
       (else (lp (1+ i))))))
  ;; ENHANCEMENT: use Fast String Matching to move forward quicker
  ;;(lp (1+ (bv-read-line body i)))))))
  (let lp((i 0) (mfds '()))
    (cond
     ((is-end-line? i) mfds)
     ((<= i len)
      (let* ((hp (get-headers i))
             (headers (car hp))
             (start (cdr hp))
             (end (get-content-end start))
             (dispos (assoc-ref headers "Content-Disposition"))
             (filename (-> dispos "filename"))
             (name (-> dispos "name"))
             (type (-> headers "Content-Type"))
             (mfd (make-mfd (car dispos) name filename start end type)))
        (lp end (cons mfd mfds))))
     (else (throw 'artanis-err 422 parse-mfds
                  "Wrong multipart form body!")))))

(define* (make-mfd-dumper body #:key (path (current-upload-path))
                          (uid #f) (gid #f) (path-mode #o775)
                          (mode #o664) (sync #f))
  (define (gen-new-name oldname renamer)
    (cond
     ((not renamer)  oldname)
     ((string? renamer) renamer)
     ((procedure? renamer)
      (let ((newname (renamer oldname)))
        (if (string? newname)
            newname
            (throw 'artanis-err 500 gen-new-name
                   (format #f "Somehow your renamer generated a bad name ~a!"
                           newname)))))
     (else (throw 'artanis-err 500 gen-new-name
                  (format #f "Invalid renamer ~a!" renamer)))))
  (lambda* (mfd #:key (renamer #f) (repath #f))
    (let* ((oldname (mfd-filename mfd))
           (filename (or (gen-new-name oldname renamer) oldname))
           (target-path (or repath path)))
      ;; iif the mfd is a valid file
      (when (and filename (string? filename) (not (string-null? filename)))
        (let* ((real-path (format #f "~a/~a" target-path (dirname filename)))
               (des-file (format #f "~a/~a" real-path filename)))
          (checkout-the-path real-path path-mode)
          (let ((fp (open-file des-file "wb")))
            (put-bv fp body (mfd-begin mfd) (mfd-end mfd))
            (and sync (force-output fp)))
          (handle-proper-owner des-file uid gid)
          (chmod des-file mode))))))

;; mfd-simple-dump will choose current-upload-path,
;; with default filename and path
(define (mfd-simple-dump rc mfd)
  ((make-mfd-dumper (rc-body rc)) mfd))

(define (mfd-simple-dump-all rc mfds)
  (let ((md (make-mfd-dumper (rc-body rc))))
    (for-each md mfds)))

(define (mfd-size m)
  (and (mfd-filename m)
       ;; NOTE: There'd be 3 chars need to be dropped, \r\n(head) and \r(tail)
       ;; (- (- (mfd-end m) (mfd-begin m) -1) 3)
       (- (mfd-end m) (mfd-begin m) 2)))

;; NOTE: we won't limit file size here, since it should be done in server reader
(define* (store-uploaded-files rc #:key (path (current-upload-path))
                               (uid #f) (gid #f) (simple-ret? #t) (need-mfd? #f)
                               (mode #o664) (path-mode #o775) (sync #f))
  (define (get-slist mfds) (filter-map mfd-size mfds))
  (define (get-flist mfds) (filter-map mfd-filename mfds))
  (cond
   ((content-type-is-mfd? rc)
    => (lambda (boundry)
         (let ((mfds (parse-mfds (string->utf8 (string-append "--" boundry))
                                 (rc-body rc)))
               (dumper (make-mfd-dumper (rc-body rc) #:path path #:mode mode
                                        #:uid uid #:gid gid #:path-mode path-mode
                                        #:sync sync)))
           (catch #t
             (lambda () (for-each dumper mfds))
             (lambda e (format (current-error-port) "[WARNING] Failed to dump mfds! ~a~%" e)))
           (if simple-ret?
               (if need-mfd? mfds 'success)
               (if need-mfd?
                   `(success ,mfds ,(get-slist mfds) ,(get-flist mfds))
                   `(success ,(get-slist mfds) ,(get-flist mfds)))))))
   (else 'none)))

(define-record-type mfds-operator
  (fields
   mfds ; the list of mfd
   dumper)) ; the dumper

(define (mfds-op-ref rc mo key)
  (let* ((mfds (mfds-operator-mfds mo))
         (value-mfd (any (lambda (mfd)
                           (and (string=? key (mfd-name mfd))
                                mfd))
                         mfds)))
    (if value-mfd
        (mfd->utf8 rc value-mfd)
        (throw 'artanis-err 500 mfds-op-ref
               (format #f "No mfd field named ~a!" key)))))

(define (mfds-op-store mo . args)
  (let ((file-mfds (mfds-operator-mfds mo))
        (dumper (mfds-operator-dumper mo)))
    (handle-upload
     (lambda ()
       (for-each (lambda(mfd)
                   (apply dumper mfd args)) file-mfds)))))

(define* (get-mfds-op-from-post rc #:key (path (current-upload-path))
                                (uid #f) (gid #f) (simple-ret? #t) (need-mfd? #f)
                                (mode #o664) (path-mode #o775) (sync #f))
  (cond
   ((content-type-is-mfd? rc)
     => (lambda (boundry)
          (let ((mfds (parse-mfds (string->utf8 (string-append "--" boundry))
                                  (rc-body rc)))
                (dumper (make-mfd-dumper (rc-body rc) #:path path #:mode mode
                                         #:uid uid #:gid gid #:path-mode path-mode
                                         #:sync sync)))
            (make-mfds-operator mfds dumper))))
   (else (make-mfds-operator '() identity))))

(define (mfds->body mfdsp boundary)
  (call-with-bytevector-output-port
   (lambda (port)
     (for-each
      (lambda (mfdp)
        (let ((mfd (car mfdp))
              (data (cdr mfdp)))
          (when (not (mfd? mfd))
            (throw 'artanis-err 500 mfds->body "Invalid <mfd> type: ~a!" mfd))
          (display (mfd-dispos mfd) port)
          (put-bv port data (mfd-begin mfd) (mfd-end mfd))))
      mfdsp)
     (display (string-append "\r\n--" boundary "--\r\n") port))))

(define (current-upload-path) (get-conf '(upload path)))

;; Usage: the pattern should be:
;; '((file filelist ...) (data datalist ...))
;; e.g
;;  '((data ("data1" "hello world"))
;;    (file ("file1" "filename") ("file2" "filename2")))
;; * You may ignore `data' part if you just want to upload files.
;; * The name field, say, "file1" is optional.
(define (upload-files-to uri pattern)
  (define boundary (format #f "----------Artanis-~a" (get-random-from-dev #:uppercase #t)))
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
        (throw 'artanis-err 500 upload-files-to "This file `~a' doesn't exist!" file)))
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
                 0
                 0
                 #f))
      (else (throw 'artanis-err 500 build-data-mfd "Invalid pattern: ~a~%" p))))
  (define (build-file-mfd p)
    (match p
      ((name filename)
       (let ((mime (mime-guess (get-file-ext filename)))
             (data (->file filename)))
         (cons (make-mfd (->dispos name filename mime)
                         name
                         filename
                         0
                         (1- (bytevector-length data))
                         mime)
               data)))
      ((filename)
       ;; If name field is ingored, use "file" as its name in default.
       (build-file-mfd `("file" ,filename)))
      (else (throw 'artanis-err 500 build-file-mfd "Invalid pattern: ~a~%" p))))
  (let* ((mfdsp (->mfds pattern))
         (ct (format #f "multipart/form-data; boundary=~a" boundary))
         (body (mfds->body mfdsp boundary)))
    ;; NOTE:
    ;; Content-Length contains body length only
    ;; Guile web module will count Content-Length for you.
    (http-post uri
               #:body body
               #:headers `((Content-Type  ,ct)))))

(define (is-mfds? x)
  (and (list? x) (every mfd? x)))
