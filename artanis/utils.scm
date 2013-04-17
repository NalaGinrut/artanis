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

(define-module (artanis utils)
  #:use-module (artanis md5)
  #:use-module (artanis config)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 local-eval)
  #:use-module (web http)
  #:use-module (web request)
  #:export (regexp-split hash-keys cat bv-cat get-global-time
            get-local-time string->md5 unsafe-random string-substitute
            get-file-ext get-global-date get-local-date uri-decode
            nfx static-filename remote-info seconds-now local-time-stamp
            parse-date write-date make-expires export-all-from-module!
            alist->hashtable expires->time-utc local-eval-string generate-ETag)
  #:re-export (the-environment))

(define uri-decode (@ (web uri) uri-decode))
(define parse-date (@@ (web http) parse-date))
(define write-date (@@ (web http) write-date))

(define-syntax-rule (local-eval-string str e)
  (local-eval 
   (call-with-input-string (format #f "(begin ~a)" str) read)
   e))

(define (alist->hashtable al)
  (let ((ht (make-hash-table)))
    (for-each (lambda (x)
                (hash-set! ht (car x) (cadr x)))
              al)
    ht))

(eval-when (eval load compile)
 (define (export-all-from-module! module-name)
   (let ((mod (resolve-module module-name)))
         (module-for-each (lambda (s m) 
                            (module-add! (current-module) s m)) mod))))
 
(define (expires->time-utc str)
  (date->time-utc (parse-date str)))

(define (make-expires sec)
  (get-local-time (+ (seconds-now) sec)))

(define (seconds-now)
  ((@ (guile) current-time)))

;; This function only used for local logger
(define (local-time-stamp)
  (strftime "%F %T" (localtime (seconds-now))))

;; default time is #f, get current time
(define* (get-global-time #:optional (time #f) (nsec 0))
  (call-with-output-string 
   (lambda (port)
     ;; NOTE: (time-utc->data t 0) to get global time.
     (write-date 
      (time-utc->date 
       (if time (make-time 'time-utc nsec time) (current-time))
       0)
      port))))

;; default time is #f, get current time
(define* (get-local-time #:optional (time #f) (nsec 0))
  (call-with-output-string 
   (lambda (port)
     ;; NOTE: (time-utc->data t) to get local time.
     (write-date 
      (time-utc->date 
       (if time (make-time 'time-utc nsec time) (current-time)))
      port))))

(define* (regexp-split regex str #:optional (flags 0))
  (let ((ret (fold-matches 
              regex str (list '() 0 str)
              (lambda (m prev)
                (let* ((ll (car prev))
                       (start (cadr prev))
                       (tail (match:suffix m))
                       (end (match:start m))
                       (s (substring/shared str start end))
                       (groups (map (lambda (n) (match:substring m n))
                                    (iota (1- (match:count m)) 1))))
                  (list `(,@ll ,s ,@groups) (match:end m) tail)))
              flags)))
    `(,@(car ret) ,(caddr ret))))

(define (hash-keys ht)
  (hash-map->list (lambda (k v) k) ht))

;; WARN: besure that you've already checked the file exists before!!!
(define* (cat file/port #:optional (out (current-output-port)))
  (define get-string-all (@ (rnrs io ports) get-string-all))
  (if (port? file/port)
      (get-string-all file/port)
      (let ((str (if (port? file/port)
                     (get-string-all file/port)
                     (call-with-input-file file/port get-string-all))))
        (if out
            (display str out)
            str))))

;; WARN: besure that you've already checked the file exists before!!!
(define* (bv-cat file/port #:optional (out (current-output-port)))
  (define get-bytevector-all (@ (rnrs io ports) get-bytevector-all))
  (let ((bv (if (port? file/port)
                (get-bytevector-all file/port)
                (call-with-input-file file/port get-bytevector-all))))
    (if out
        (display bv out)
        bv)))

(define (string->md5 str)
  (call-with-input-string str md5))

;; 35147 is the length of GPLv3 in bytes
(define* (unsafe-random #:optional (n 35147))
  (random n (random-state-from-platform)))

(define (string-substitute str re what)
  (regexp-substitute/global #f re str 'pre what 'post))

(define-syntax get-file-ext               
  (syntax-rules ()
    ((_ filename)
     (substring/shared filename
                       (1+ (string-index-right filename #\.))))))

(define* (get-global-date #:optional (time #f))
  (parse-header 'date 
                (if time
                    (get-global-time (car time) (cdr time)) 
                    (get-global-time))))

(define* (get-local-date #:optional (time #f))
  (parse-header 'date 
                (if time
                    (get-local-time (car time) (cdr time)) 
                    (get-local-time))))

(define (nfx exp)   
  (let lp((rest exp) (result '()) (cur #f))
    (cond 
     ((null? rest) result)
     ((null? result)
      (let ((e (list (cadr rest) (car rest) (caddr rest)))) 
        (lp (cdddr rest) e (car rest))))
     (else
      (let ((e (list cur result (cadr rest)))) 
        (lp (cddr rest) e #f))))))

(define-syntax-rule (static-filename path)
  (substring/shared path 1))

(define-syntax-rule (remote-info req)
  (if use-Nginx?
      (assoc-ref (request-headers req) 'x-real-ip)
      (request-host req)))

(define (generate-Etag filename)
  (and (file-exists? filename)
       (let ((st (stat filename)))
         (format #f "~a-~a-~a" 
                 (stat:ino st) (stat:mtime st) (stat:size st)))))
