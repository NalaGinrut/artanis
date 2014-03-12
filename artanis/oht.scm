;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014
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

;; -----------------------------------------------------------------------
;; This module implemented option-handler-table which is used to deal with
;; flexible rule options.

(define-module (artanis oht)
  #:use-module (artanis utils)
  #:use-module ((artanis artanis) #:select (get-handler-rc rc-rhk))
  #:use-module (artanis sql-mapping)
  #:use-module (artanis cookie)
  #:use-module (srfi srfi-1)
  #:export (new-oht))

(define-syntax-rule (get-oht rc)
  (get-handler-rc (rc-rhk rc)))

(define-syntax-rule (=> opt rc args ...)
  (let* ((oht (get-oht rc))
         (h (assoq-ref oht opt)))
    (h rc args ...)))

(define (str-maker val rule keys)
  (let ((tpl (apply make-string-template fmt)))
    (lambda (rc . args)
      (apply tpl (alist->kblist (rc-bt rc))))))

;; NOTE: these handlers should never be exported!
;; ((handler arg rule keys) rc . args)
(define *opions-meta-handler-table*
  `(;; a short way for sql-mapping from the bindings in rule
    ;; e.g #:sql-mapping "select * from table where id=${:id}"
    (#:sql-mapping . sql-mapping-maker)
    ;; generate a string from the bindings in rule
    ;; e.g (get "/get/:who" #:str "hello ${:who}" ...)
    (#:str . str-maker)
    ;; short-cut for authentication
    ;; e.g (get "/login" #:auth "select passwd from users where usr=${usr}"
    ;;      (lambda (rc)
    ;;       (:auth #:usr (params rc "usrname") #:passwd (params rc "passwd"))))
    ;; TODO: working on this
    (#:auth . auth-maker)
    ;; request a connection from connection-pool
    ;; NOTE: if you use #:sql-mapping short-cut, there's already a connect picked
    ;;       from pool, so #:sql-mapping implies #:conn is set to #t.
    ;; TODO: add recycling step after rule handler returning.
    ;; e.g (get "/show-all" #:conn #t
    ;;      (lambda (rc)
    ;;       (:conn "select * from articles")))
    (#:conn . conn-maker)
    ;; short-cut to set cookies
    ;; e.g (get "/" #:cookies (ca cb)
    ;;      (lambda (rc)
    ;;       (:cookies-set! ca "sid" "1231231")))
    (#:cookies . cookies-maker)))

(define-macro (meta-handler-register what)
  `(define-syntax-rule (,(symbol-append ': 'what) rc args ...)
     (=> ,(symbol->keyword what) rc args ...)))

;; register all the meta handler
(meta-handler-register sql-mapping)
(meta-handler-register str)
(meta-handler-register cookies)
(define-syntax-rule (:cookies-set! ck k v)
  ((:cookies 'set) ck k v))
(define-syntax-rule (:cookies-ref ck k)
  ((:cookies 'ref) ck k))

(for-each (lambda (x) (meta-handler-register (car x)))
          *opions-meta-handler-table*)

(define-syntax-rule (oh-ref o)
  (assoq *opions-meta-handler-table* o))

(define* (new-oht opts #:key (rule 'no-rules) (keys 'no-keys))
  (let ((oht (make-hash-table)))
    (apply for-each
           (lambda (k v)
             (let ((h (oh-ref k)))
               (cond
                ((or h (eq? k #:handler))
                 ;; #:handler is user customed handler
                 (hash-set! oht k (h v rule keys)))
                (else #f))))
           opts)
    oht))
