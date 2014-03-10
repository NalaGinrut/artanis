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
  #:use-module (srfi srfi-1)
  #:export (new-oht))

(define-syntax-rule (get-oht rc)
  (get-handler-rc (rc-rhk rc)))

(define-syntax-rule (=> opt rc args ...)
  (let* ((oht (get-oht rc))
         (h (assoq-ref oht opt)))
    (h rc args ...)))

(define (path-maker rule keys)
  #t)

(define (str-maker rule keys)
  #t)

(define (bind-maker rule keys)
  #t)

(define (cookies-maker rule keys)
  #t)

;; NOTE: these handler should never be exported!
;; (handler arg rule keys)
(define *opions-meta-handler-table*
  `(;; a short way for sql-mapping from the bindings in rule
    (#:sql-mapping . sql-mapping-maker)
    ;; generate a string from the bindings in rule
    (#:str . str-maker)
    ;; bind the key-bindings to specified identifier  
    (#:bind . bind-maker)
    ;; short-cut to set cookies
    (#:cookies . cookies-maker)))

(define-syntax-rule (:sql-mapping rc args ...)
  (=> #:sql-mapping rc args ...))

(define-syntax-rule (:str rc args ...)
  (=> #:str rc args ...))

(define-syntax-rule (:bind rc args ...)
  (=> #:bind rc args ...))

(define-syntax-rule (:cookies rc args ...)
  (=> #:cookies rc args ...))

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
