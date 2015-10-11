;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015
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

(define-module (artanis mvc model)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis fprm)
  #:use-module (artanis db)
  #:use-module ((rnrs) #:select (define-record-type))
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (do-model-create
            model-field-add!))

(define (opts-add new old)
  (apply lset-adjoin eq? new old))

(define (create-model-meta fields)
  (let ((ht (make-hash-table)))
    (for-each (lambda (f) (hash-set! ht (car f) (cdr f))) fields)
    ht))

(define-record-type model-field (fields name args))
(define-record-type date-field (parent model-field) (fields now))

(define (general-field-handler name . opts)
  (define (get-maxlen lst)
    (call-with-values
        (lambda () (plist-remove lst #:maxlen))
      (lambda (opts kv)
        (append (cdr kv) opts))))
  (case name
    ;; Auto index field
    ((auto) (list 'serial (opts-add '(#:no-edit #:not-null #:primary-key) opts)))
    ;; 64 long integer
    ((big-integer) (list 'integer 64 opts))
    ;; NOTE: No, we may not going to provide binary field, in Django, binary field could be
    ;;       used to store data which is upto 4GB into DB. It is a bad design IMO.
    ;; Raw binary data
    ;; ((binary) (list 'longblob opts)) ; longblob for mysql
    ((boolean) (list 'boolean opts))
    ((char-field) `(varchar ,@(get-maxlen opts)))
    ))

(define (date-field-handler now . opts)
  (let ((new-opts
         (list #:no-edit
               (case now
                 ((auto) #:auto-now)
                 ((auto-add) #:auto-now-add)
                 (else #:default-date-now)))))
    (list 'date (opts-add new-opts opts))))

(define *model-field-handlers*
  `((auto . ,general-field-handler)
    (bit-integer . ,general-field-handler)
    (boolean . ,general-field-handler)
    (char-field . ,general-field-handler)
    (date-field . ,date-field-handler)))

;; handler returns a list contains the valid field type in certain DBD
(define (model-field-add! name handler)
  (set! *model-field-handlers*
        (assoc-set! *model-field-handlers* name handler)))

(define (field:auto . args) (make-model-field 'auto args))
(define (field:bit-integer . args) (make-model-field 'big-integer args))
(define (field:boolean . args) (make-model-field 'boolean args))
(define (field:char-field . args) (make-model-field 'char-field args))
(define (field:date-field . args) (make-date-field 'date-field args))

(define (return-fixed-val info val)
  (case (car info)
    ((auto bit-integer boolean char-field) val)
    ((date-field)
     (cond
      ((assq-ref (cddr info) #:auto-now)
       (call-with-output-string
        (lambda (port)
          (write-date (get-local-time) port))))
      ((assq-ref (cddr info) #:auto-now-add)
       #t
       ;; TODO: finish it
       )))
    (else (throw 'artanis-err 500 "return-fixed-val: Invalid field type!" (car info)))))

(define (fix-fields cmd args meta)
  (define (fix-fields-to-set)
    (define (fix-val k v)
      (cond
       ((hash-ref meta k)
        => (lambda (opts)
             (cond
              ((assoc-ref opts #:no-edit)
               (throw 'artanis-err 500 "fix-val: Field can't be edited!" k))
              ((hash-ref meta k) => (lambda (info) (return-fixed-val info v)))
              (else (throw 'artanis-err 500 "fix-val: Field doesn't exist!" k)))))
       (else (throw 'artanis-err 500 "fix-val: No such field!" k))))
    (let lp((next args) (ret '()))
      (cond
       ((null? next) (reverse ret))
       ((keyword? (car next))
        (check-settable (car next))
        (let ((k (car next))
              (v (fix-val k (cadr next))))
          (lp (cddr next) (cons v (cons k ret)))))
       (else (lp (cdr next) (cons (car next) ret))))))
  (case cmd
    ((set) (fix-fields-to-set))
    ((get) args)
    (else (throw 'artanis-err 500 "fix-fields: Invalid cmd!" cmd))))

(define (parse-raw-fields lst)
  (define (->type type opts)
    (let ((h (assoc-ref *model-field-handlers* type)))
      (if h
          (apply h type opts)
          (throw 'artanis-err 500 "parse-raw-fields: Invalid field type!" type))))
  (let lp((next lst) (ret '()))
    (match next
      (() (reverse ret))
      ((((? symbol? name) (? symbol? type) (opts ...)) rest ...)
       (lp rest (cons `(,name ,@(->type type opts)) ret)))
      (else (throw 'artanis-err 500 "parse-raw-fields: Invalid field definition!" next)))))

;; For example:
;; (define-person
;;   (id auto (#:not-null #:primary-key))
;;   (first_name char-field (#:maxlen 30 #:not-null))
;;   (last_name char-field (#:maxlen 30 #:not-null)))
(define-syntax create-artanis-model
  (lambda (x)
    (syntax-case x ()
      ((_ name) (identifier? #'name)
       #`(begin
           ;; NOTE: we have to encapsulate them to a module for protecting namespaces
           ;; NOTE: we're not going to imort (artanis env) directly to avoid revealing global
           ;;       env vars to users.
           (define-module (app models name)
             #:use-module (artanis artanis)
             #:use-module (artanis utils)
             #:use-module (artanis db)
             #:use-module (artanis fprm))
           (define-syntax #,(datum->syntax #'name (symbol-append 'define- (syntax->datum #'name)))
             (syntax-rules ::: ()
               ((_ rest rest* :::)
                (define name
                  (let* ((raw (parse-raw-fields (list `rest `rest* :::)))
                         (mt (map-table-from-DB (get-conn-from-pool 0)))
                         (meta (create-model-meta (list `rest `rest* :::))))
                    (mt 'create 'name raw)
                    (lambda (cmd . args)
                      (apply mt cmd 'name (fix-fields cmd args meta)))))))))))))

(define (gen-model-header name)
  (call-with-output-string
   (lambda (port)
     (format port ";; Model ~a definition of ~a~%" name (current-appname))
     (display ";; Please add your license header here.\n" port)
     (display ";; This file is generated automatically by GNU Artanis.\n" port)
     (format port "(create-artanis-model ~a) ; DO NOT REMOVE THIS LINE!!!~%~%" name))))

(define (do-model-create name methods port)
  #t)
