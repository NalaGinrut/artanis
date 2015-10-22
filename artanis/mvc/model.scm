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
  #:use-module (artanis irregex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (do-model-create
            model-field-add!
            field-validator-add!))

(define (opts-add new old)
  (apply lset-adjoin eq? new old))

(define opts-del plist-remove)

(define* (opts-ref opts o #:optional (value? #f))
  (let lp((next opts))
    (cond
     ((null? next) #f)
     ((eq? o (car next))
      (if value?
          (cadr next)
          o))
     (else (lp (cdr next))))))

(define (meta-update! o meta k)
  (hashq-set! meta k (opts-del (hashq-ref meta k) o)))

(define (create-model-meta fields)
  (let ((ht (make-hash-table)))
    (for-each (lambda (f) (hash-set! ht (car f) (cdr f))) fields)
    ht))

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
    ((char-field) `(varchar ,@(get-maxlen opts)))))

(define (date-field-handler now . opts)
  (let ((new-opts
         (cons #:no-edit
               (case now
                 ((auto) '(#:auto-now))
                 ((auto-once) '(#:auto-now-once))
                 (else '())))))
    (list 'date (opts-add new-opts opts))))

(define *model-field-handlers*
  `((auto . ,general-field-handler)
    (big-integer . ,general-field-handler)
    (boolean . ,general-field-handler)
    (char-field . ,general-field-handler)
    (date-field . ,date-field-handler)))

;; handler returns a list contains the valid field type in certain DBD
(define (model-field-add! name handler)
  (set! *model-field-handlers*
        (assoc-set! *model-field-handlers* name handler)))

;; NOTE: Different from auto_now* in Django, we allow users pass new value
;;       even auto_now* was specified.
;; NOTE: #:default will overwrite #:auto-now or #:auto-now-once. So don't use
;;       them together.
(define (fixed-date-field-val cmd meta k)
  (define (gen-local-date-str)
    (call-with-output-string
     (lambda (port)
       (write-date (get-local-time) port))))
  (let ((info (hashq-ref meta k)))
    (cond
     ((opts-ref (cddr info) #:auto-now #t)
      ;; Automatically set the field to now each time.
      (gen-local-date-str))
     ((opts-ref (cddr info) #:auto-now-once #t)
      ;; Set the field to now only once when the object is first created.
      (if (eq? cmd 'create)
          (meta-update! #:auto-now-once meta k)
          (throw 'artanis-err 500
                 "fixed-date-field-val: #:auto-now-once should be used with `create' cmd"))
      (gen-local-date-str))
     (else (throw 'artanis-err 500
                  "fixed-date-field-val: Shouldn't be here, why no default setting?")))))

(define-syntax-rule (check-field-value type r)
  (unless r
          (throw 'artanis-err 500
                 (format #f "~a check failed: ~a" type r))))

(define (auto-field-validator v)
  (throw 'artanis-err 500 "AUTO field: you shouldn't set it manually!"))

(define BIGINT_MAX 9223372036854775808)
(define (big-integer-validator v)
  (check-field-value
   'big-integer
   (and (integer? v) (>= v (- BIGINT_MAX)) (<= v BIGINT_MAX))))

;; NOTE: Boolean should be string since all the values should be upcased.
(define (boolean-validator v)
  (check-field-value
   'boolean
   (let ((vv (string-upcase v)))
     (or (string=? vv "FALSE") (string=? vv "TRUE")))))

(define *date-re*
  (string->sre "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"))
(define (date-validator v)
  (check-field-value
   'date
   (irregex-search *date-re* v)))

(define (datetime-validator v)
  (check-field-value
   'datetime
   (irregex-search *date-re* v)))

;; FIXME: Shoud we check type validation in Scheme level? Or leave it as DB work.
(define *field-validators*
  `((auto . ,auto-field-validator)
    (big-integer . ,big-integer-validator)
    (boolean . ,boolean-validator)
    (date . ,date-validator)
    (datetime . ,datetime-validator)))

(define (field-validator-add! type validator)
  (set! *field-validators*
        (assoc-set! *field-validators* type validator)))

(define (general-field-validator v)
  ;; TODO: maybe do some security check?
  v)

(define (validate-field-value v info)
  (cond
   ((or (kw-arg-ref info #:validator) (assoc-ref *field-validators* (car info)))
    => (lambda (h) (h v)))
   (else (general-field-validator v))))

;; Don't use it directly, since there's no existance-check in meta here. 
(define (return-default-val cmd meta k)
  (let ((info (hashq-ref meta k)))
    (cond
     ((kw-arg-ref (cadr info) #:default)
      => (lambda (thunk) (list (thunk) k)))
     (else
      (case (car info)
        ((date-field date-time-field)
         (list (fixed-date-field-val cmd meta k) k))
        (else '())))))) ; no default value

(define (fix-fields cmd args meta)
  (define (fix-fields-to-set)
    (define (gen-default-val k)
      (cond
       ((hash-ref meta k)
        => (lambda (opts)
             (cond
              ((assoc-ref opts #:no-edit)
               (throw 'artanis-err 500 "fix-val: Field can't be edited!" k))
              ((hashq-ref meta k) => (lambda (info) (return-default-val cmd meta k)))
              (else (throw 'artanis-err 500 "fix-val: Field doesn't exist!" k)))))
       (else (throw 'artanis-err 500 "fix-val: No such field!" k))))
    (let lp((kl (hash-map->list cons meta)) (al args) (ret '()))
      (cond
       ((null? kl) `(,(reverse ret) ,@al)) ; append condition string if possible
       ((kw-arg-ref args (caar kl))
        => (lambda (v)
             (lp (cdr kl)
                 (cddr args) ; a trick to get condition string
                 (list (validate-field-value v (cdar kl)) (caar kl) ret))))
       (else (lp (cdr kl) al `(,@(return-default-val cmd meta (caar kl)) ,@ret))))))
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
           (try-to-load-migrate-cache name)
           (define-syntax #,(datum->syntax #'name (symbol-append 'define- (syntax->datum #'name)))
             (syntax-rules ::: ()
               ((_ rest rest* :::)
                (define-public name
                  (let* ((raw (parse-raw-fields (list `rest `rest* :::)))
                         (mt (map-table-from-DB (get-conn-from-pool 0)))
                         (meta (create-model-meta (list `rest `rest* :::))))
                    (lambda (cmd . args)
                      (apply mt cmd 'name (fix-fields cmd args meta)))))))))))))

(define (gen-model-header name)
  (call-with-output-string
   (lambda (port)
     (format port ";; Model ~a definition of ~a~%" name (current-appname))
     (display ";; Please add your license header here.\n" port)
     (display ";; This file is generated automatically by GNU Artanis.\n" port)
     (format port "(create-artanis-model ~a) ; DO NOT REMOVE THIS LINE!!!~%~%" name))))

;; NOTE: Whole list of types:
;;       integer
;;       primary_key
;;       decimal
;;       float
;;       boolean
;;       binary
;;       string
;;       text
;;       date
;;       time
;;       datetime
;;       timestamp
(define (parse-field-str str)
  `(,@(map string-trim-both (string-split str #\:)) (#:not-null)))

;; NOTE: id will be generated automatically, as primary-key.
;;       You may remove it to add your own primary-key.
;; NOTE: Each field will be set as #:not-null, modify it as you wish.
(define (do-model-create name fields port)
  (display (gen-model-header name) port)
  )
