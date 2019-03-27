;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015,2018,2019
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
  #:export (create-artanis-model
            load-app-models
            do-model-create
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

(define (get-kw-val kw lst)
  (cond
   ((or (null? lst) (not (kw-arg-ref lst kw))) (list lst))
   (else
    (call-with-values
        (lambda () (plist-remove lst kw))
      (lambda (opts kv)
        (list (cadr kv) opts))))))

(define (general-field-handler name . opts)
  (define (get-maxlen lst) (get-kw-val #:maxlen lst))
  (define (get-diswidth lst) (get-kw-val #:diswidth lst))
  (define (get-integer-fractional-part lst)
    (match (get-kw-val #:integer-fractional lst)
      ;; We don't check the type here, deley to let FPRM check it.
      (((i f) . _) (list i f))
      (else '())))
  (case name
    ;; Auto index field
    ((auto) (list 'serial (opts-add '(#:no-edit #:not-null #:primary-key) opts)))
    ((tiny-integer) `(tinyint ,@(get-diswidth opts)))
    ((small-integer) `(smallint ,@(get-diswidth opts)))
    ;; 64 long integer
    ((big-integer) (list 'integer 64 opts))
    ;; NOTE: No, we may not going to provide binary field, in Django, binary field could be
    ;;       used to store data which is upto 4GB into DB. It is a bad design IMO.
    ;; Raw binary data
    ;; ((binary) (list 'longblob opts)) ; longblob for mysql
    ((boolean) (list 'boolean opts))
    ;; Integer part is the total number of digits.
    ;; Fractional part is the number of digits following the decimal point.
    ((float) `(float ,@(get-integer-fractional-part opts)))
    ((double) (pk "double"`(double ,@(get-integer-fractional-part opts))))
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
    (tiny-integer . ,general-field-handler)
    (small-integer . ,general-field-handler)
    (big-integer . ,general-field-handler)
    (boolean . ,general-field-handler)
    (float . ,general-field-handler)
    (double . ,general-field-handler)
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
          (throw 'artanis-err 500 fixed-date-field-val
                 "#:auto-now-once should be used with `create' command"))
      (gen-local-date-str))
     (else (throw 'artanis-err 500 fixed-date-field-val
                  "Shouldn't be here, why no default setting?")))))

(define-syntax-rule (check-field-value type r)
  (unless r
    (throw 'artanis-err 500 'check-field-value
           (format #f "~a check failed: ~a" type r))))

(define (auto-field-validator v)
  (throw 'artanis-err 500 auto-field-validator
         "AUTO field: you shouldn't set it manually!"))

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
  (string->irregex "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"))
(define (date-validator v)
  (check-field-value
   'date
   (irregex-search *date-re* v)))

(define (datetime-validator v)
  (check-field-value
   'datetime
   (irregex-search *date-re* v)))

(define (tinyint-validator v) v)
(define (smallint-validator v) v)
(define (float-validator v)
  ;; TODO: Is it possible to check integer-fractional by definition in migration?
  v)

;; FIXME: Shoud we check type validation in Scheme level? Or leave it as DB work.
(define *field-validators*
  `((auto . ,auto-field-validator)
    (tiny-integer . ,tinyint-validator)
    (small-integer . ,smallint-validator)
    (big-integer . ,big-integer-validator)
    (boolean . ,boolean-validator)
    (float . ,float-validator)
    (double . ,float-validator)
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
               (throw 'artanis-err 500 gen-default-val
                      "Field `~a' can't be edited!" k))
              ((hashq-ref meta k) => (lambda (info) (return-default-val cmd meta k)))
              (else (throw 'artanis-err 500 gen-default-val
                           "Field `~a' doesn't exist!" k)))))
       (else (throw 'artanis-err 500 gen-default-val
                    "No such field!" k))))
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
    (else args)))

(define (parse-raw-fields lst)
  (define (->type type opts)
    (let ((h (assoc-ref *model-field-handlers* type)))
      (if h
          (apply h type opts)
          (throw 'artanis-err 500 parse-raw-fields
                 "Invalid field type `~a'!" type))))
  (let lp((next lst) (main '()) (options '()))
    (match next
      (() `(,(reverse main) ,@(reverse options)))
      (((? keyword? k) v rest ...)
       (lp rest main `(,v ,k ,@options)))
      ((((? symbol? name) (? symbol? type) . (or (? null? opts) ((opts ...)))) rest ...)
       (lp rest (cons `(,name ,@(->type type opts)) main) options))
      (else (throw 'artanis-err 500 parse-raw-fields
                   "Invalid field definition!" next)))))

;; For example:
;; (create-artanis-model
;;   people
;;   (id auto (#:not-null #:primary-key))
;;   (first_name char-field (#:maxlen 30 #:not-null))
;;   (last_name char-field (#:maxlen 30 #:not-null)))
(define-syntax create-artanis-model
  (lambda (x)
    (syntax-case x ()
      ((_ name) (identifier? #'name)
       #'(error (format #f "You have created model `~a' without any definition!" 'name)))
      ((_ name rest rest* ...) (identifier? #'name)
       #`(begin
           ;; NOTE: we have to encapsulate them to a module for protecting namespaces
           ;; NOTE: we're not going to imort (artanis env) directly to avoid revealing global
           ;;       env vars to users.
           (define-module (app models name)
             #:use-module (artanis artanis)
             #:use-module (artanis utils)
             #:use-module (artanis db)
             #:use-module (artanis fprm))
           (define-public #,(datum->syntax #'name (symbol-append '$ (syntax->datum #'name)))
             (let ((raw (parse-raw-fields (list `rest `rest* ...)))
                   (mt (map-table-from-DB (current-connection))))
               (when (not (mt 'table-exists? 'name))
                 (format (artanis-current-output)
                         "Creating table `~a' defined in model......"
                         'name)
                 (apply mt 'try-create 'name raw)
                 (flush-to-migration-cache 'name (list `rest `rest* ...))
                 (try-to-load-migrate-cache 'name)
                 (format (artanis-current-output) "Done.~%"))
               (lambda (cmd . args)
                 (apply mt cmd 'name args))))
           (try-to-load-migrate-cache 'name))))))

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

(define-syntax-rule (scan-models) (scan-app-components 'models))

(define (load-app-models)
  (define toplevel (current-toplevel))
  (display "Loading models...\n" (artanis-current-output))
  (use-modules (artanis mvc model)) ; black magic to make Guile happy
  (let ((cs (scan-models)))
    (for-each (lambda (s)
                (load (format #f "~a/app/models/~a.scm" toplevel s)))
              cs)))

;; NOTE: id will be generated automatically, as primary-key.
;;       You may remove it to add your own primary-key.
;; NOTE: Each field will be set as #:not-null, modify it as you wish.
(define (do-model-create name fields port)
  (display (gen-model-header name) port)
  )
