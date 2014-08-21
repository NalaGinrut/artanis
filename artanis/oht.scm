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
  #:use-module (artanis sql-mapping)
  #:use-module (artanis db)
  #:use-module (artanis cookie)
  #:use-module (artanis cache)
  #:use-module (artanis route)
  #:use-module (artanis env)
  #:use-module (artanis third-party json)
  #:use-module (artanis third-party csv)
  #:use-module (ice-9 regex) ; FIXME: should use irregex!
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (web uri)
  #:use-module (web request)
  #:export (define-handler

            get
            post
            put
            patch
            page-delete

            :sql-mapping
            :str
            :conn
            :raw-sql
            :cookies
            :cache
            :cookies-set!
            :cookies-ref
            :cookies-update!))

(define (define-handler method rule opts-and-handler)
  (let ((keys (rule->keys rule))
        (path-regexp (compile-rule rule))
        (opts (oah->opts opts-and-handler))
        (handler (oah->handler opts-and-handler)))
    (hash-set! *handlers-table*
               (string-append method " " path-regexp)
               (make-handler-rc handler keys (new-oht opts #:rule rule #:keys keys)))))

(define (get rule . opts-and-handler) (define-handler "GET" rule opts-and-handler))
(define (post rule . opts-and-handler) (define-handler "POST" rule opts-and-handler))
(define (put rule . opts-and-handler) (define-handler "PUT" rule opts-and-handler))
(define (patch rule . opts-and-handler) (define-handler "PATCH" rule opts-and-handler))
;; NOTE: delete method is rarely used, and this name will override the list deletion in Scheme.
;;       So my vote is changing its name, feel free to let me know if it's improper.
(define (page-delete rule . opts-and-handler) (define-handler "DELETE" rule opts-and-handler))

;; NOTE: we banned "\" in the path to avoid SQL-injection!!!
(define *rule-regexp* (make-regexp ":[^\\/]+"))    
(define *path-keys-regexp* (make-regexp "/:([^\\/]+)"))

(define (compile-rule rule)
  (string-append "^" 
                 (regexp-substitute/global 
                  #f *rule-regexp* rule 'pre "([^\\/\\?]+)" 'post)
                 "[^ $]?$"))

;; parse rule-string and generate the regexp to parse keys from path-string
(define (rule->keys rule)
  (map (lambda (m) (match:substring m 1))
       (list-matches *path-keys-regexp* rule)))

(define-syntax-rule (get-oht rc)
  (handler-rc-oht (get-handler-rc (rc-rhk rc))))

;; returns #f if there's no such keyword was specified 
(define-syntax-rule (=> opt rc args ...)
  (let* ((oht (get-oht rc))
         (h (and oht (hash-ref oht opt))))
    (and h (h rc args ...))))

;; --------------------------------------------------------------
;; oht handlers

;; for #:str
(define (str-maker fmt rule keys)
  (let ((tpl (make-db-string-template fmt)))
    (lambda (rc . kargs)
      (and tpl (apply tpl `(,@(alist->kblist (rc-bt rc)) ,@kargs))))))

;; returns a queried conn, users have to get result by themselves.
;; for #:conn
(define (conn-maker yes? rule keys)
  (and yes?
       (lambda (rc sql)
         (let ((conn (DB-open rc)))
           (DB-query conn sql)
           conn))))

(define (raw-sql-maker sql rule keys)
  (lambda (rc mode)
    (let ((r (DB-query (DB-open rc) sql)))
      (match mode
        ('all (DB-get-all-rows r))
        ('top (DB-get-top-row r))
        ((? positive? n) (DB-get-n-rows r n))
        (else (throw 'artanis-err 500 "raw-sql: Invalid mode!" mode))))))

;; for #:cache
(define (cache-maker pattern rule keys)
  (define (non-cache rc body) body)
  (match pattern
    ((#f) non-cache)
    (((? string=? file) . maxage0)
     (lambda* (rc #:key (maxage (->maxage maxage0)))
       (try-to-cache-static-file rc file "public" maxage)))
    (('public (? string=? file) . maxage0)
     (lambda* (rc #:key (maxage (->maxage maxage0)))
       (try-to-cache-static-file rc file "public" maxage)))
    (('private (? string=? file) . maxage0)
     (lambda* (rc #:key (maxage (->maxage maxage0)))
       (try-to-cache-static-file rc file "private" maxage)))
    ((or (? boolean? opts) (? list? opts))
     (lambda (rc body) (try-to-cache-body rc body opts)))
    (else (throw 'artanis-err "cache-maker: invalid pattern!" pattern))))

;; for #:cookies
(define (cookies-maker val rule keys)
  (define *the-remove-expires* "Thu, 01-Jan-70 00:00:01 GMT")
  (define ckl '())
  (define (cget ck)
    (or (assoc-ref ckl ck)
        (throw 'artansi-err 500 "Undefined cookie name" ck)))
  (define (cset! ck k v)
    (and=> (cget ck) (cut cookie-set! <> k v)))
  (define (cref ck k)
    (and=> (cget ck) (cut cookie-ref <> k)))
  (define (update rc)
    (rc-set-cookie! rc (map cdr rc)))
  (define (remove rc k)
    (let ((cookies (rc-set-cookie rc)))
      (rc-set-cookie! 
       rc
       `(,@cookies ,(new-cookie #:npv '((key "")) #:expires *the-remove-expires*)))))
  (for-each 
   (lambda (ck) 
     (set! ckl (cons (cons ck (new-cookie)) ckl)))
   val)
  (lambda (op)
    (case op
      ((set) cset!)
      ((ref) cref)
      ((update) update)
      ((remove) remove)
      (else (throw 'artanis-err "cookies-maker: Invalid operation!" op)))))

;; for #:mime
(define (mime-maker type rule keys)
  (define mime (mime-guess (type)))
  (lambda args
    (define headers `((content-type . (,(mime-guess type)))))
    (define-syntax-rule (-> func) (values (apply func args) #:headers headers))
    (case type
      ((json) (-> scm->json-string))
      ((xml) (-> sxml->xml-string))
      ((csv) (-> scm->csv-string))
      (else (throw 'artanis-err 500 "mime-maker: Invalid type!" type)))))

;; ---------------------------------------------------------------------------------
  
;; NOTE: these short-cut-maker should never be exported!
;; ((handler arg rule keys) rc . args)
;; NOTE: If the keyword wasn't specified while defining the url-remap,
;;       the handler should return #f.
;; CONVENTION:
;; 1. When a var is arounded with ${..}, and there's ":" as its prefix,
;;    say, ${:xxx}, it means "to get xxx from rule key".
(define *options-meta-handler-table*
  `(;; a short way for sql-mapping from the bindings in rule
    ;; e.g #:sql-mapping "select * from table where id=${:id}"
    (#:sql-mapping . ,sql-mapping-maker)

    ;; generate a string from the bindings in rule
    ;; e.g (get "/get/:who" #:str "hello ${:who}" ...)
    (#:str . ,str-maker)

    ;; short-cut for authentication
    ;; #:auth accepts these values:
    ;; 1. SQL as string
    ;; 2. (table-name username-field passwd-field crypto-proc)
    ;; 3. (table-name crypto-proc), so passwd field will be "passwd" in default.
    ;; e.g (get "/auth" 
    ;;      #:auth "select ${passwd} from blog where usrname=${myname}"
    ;;      (lambda (rc)
    ;;       (:auth rc #:usrname (params rc "username") #:passwd (params rc "passwd"))
    ;;       ...))
    ;; The #:usrname and #:passwd will be expaned automatically.
    ;; or (get "/auth" #:auth `(blog "myuser" "mypasswd" ,string->md5)
    ;;     (lambda (rc)
    ;;      (if (:auth rc)
    ;;          (redirect-to rc "/dashboard")
    ;;          (redirect-to rc "/login?auth_failed=true"))))
    ;; or (get "/auth" #:auth `(blog ,string->md5)
    ;;     (lambda (rc)
    ;;      (if (:auth rc)
    ;;          (redirect-to rc "/dashboard")
    ;;          (redirect-to rc "/login?auth_failed=true"))))
    (#:auth . ,auth-maker)

    ;; Auto connection
    ;; request a connection from connection-pool, and close automatically.
    ;; NOTE: if you use #:sql-mapping short-cut, there's already a connect picked
    ;;       from pool, so #:sql-mapping implies #:conn is set to #t.
    ;; TODO: add recycling step after rule handler returning.
    ;; e.g (get "/show-all" #:conn #t
    ;;      (lambda (rc)
    ;;       (:conn rc "select * from articles")))
    (#:conn . ,conn-maker)

    ;; Raw simple sql query
    ;; NOTE: Raw sql is a const string of valid SQL.
    ;; E.g: (get "/sqltest"
    ;;       #:raw-sql "select * from Persons where name='nala'"
    ;;       (lambda (rc)
    ;;        (:raw-sql rc 'all)))
    ;; It's used for a quick query, but you can't modify/specify query on the fly. 
    ;; Usage:
    ;;  :raw-sql procedure accepts three modes:
    ;; 1. 'all for getting all results.
    ;; 2. certain number n for getting top n results.
    ;; 3. 'top for getting the top result.
    (#:raw-sql . ,raw-sql-maker)

    ;; Web caching
    ;; The default value is #f.
    ;; This is useful for web caching.
    ;; values:
    ;; * #t: will store the md5 of this page content, 
    ;;       and compare it with Etag.
    ;; * #f: won't store md5, and won't do any caching work.
    ;;       Etag would be ignored. If the current rule is for dynamic page,
    ;;       you have to set it to #f.
    ;; * filename: You may use (cache-file rc) to return the static file,
    ;;             it's the same function like emit-response-with-file.
    ;;             The difference is to cache it in memory.
    ;; * Caveats:
    ;; 1. #t will generate the dynamic page and get md5 for it anyway, the
    ;;    only advantage would be saving the bandwidth (return 304).
    ;; 2. Whether to use #:cache is depending on you.
    ;; NOTE: this option will be disabled automatically for some reasons:
    ;; 1. when #:auth was used in the same rule.
    ;; 2. when there's any HTTP header to stop the caching.
    (#:cache . ,cache-maker)

    ;; short-cut to set cookies
    ;; e.g (get "/" #:cookies (ca cb)
    ;;      (lambda (rc)
    ;;       (:cookies-set! ca "sid" "1231231")))
    (#:cookies . ,cookies-maker)

    ;; Convert to certain MIME type
    ;; There're three types: json/csv/xml
    ;; NOTE: Only used at the end of the handler (to replace response-emit)
    ;; e.g (get "/json" #:mime 'json
    ;;       (lambda (rc)
    ;;         (let ((j (json (object ("name" "nala") ("age" "15")))))
    ;;           (:mime j))))
    (#:mime . ,mime-maker)))

(define-macro (meta-handler-register what)
  `(define-syntax-rule (,(symbol-append ': what) rc args ...)
     (=> ,(symbol->keyword what) rc args ...)))

;; register all the meta handler
(meta-handler-register sql-mapping)
(meta-handler-register str)
(meta-handler-register conn)
(meta-handler-register raw-sql)
(meta-handler-register cookies)
(meta-handler-register cache)
(define-syntax-rule (:cookies-set! ck k v)
  ((:cookies 'set) ck k v))
(define-syntax-rule (:cookies-ref ck k)
  ((:cookies 'ref) ck k))
(define-syntax-rule (:cookies-update! rc)
  ((:cookies 'update) rc))

(define-syntax-rule (oh-ref o)
  (assq-ref *options-meta-handler-table* o))

;; return #f when there's no opt specified
(define* (new-oht opts #:key (rule 'no-rules) (keys 'no-keys))
  (if (null? opts)
      #f
      (let ((oht (make-hash-table)))
        (apply
         for-each
         (lambda (k v)
           (let ((h (oh-ref k)))
             (cond
              ((or h (eq? k #:handler))
               ;; #:handler is user customed handler
               (hash-set! oht k (h v rule keys)))
              (else #f))))
         opts)
        oht)))
