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

(define-module (artanis utils)
  #:use-module (artanis crypto md5)
  #:use-module (artanis crypto sha-1)
  #:use-module (artanis config)
  #:use-module (artanis irregex)
  #:use-module (artanis mime)
  #:use-module (system foreign)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 local-eval)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 q)
  #:use-module (web http)
  #:use-module (web request)
  #:export (regexp-split hash-keys cat bv-cat get-global-time
            get-local-time string->md5 unsafe-random string-substitute
            get-file-ext get-global-date get-local-date uri-decode
            nfx static-filename remote-info seconds-now local-time-stamp
            parse-date write-date make-expires export-all-from-module!
            alist->hashtable expires->time-utc local-eval-string
            time-expired? valid-method? mmap munmap get-random-from-dev
            string->byteslist string->sha-1 list-slice bv-slice uni-basename
            checkout-the-path make-string-template guess-mime prepare-headers
            new-stack new-queue stack-slots queue-slots stack-pop! stack-push!
            stack-top stack-empty? queue-out! queue-in! queue-head queue-tail
            queue-empty? list->stack list->queue stack-remove! queue-remove!
            plist->alist make-db-string-template non-list?
            keyword->string range oah->handler oah->opts string->keyword
            alist->klist alist->kblist is-hash-table-empty?)
  #:re-export (the-environment))

;; There's a famous rumor that 'urandom' is safer, so we pick it.
(define* (get-random-from-dev #:key (length 8) (uppercase #f))
  (call-with-input-file "/dev/urandom" 
    (lambda (port)  
      (let* ((bv ((@ (rnrs) get-bytevector-n) port length))
             (str (format #f "~{~2,'0x~}" ((@ (rnrs) bytevector->u8-list) bv))))
        (if uppercase
            (string-upcase str)
            str)))))
           
(define uri-decode (@ (web uri) uri-decode))
(define parse-date (@@ (web http) parse-date))
(define write-date (@@ (web http) write-date))
(define bytevector? (@ (rnrs) bytevector?))

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

(define (time-expired? expires)
  (if expires
      (let ((t (expires->time-utc expires)))
        (time>? (current-time) t))
      #t)) ;; no expired, means session cookie, which is always expired

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
  (if (get-conf '(server nginx))
      (assoc-ref (request-headers req) 'x-real-ip)
      (request-host req)))

(define *methods-list* '(HEAD GET POST PUT PATCH DELETE))
(define (allowed-method? method)
  ;; TODO: check allowed method from config
  #t)
(define (valid-method? method)
  (if (and (member method *methods-list*) (allowed-method? method))
      method
      (throw 'artanis-err 405 "invalid HTTP method" method)))

;; -------------- mmap ---------------------
(define-public ACCESS_COPY              #x3)
(define-public ACCESS_READ              #x1)
(define-public ACCESS_WRITE             #x2)
(define-public ALLOCATIONGRANULARITY #x1000)

(define-public PROT_READ       #x1)       
(define-public PROT_WRITE      #x2)       
(define-public PROT_EXEC       #x4)       
(define-public PROT_SEM        #x8)       
(define-public PROT_NONE       #x0)       
(define-public PROT_GROWSDOWN  #x01000000)
(define-public PROT_GROWSUP    #x02000000)

(define-public PAGESIZE       #x1000)
(define-public MAP_ANON         #x20)
(define-public MAP_DENYWRITE   #x800)
(define-public MAP_EXECUTABLE #x1000)
(define-public MAP_SHARED       #x01)
(define-public MAP_PRIVATE      #x02)
(define-public MAP_TYPE         #x0f)
(define-public MAP_FIXED        #x10)
(define-public MAP_ANONYMOUS    #x20)
(define-public MAP_UNINITIALIZED 0) ;; don't support map uninitialized

(define *libc-ffi* (dynamic-link))
(define %mmap
  (pointer->procedure '*
                      (dynamic-func "mmap" *libc-ffi*)
                      (list '* size_t int int int size_t)))
(define %munmap
  (pointer->procedure int
                      (dynamic-func "munmap" *libc-ffi*)
                      (list '* size_t)))
(define* (mmap size #:key (addr %null-pointer) (fd -1) (prot MAP_SHARED) 
               (flags PROT_READ) (offset 0))
  (pointer->bytevector (%mmap addr size prot flags fd offset) size))
(define (munmap bv size)
  (%munmap (bytevector->pointer bv size) size))

;; FIXME: what if len is not even?
(define (string->byteslist str step base)
  (define len (string-length str))
  (let lp((ret '()) (i 0)) 
    (cond 
     ((>= i len) (reverse ret))
     ((zero? (modulo i step)) 
      (lp (cons (string->number (substring/shared str i (+ i step)) base) ret) (1+ i))) 
     (else (lp ret (1+ i))))))

(define (string->sha-1 str/bv)
  (let ((in (cond
             ((string? str/bv)
              ((@ (rnrs) string->utf8) str/bv))
             (((@ (rnrs) bytevector?) str/bv)
              str/bv)
             (else (error "need string or bytevector!" str/bv)))))
    (sha-1->string (sha-1 in))))

(define-syntax list-slice
  (syntax-rules (:)
    ((_ ll lo : hi)
     (let ((len (length ll)))
       (and (<= lo len) (>= len hi)
	    (let lp((rest ll) (result '()) (cnt 1))
	      (cond
	       ((null? rest) (error "no"))
	       ((<= cnt lo) (lp (cdr rest) result (1+ cnt)))
	       ((> cnt hi) (reverse result))
	       (else (lp (cdr rest) (cons (car rest) result) (1+ cnt))))))))
    ((_ ll lo :)
     (drop ll lo))
    ((_ ll : hi)
     (take ll hi))))

;; TODO: 
;; 1. (> hi (bytevector-length bv))
;; 2. (< lo 0) wrap reference
(define (%bv-slice bv lo hi) 
  (let* ((len (- hi lo)) 
         (slice ((@ (rnrs) make-bytevector) len)))
    ((@ (rnrs) bytevector-copy!) bv lo slice 0 len) slice))

;; NOT SAFE %bytevector-slice for GC, need 
;;(define (%bytevector-slice bv lo hi)
;;  (and (< hi lo) (error %bytevector-slice "wrong range" lo hi))
;;  (let* ((ptr (bytevector->pointer bv))
;;         (addr (pointer-address ptr))
;;        (la (+ addr lo))
;;         (len (- hi lo)))
;;    (pointer->bytevector (make-pointer la) len)))
  
(define-syntax bv-slice
  (syntax-rules (:)
    ((_ bv lo : hi)
     (%bv-slice bv lo hi))
    ((_ bv lo :)
     (%bv-slice bv lo ((@ (rnrs bytevectors) bytevector-length) bv)))
    ((_ bv : hi)
     (%bv-slice bv 0 hi))))

;; get the unified basename both POSIX and WINDOWS
(define (uni-basename filename)
  (substring filename
   (1+ 
    (string-index-right filename 
                        (lambda (c) (or (char=? c #\\) (char=? c #\/)))))))

;; FIXME: checkout-the-path only support POSIX file path
;; FIXME: what's the proper default mode for the dir?
(define* (checkout-the-path path #:optional (mode #o775))
  (let ((paths (string-split path #\/)))
    (let lp((next paths) (last ""))
      (cond
       ((null? next) #t)
       ((string-null? (car next)) (lp (cdr next) last))
       (else 
        (let ((now-path (string-append last (car next) "/")))
          (cond
           ((file-exists? now-path)
            (lp (cdr next) now-path))
           (else
            (mkdir now-path mode)
            (lp (cdr next) now-path)))))))))

;; NOTE: This my original verion of make-string-template

;; (define *stpl-SRE* '(or (=> tilde "~")
;;                         (=> dollar "$$")
;;                         (: "${" (=> name (+ (~ #\}))) "}")))

;; (define* (make-string-template str #:optional (mode #f) . opts)
;;   (define ll '()) ; list for all keywords
;;   (define lv '()) ; list for default value
;;   (define template
;;     (irregex-replace/all 
;;      ;;"(\\$\\{([^$])+\\})"
;;      *stpl-SRE* str
;;      (lambda (m) 
;;        (cond
;;         ((irregex-match-substring m 'dollar) "$")
;;         ((irregex-match-substring m 'tilde) "~~")
;;         (else
;;          (let* ((var (irregex-match-substring m 1))
;;                 (key (symbol->keyword (string->symbol 
;;                                        (irregex-match-substring m 'name))))
;;                 (v (kw-arg-ref opts key)))
;;            (and v (set! lv (cons (cons key v) lv))) ; default value
;;            (set! ll (cons key ll))
;;            (set! lk (cons var lk))
;;            "~a"))))))
;;   (lambda args
;;     (let ((vals (map (lambda (x) 
;;                        (or (kw-arg-ref args x) (assoc-ref lv x)
;;                            (if mode (assoc-ref lk x) "NONE"))) ll)))
;;     (format #f "~?" template (reverse vals)))))

;; NOTE: This is mark_weaver version for efficiency, Thanks mark!
(define (%make-string-template mode template . defaults)
  (define irx (sre->irregex '(or (=> dollar "$$")
                                 (: "${" (=> var (+ (~ #\}))) "}"))))
  (define (get-the-val lst key)
    (let ((str (kw-arg-ref lst key)))
      (case mode
        ((normal) str)
        ((db) (string-concatenate (list "\"" (->string str) "\"")))
        (else (throw 'artanis-err 500 "%make-string-template: invalid mode" mode)))))
  (define (->string obj) (if (string? obj) obj (object->string obj)))
  (define (optimize rev-items tail)
    (cond ((null? rev-items) tail)
          ((not (string? (car rev-items)))
           (optimize (cdr rev-items)
                     (cons (car rev-items) tail)))
          (else (receive (strings rest) (span string? rev-items)
                  (let ((s (string-concatenate-reverse strings)))
                    (if (string-null? s)
                        (optimize rest tail)
                        (optimize rest (cons s tail))))))))
  (define (match->item m)
    (or (and (irregex-match-substring m 'dollar) "$")
        (let* ((name (irregex-match-substring m 'var))
               (key (symbol->keyword (string->symbol name))))
          (cons key (kw-arg-ref defaults key)))))
  (let* ((rev-items (irregex-fold
                     irx
                     (lambda (idx m tail)
                       (cons* (match->item m)
                              (substring template
                                         idx
                                         (irregex-match-start-index m 0))
                              tail))
                     '()
                     template
                     (lambda (idx tail)
                       (cons (substring template idx) tail))))
         (items (optimize rev-items '())))
    (lambda keyword-args
      (define (item->string item)
        (if (string? item)
            item
            (or (and=> (get-the-val keyword-args (car item)) ->string)
                (cdr item)
                (error "Missing keyword" (car item)))))
      (string-concatenate (map item->string items)))))

;; the normal mode, no double quotes for vals
(define (make-string-template tpl . vals)
  (apply %make-string-template 'normal tpl vals))

;; DB str tpl will treat all values with double quotes, for SQL
(define (make-db-string-template tpl . vals)
  (apply %make-string-template 'db tpl vals))

(define (guess-mime filename)
  (mime-guess (get-file-ext filename)))

(define (bytevector-null? bv)
  ((@ (rnrs bytevectors) bytevector=?) bv #u8()))

(define (prepare-headers body headers)
  (define *default-header* '((content-type . (text/html))))
  ;; FIXME: the latest Guile fixed content-length:0 bug, but 2.0.9 is not,
  ;;        so remove it when next release.
  (when (not body)
    (error prepare-headers "Fatal: Something got wrong, the body shouldn't be #f!" body))
  (let* ((check (cond ((bytevector? body) bytevector-null?)
                      ((string? body) string-null?)))
         (len (if (check body) '((content-length . 0)) '())))
    `(,@*default-header* ,@headers ,@len)))

(define new-stack make-q)
(define new-queue make-q)
(define stack-slots car)
(define queue-slots car)

(define (%q-remove-with-key! q key)
  (assoc-remove! (car q) key)
  (sync-q! q))

(define stack-pop! q-pop!)
(define stack-push! q-push!)
(define stack-top q-front)
(define stack-remove! %q-remove-with-key!)
(define stack-empty? q-empty?) 

(define queue-out! q-pop!)
(define queue-in! enq!)
(define queue-head q-front)
(define queue-tail q-rear)
(define queue-remove! %q-remove-with-key!)
(define queue-empty? q-empty?)

(define* (list->stack lst #:optional (stk (new-stack))) ; NOTE: make-stack exists in Guile
  (for-each (lambda (x) (stack-push! stk x)) lst)
  stk)

(define* (list->queue lst #:optional (queue (new-queue)))
  (for-each (lambda (x) (queue-in! queue x)) lst)
  queue)

;; NOTE: keyword could be the value, so this version is correct.
(define (plist->alist lst)
  (let lp((next lst) (ret '()))
    (match next
      (() (reverse ret))
      ((k v . rest) (lp (cddr next) (acons (keyword->symbol k) v ret))))))

(define-syntax-rule (non-list? x) (not (list? x)))
(define-syntax-rule (keyword->string x) (symbol->string (keyword->symbol x)))

(define* (range from to #:optional (step 1))
  (iota (- to from) from step))

;; NOTE: handler must be the last element of the list, it's should be error
;;       if it's not so.
(define (oah->handler opts-and-handler)
  (let ((handler (and (list? opts-and-handler) (last opts-and-handler))))
    (if (procedure? handler)
        handler
        (error oah->handler "You have to specify a handler for this rule!"))))

;; get all kw-args from the middle of args
(define (oah->opts opts-and-handler)
  (if (procedure? opts-and-handler)
      '() ; there's no opts
      (let lp((next opts-and-handler) (kl '()) (vl '()))
        (match next
          (((? keyword? k) v rest ...)
           (lp rest (cons k kl) (cons v vl)))
          ((or (? null?) (? procedure?))
           ;; no opts left, return the result
           (list kl vl))
          (else (lp (cdr next) kl vl))))))

(define (string->keyword str)
  (symbol->keyword (string->symbol str)))

(define (alist->klist al)
  (let lp((next al) (ret '()))
    (cond
     ((null? next) ret)
     (else
      (let ((k (symbol->keyword (car (car next))))
            (v (cdr (car next))))
        (lp (cdr next) (cons k (cons v ret))))))))

(define (alist->kblist al)
  (let lp((next al) (ret '()))
    (cond
     ((null? next) ret)
     (else
      (let ((k (string->keyword (string-append ":" (car (car next)))))
            (v (cdr (car next))))
        (lp (cdr next) (cons k (cons v ret))))))))

(define (is-hash-table-empty? ht)
  (zero? (hash-count values ht)))
