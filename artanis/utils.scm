;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015
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

(define-module (artanis utils)
  #:use-module (artanis crypto md5)
  #:use-module (artanis crypto sha-1)
  #:use-module (artanis crypto base64)
  #:use-module (artanis tpl sxml)
  #:use-module (artanis config)
  #:use-module (artanis irregex)
  #:use-module (artanis env)
  #:use-module (artanis mime)
  #:use-module (system foreign)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
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
            alist->klist alist->kblist is-hash-table-empty?
            symbol-downcase symbol-upcase normalize-column
            sxml->xml-string run-after-request! run-before-response!
            make-pipeline HTML-entities-replace eliminate-evil-HTML-entities
            generate-kv-from-post-qstr handle-proper-owner
            generate-data-url find-ENTRY-path verify-ENTRY current-appname
            draw-expander remove-ext scan-app-components cache-this-route!
            dump-route-from-cache generate-modify-time)
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
(define utf8->string (@ (rnrs) utf8->string))

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
  (let ((str (if (port? file/port)
                 (get-string-all file/port)
                 (call-with-input-file file/port get-string-all))))
    (if out
        (display str out)
        str)))

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
  (define (->string obj)
    (if (string? obj) obj (object->string obj)))
  (define (get-the-val lst key)
    (let ((str (kw-arg-ref lst key)))
      (case mode
        ((normal) str)
        ((db) (string-concatenate (list "\"" (->string str) "\"")))
        (else (throw 'artanis-err 500 "%make-string-template: invalid mode" mode)))))
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
                (cdr item) ; default value
                (throw 'artanis-err 500
                       "(utils)item->string: Missing keyword" (car item)))))
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

(define (generate-modify-time t)
  (get-local-date (cons (time-second t) (time-nanosecond t))))

(define (prepare-headers headers)
  (define *default-headers*
    `((content-type . (text/html (charset . ,(get-conf '(server charset)))))
      (date . ,(get-global-date))))
  (lset-union (lambda (x y) (eq? (car x) (car y)))
              (assq-remove! headers 'last-modified) *default-headers*))

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
(define* (keyword->string x #:optional (proc identity))
  (proc (symbol->string (keyword->symbol x))))

(define* (range from to #:optional (step 1))
  (iota (- to from) from step))

;; NOTE: handler must be the last element of the list, it's should be error
;;       if it's not so.
(define (oah->handler opts-and-handler)
  (let ((handler (and (list? opts-and-handler) (last opts-and-handler))))
    (if (or (procedure? handler) (string? handler))
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

(define (symbol-strop proc sym)
  (string->symbol (proc (symbol->string sym))))

(define (symbol-downcase sym)
  (symbol-strop string-downcase sym))

(define (symbol-upcase sym)
  (symbol-strop string-upcase sym))

(define* (normalize-column col #:optional (ci? #f))
  (define-syntax-rule (-> c p) (if ci? (p col) col))
  (cond
   ((string? col) (string->symbol (-> c string-downcase)))
   ((symbol? col) (-> col symbol-downcase))
   ((keyword? col) (normalize-column (keyword->string col) ci?))
   (else (throw 'artanis-err 500 "normalize-column: Invalid type of column" col))))

(define* (sxml->xml-string sxml #:key (escape? #f))
  (call-with-output-string
   (lambda (port)
     (sxml->xml sxml port escape?))))

(define (run-after-request! proc)
  (add-hook! *after-request-hook* proc))

(define (run-before-response! proc)
  (add-hook! *before-response-hook* proc))

;; NOTE: For `pipeline' methodology, please read my post:
;; http://nalaginrut.com/archives/2014/04/25/oba-pipeline-style%21
(define (make-pipeline . procs)
  (lambda (x) (fold (lambda (y p) (y p)) x procs)))

(define (HTML-entities-replace set content)
  (define in (open-input-string content))
  (define (hit? c/str) (assoc-ref set c/str))
  (define (get-estr port)
    (let lp((n 0) (ret '()))
      (cond
       ((= n 3) (list->string (reverse! ret)))
       (else (lp (1+ n) (cons (read-char port) ret))))))
  (call-with-output-string
   (lambda (out)
     (let lp((c (peek-char in)))
       (cond
        ((eof-object? c) #t)
        ((hit? c)
         => (lambda (str)
              (display str out)
              (read-char in)
              (lp (peek-char in))))
        ((char=? c #\%)
         (let* ((s (get-estr in))
                (e (hit? s)))
           (if e
               (display e out)
               (display s out))
           (lp (peek-char in))))
        (else
         (display (read-char in) out)
         (lp (peek-char in))))))))

(define *terrible-HTML-entities*
  '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;")
    ("%3C" . "&lt;") ("%3E" . "&gt;") ("%26" . "&amp;") ("%22" . "&quot;")))
;; NOTE: cooked for anti-XSS.
(define (eliminate-evil-HTML-entities content)
  (HTML-entities-replace *terrible-HTML-entities* content))

(define* (generate-kv-from-post-qstr body #:key (no-evil? #f)
                                     (key-converter identity))
  (define cook (if no-evil? eliminate-evil-HTML-entities identity))
  (define (%convert lst)
    (match lst
      ((k v) (list (key-converter k) v))
      (else (throw 'artanis-err 500 "generate-kv-from-post-qstr: Fatal! Can't be here!" lst))))
  (define (-> x)
    (string-trim-both x (lambda (c) (member c '(#\sp #\: #\return)))))
  (map (lambda (x)
         (%convert (map -> (string-split (cook x) #\=))))
       (string-split (utf8->string body) #\&)))

;; NOTE: We accept warnings, which means if warnings occurred, it'll be 200(OK) anyway, but
;;       Artanis will throw warnings in the server-side log.
;; NOTE: We *DO NOT* accept errors, which means if errors occurred, Artanis will throw 500.
(define (handle-proper-owner file uid gid)
  (define-syntax-rule (print-the-warning exe reason)
    (format (current-error-port) "[WARNING] '~a' encountered system error: ~s~%" exe reason))
  (define-syntax-rule (->err-reason exe reason)
    (format #f "'~a' encoutered system error: ~s" exe reason))
  (catch 'system-error
   (lambda ()
     (chown file (or uid (getuid)) (or gid (getgid))))
   (lambda (k . e)
     (let ((exe (car e))
           (reason (caaddr e)))
       (match (cons k reason)
         ('(system-error . "Operation not permitted")
          (print-the-warning exe reason)
          (display
           "Maybe you run Artanis as unprivileged user? (say, not as root)\n"
           (current-error-port)))
         ('(system-error . "No such file or directory")
          (throw 'artanis-err 500 (->err-reason exe reason) file))
         (else (apply throw k e)))))))

;; According to wiki, here's the standard format of data_url_scheme:
;; data:[<MIME-type>][;charset=<encoding>][;base64],<data>
(define* (generate-data-url bv/str #:key (mime 'application/octet-stream)
                            (crypto 'base64) (charset 'utf-8))
  (define-syntax-rule (->crypto)
    (match crypto
      ((or 'base64 "base64") ";base64")
      (else "")))
  (define-syntax-rule (->charset)
    (if (or (string? charset) (symbol? charset))
        (format #f ",charset=~a" charset)
        ""))
  (define-syntax-rule (->mime)
    (match mime
      (`(guess ,ext) (or (mime-guess ext) 'application/octet-stream))
      ((or (? symbol?) (? string?))
       (or (and (get-conf 'debug-mode) (mime-check mime) (format #f "~a" mime))
           (format #f "~a" mime)))
      (else (throw 'artanis-err 500
                   "generate-data-url: Invalid MIME! Should be symbol or string"
                   mime))))
  (let ((b64 (base64-encode bv/str)))
    (string-concatenate (list "data:" (->mime) (->crypto) (->charset) "," b64))))

(define* (find-ENTRY-path proc #:key (check-only? #f))
  (define (-> p)
    (let ((ff (string-append p "/" *artanis-entry*)))
      (and (file-exists? ff) p)))
  (define (last-path pwd)
    (and=> (string-match "(.*)/.*$" pwd) (lambda (m) (match:substring m 1))))
  ;; FIXME: Maybe we should generate relative path?
  (let lp((pwd (getcwd)))
    (cond
     ((not (string? pwd)) (error find-ENTRY-path "BUG: please report it!" pwd))
     ((string-null? pwd)
      (if check-only?
          #f
          (error find-ENTRY-path
                 "No ENTRY! Are you in a legal Artanis app dir? Or maybe you need to create a new app?")))
     ((-> pwd) => proc)
     (else (lp (last-path pwd))))))

(define (verify-ENTRY entry)
  (cond
   ((not (file-exists? entry)) #f)
   (else
    (let* ((line (call-with-input-file entry read-line))
           (m (string-match "Artanis top-level: (.*)" line)))
      (and m (string=? (match:substring m 1) (dirname entry)))))))

(define (current-appname) (basename (find-ENTRY-path identity)))

(define-syntax draw-expander
  (syntax-rules (rule options method)
    ((_ (options options* ...) rest rest* ...)
     `(,@(list options* ...) ,@(draw-expander rest rest* ...)))
    ((_ (method method*) rest rest* ...)
     `((method ,'method*) ,@(draw-expander rest rest* ...)))
    ((_ (rule url) rest rest* ...)
     `((rule ,url) ,@(draw-expander rest rest* ...)))
    ((_ handler) (list handler))))

(define (remove-ext str)
  (let ((i (string-contains str ".")))
    (substring str 0 i)))

(define (scan-app-components component)
  (let ((toplevel (find-ENTRY-path identity)))
    (map (lambda (f) (string->symbol (remove-ext f)))
         (scandir (format #f "~a/app/~a/" toplevel component)
                  (lambda (f) 
                    (not (or (string=? f ".") 
                             (string=? f ".."))))))))

(define (cache-this-route! url)
  (define (write-header port)
    (format port ";; Do not touch anything!!!~%")
    (format port ";; All things here should be automatically handled properly!!!~%"))
  (define route-cache
    (find-ENTRY-path (lambda (p) (string-append p "/tmp/cache/.route.cache"))))
  (when (not (file-exists? route-cache))
    (format #t "[WARNING] route cache is missing, regenerating...~%")
    (call-with-output-file route-cache write-header))
  (flock route-cache LOCK_EX)
  (let ((rl (call-with-output-file route-cache read)))
    (delete-file route-cache)
    (call-with-output-file route-cache
      (lambda (port)
        (write-header port)
        (write rl port)))
    (flock route-cache LOCK_UN)))

(define (dump-route-from-cache)
  (define toplevel (find-ENTRY-path identity))
  (define route-cache (string-append toplevel "/tmp/cache/.route.cache"))
  (define route (string-append toplevel "/route.scm"))
  (when (file-exists? route) (delete-file route))
  (when (not (file-exists? route-cache))
        (error dump-route-from-cache "Can't find route cache!"))
  (call-with-output-file route
    (lambda (port)
      (for-each (lambda (r)
                  (format port "(get ~s)" r))
                (call-with-input-file route-cache read)))))
