;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2016,2017,2018,2019,2020
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
  #:use-module (artanis security nss)
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
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 local-eval)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 q)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (web http)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module ((rnrs)
                #:select (get-bytevector-all
                          utf8->string put-bytevector
                          bytevector-u8-ref
                          string->utf8
                          bytevector-length
                          make-bytevector
                          bytevector-s32-native-ref
                          bytevector?
                          define-record-type
                          record-rtd
                          get-string-all
                          bitwise-ior
                          div
                          get-u8
                          get-bytevector-n
                          bytevector->u8-list))
  #:export (regexp-split
            hash-keys cat bv-cat get-global-time sanitize-response
            get-local-time unsafe-random parse-date write-date
            get-file-ext get-global-date get-local-date
            string-substitute nfx static-filename remote-info seconds-now local-time-stamp
            make-expires export-all-from-module!
            alist->hashtable expires->time-utc local-eval-string
            time-expired? valid-method? mmap munmap get-random-from-dev
            string->byteslist list-slice bv-slice uni-basename detect-type-name
            checkout-the-path make-string-template guess-mime prepare-headers
            new-stack new-queue stack-slots queue-slots stack-pop! stack-push!
            stack-top stack-empty? queue-out! queue-in! queue-head queue-tail
            queue-empty? list->stack list->queue stack-remove! queue-remove!
            queue->list stack->list queue-length stack-length
            plist->alist make-db-string-template non-list?
            keyword->string range oah->handler oah->opts string->keyword
            alist->klist alist->kblist is-hash-table-empty?
            symbol-downcase symbol-upcase normalize-column run-before-run!
            sxml->xml-string run-after-request! run-before-response! run-when-DB-init! run-when-sigint-hook
            run-when-sigint! run-when-refresh! make-pipeline HTML-entities-replace eliminate-evil-HTML-entities
            generate-kv-from-post-qstr handle-proper-owner run-after-websocket-handshake!
            generate-data-url verify-ENTRY exclude-dbd
            draw-expander remove-ext scan-app-components cache-this-route!
            dump-route-from-cache generate-modify-time delete-directory
            handle-existing-file check-drawing-method DEBUG
            subbv->string subbv=? bv-read-line bv-read-delimited put-bv
            bv-u8-index bv-u8-index-right build-bv-lookup-table filesize
            plist-remove gen-migrate-module-name gen-local-conf-file with-dbd
            call-with-sigint define-box-type make-box-type unbox-type
            ::define did-not-specify-parameter colorize-string-helper
            colorize-string WARN-TEXT ERROR-TEXT REASON-TEXT
            NOTIFY-TEXT STATUS-TEXT get-trigger get-family get-addr request-path
            response-keep-alive? request-keep-alive? file->bytevector
            procedure-name->string proper-toplevel gen-content-length
            make-file-sender file-sender? file-sender-size file-sender-thunk
            get-string-all-with-detected-charset make-unstop-exception-handler
            artanis-log exception-from-client exception-from-server render-sys-page
            bv-copy/share bv-backward artanis-list-matches get-syspage
            artanis-sys-response char-predicate handle-upload is-valid-table-name?
            is-guile-compatible-server-core? positive-integer? negative-integer?
            io-exception:peer-is-shutdown? io-exception:out-of-memory?
            out-of-system-resources? allow-long-live-connection?
            free-JS-announcement generate-rule-uid gen-cache-file)
  #:re-export (the-environment
               utf8->string
               bytevector?

               uri-decode
               uri-encode
               uri-query
               uri-path

               build-response
               write-response
               write-response-body

               response-version
               response-code
               response-connection
               response-port
               write-response-body
               response-content-length
               read-request
               read-request-body
               request-uri
               request-headers
               request-method
               request-content-length
               request-port

               (nss:md5 . string->md5)
               (nss:sha-1 . string->sha-1)
               (nss:sha-224 . string->sha-224)
               (nss:sha-256 . string->sha-256)
               (nss:sha-384 . string->sha-384)
               (nss:sha-512 . string->sha-512)
               nss:base64-encode
               nss:base64-decode))

(define parse-date (@@ (web http) parse-date))
(define write-date (@@ (web http) write-date))

;; There's a famous rumor that 'urandom' is safer, so we pick it.
(define* (get-random-from-dev #:key (length 8) (uppercase #f))
  (define-syntax-rule (gen-for-even port)
    (let ((bv (get-bytevector-n port (div length 2))))
      (format #f "~{~2,'0x~}" (bytevector->u8-list bv))))

  (call-with-input-file "/dev/urandom"
    (lambda (port)
      ;; generate token directly when length is even, otherwise, add the last byte with 8bit length.
      (and=>
       (cond
        ((even? length) (gen-for-even port))
        (else
         (string-append (gen-for-even port)
                        (format #f "~x" (logand #xf (get-u8 port))))))
       (lambda (str)
         (if uppercase
             (string-upcase str)
             str))))))

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
(define* (cat filename #:optional (out (current-output-port)))
  (define get-string-all (@ (rnrs io ports) get-string-all))
  (let ((str (if (port? filename)
                 (throw 'artanis-err 500 cat
                        "BUG: Shouldn't be port here (~a)!" filename)
                 (call-with-input-file filename get-string-all))))
    (if out
        (display str out)
        str)))

;; WARN: besure that you've already checked the file existtance before!!!
(define* (bv-cat filename #:optional (out (current-output-port)))
  (define get-bytevector-all (@ (rnrs io ports) get-bytevector-all))
  (let ((bv (if (port? filename)
                (throw 'artanis-err 500 bv-cat
                       "BUG: Shouldn't be port here (~a)!" filename)
                (call-with-input-file filename get-bytevector-all))))
    (if out
        (display bv out)
        bv)))

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
  (if (current-toplevel)
      (format #f "~a/pub/~a" (current-toplevel) path)
      (format #f "./pub/~a" path)))

(define-syntax-rule (request-ip req)
  ;; TODO: support AF_INET6 in the future
  (if (port-filename (request-port req))
      ;; Valid socket port
      (inet-ntop AF_INET (sockaddr:addr (getpeername (request-port req))))
      "localtest")) ; fake hostname for testing

(define-syntax-rule (remote-info req)
  (if (get-conf '(server nginx))
      (or (assoc-ref (request-headers req) 'x-real-ip) "Unknown IP")
      (or (request-ip req) "Unknown IP")))

(define *methods-list* '(HEAD GET POST PUT PATCH OPTIONS DELETE))
(define (allowed-method? method)
  (memq method (get-conf '(server allowedmethods))))
(define (valid-method? method)
  (if (and (member method *methods-list*) (allowed-method? method))
      method
      (throw 'artanis-err 405 valid-method? "invalid HTTP method `~a'" method)))

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
(define-public MAP_FAILED #xffffffffffffffff)

;;void *mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset)
(define %mmap
  (pointer->procedure '*
                      (dynamic-func "mmap" (dynamic-link))
                      (list '* size_t int int int long)
                      #:return-errno? #t))

;;redefine mmap as
;;(mmap fd len addr prot flags offset)
(define* (mmap fd len #:key (addr %null-pointer) (prot (list PROT_READ PROT_WRITE)) (flags (list MAP_SHARED)) (offset 0))
  (let ((pv (fold bitwise-ior 0 prot))
        (fv (fold bitwise-ior 0 flags)))
    (call-with-values
        (lambda()
          (%mmap addr len pv fv fd offset))
      (lambda(ret errno)
        (cond
         ((not (= (pointer-address ret) MAP_FAILED)) ret)
         (else (throw 'artanis-error mmap
                      "Error: ~a" (strerror errno))))))))

(define %munmap
  (pointer->procedure int
                      (dynamic-func "munmap" (dynamic-link))
                      (list '* size_t)
                      #:return-errno? #t))

(define (munmap addr len)
  (call-with-values
      (lambda ()
        (%munmap addr len))
    (lambda (ret errno)
      (cond
       ((>= ret 0) ret)
       (else (throw 'artanis-error 500 munmap
                    "Error: ~a" (strerror errno)))))))

(define (file->bytevector filename)
  (let ((size (stat:size (stat filename)))
        (port (open-input-file filename)))
    (let ((bv (pointer->bytevector
               (mmap (port->fdes port) size
                     #:prot (list PROT_READ) #:flags (list MAP_SHARED))
               size)))
      (close port)
      bv)))

;; FIXME: what if len is not even?
(define (string->byteslist str step base)
  (define len (string-length str))
  (let lp((ret '()) (i 0))
    (cond
     ((>= i len) (reverse ret))
     ((zero? (modulo i step))
      (lp (cons (string->number (substring/shared str i (+ i step)) base) ret) (1+ i)))
     (else (lp ret (1+ i))))))

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
  (let* ((len (- hi lo -1))
         (slice ((@ (rnrs) make-bytevector) len)))
    ((@ (rnrs) bytevector-copy!) bv lo slice 0 len)
    slice))

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
  (define (->path p)
    (let ((pp (irregex-split "/" p)))
      (if (char=? (string-ref p 0) #\/)
          (cons (string-append "/" (car pp)) (cdr pp))
          pp)))
  (let ((paths (->path path)))
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
        (else (throw 'artanis-err 500 %make-string-template
                     "invalid mode `~a'" mode)))))
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
                "")))
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
  ((@ (rnrs bytevectors) bytevector=?) bv #vu8()))

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
(define stack-length q-length)
(define (stack->list stk) (list-copy (stack-slots stk)))

(define queue-out! q-pop!)
(define queue-in! enq!)
(define queue-head q-front)
(define queue-tail q-rear)
(define queue-remove! %q-remove-with-key!)
(define queue-empty? q-empty?)
(define queue-length q-length)
(define (queue->list q) (list-copy (queue-slots q)))

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
   (else (throw 'artanis-err 500 normalize-column
                "Invalid type of column `~a'" col))))

(define* (sxml->xml-string sxml #:key (escape? #f))
  (call-with-output-string
   (lambda (port)
     (sxml->xml sxml port escape?))))

(define (run-after-request! proc)
  (add-hook! *after-request-hook* proc))

(define (run-before-response! proc)
  (add-hook! *before-response-hook* proc))

(define (run-before-run! proc)
  (add-hook! *before-run-hook* proc))

(define (run-when-DB-init! proc)
  (add-hook! *DB-conn-init-hook* proc))

(define (run-after-websocket-handshake! proc)
  (add-hook! *after-websocket-handshake-hook* proc))

(define (run-when-sigint! proc)
  (add-hook! *when-sigint-hook* proc))

(define (run-when-sigint-hook)
  (run-hook *when-sigint-hook*))

(define (run-when-refresh! proc)
  (add-hook! *refresh-hook* proc))

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
  '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;") (#\sp . "&nbsp;")
    ("%3C" . "&lt;") ("%3E" . "&gt;") ("%26" . "&amp;") ("%22" . "&quot;")))
;; NOTE: cooked for anti-XSS.
(define (eliminate-evil-HTML-entities content)
  (HTML-entities-replace *terrible-HTML-entities* content))

(define* (generate-kv-from-post-qstr body #:key (no-evil? #f)
                                     (key-converter identity))
  (define cook (if no-evil? eliminate-evil-HTML-entities identity))
  (define (%convert lst)
    (match lst
      ((k v) (list (key-converter k) ((current-encoder) v)))
      (else (throw 'artanis-err 500 generate-kv-from-post-qstr
                   "Fatal! Can't be here `~a'!" lst))))
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
           (throw 'artanis-err 500 handle-proper-owner (->err-reason exe reason) file))
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
      (else (throw 'artanis-err 500 generate-data-url
                   "Invalid MIME! Should be symbol or string"
                   mime))))
  (let ((b64 (nss:base64-encode bv/str)))
    (string-concatenate (list "data:" (->mime) (->crypto) (->charset) "," b64))))

(define (verify-ENTRY entry)
  (cond
   ((not (file-exists? entry))
    (format (artanis-current-output) "ENTRY file is missing!~%")
    #f)
   (else
    (let ((line (call-with-input-file entry read-line)))
      (string=? line ";; This an Artanis ENTRY file, don't remove it!")))))

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

(define* (scan-app-components component #:optional (sym? #t))
  (define-syntax-rule (-> x)
    (if sym? (string->symbol x) x))
  (let* ((toplevel (current-toplevel))
         (cpath (format #f "~a/app/~a/" toplevel component)))
    (cond
     ((file-exists? cpath)
      (map (lambda (f) (-> (remove-ext f)))
           (scandir cpath
                    (lambda (f)
                      (not (or (string=? f ".")
                               (string=? f "..")
                               (string=? f ".gitkeep")))))))
     (else '()))))

(define (cache-this-route! url meta)
  (define (write-header port)
    (format port ";; Do not touch anything!!!~%")
    (format port ";; All things here should be automatically handled properly!!!~%"))
  (define route-cache (string-append (current-toplevel) "/tmp/cache/route.cache"))
  (when (not (file-exists? route-cache))
    (format (artanis-current-output) "Regenerating route cache ...~%")
    (call-with-output-file route-cache
      (lambda (port) (write-header port) (write '() port))))
  (when (and url meta)
    (let ((rl (call-with-input-file route-cache read)))
      (delete-file route-cache)
      (call-with-output-file route-cache
        (lambda (port)
          (flock port LOCK_EX)
          (write-header port)
          (if (eof-object? rl)
              (write '() port)
              (write
               (assoc-set! rl url
                           (map (lambda (e)
                                  (if (procedure? e)
                                      (object->string e)
                                      e)) meta))
               port
               ))
          (flock port LOCK_UN))))))

(define (dump-route-from-cache)
  (define toplevel (current-toplevel))
  (define route-cache (string-append toplevel "/tmp/cache/route.cache"))
  (define route (string-append toplevel "/.route"))
  (define (load-customized-router)
    (let ((croute (string-append toplevel "conf/route")))
      (cond
       ((not (file-exists? croute)) #t) ; No customized route
       (else
        (use-modules (artanis mvc route)) ; black magic to make Guile happy
        (load croute)))))
  (when (file-exists? route) (delete-file route))
  (when (not (file-exists? route-cache))
    (cache-this-route! #f #f)
    (dump-route-from-cache))
  (let ((rl (call-with-input-file route-cache read)))
    (cond
     ((eof-object? rl)
      (cache-this-route! #f #f)
      (dump-route-from-cache))
     (else
      (call-with-output-file route
        (lambda (port)
          (for-each (lambda (r)
                      (let* ((meta (cdr r))
                             (rule (assq-ref meta 'rule))
                             (method (and=> (assq-ref meta 'method) car)))
                        (format port "~2t(~a ~s)~%"
                                (if method method 'get)
                                (if rule rule (car r)))))
                    rl)))
      ;; load customized router
      (load-customized-router)))))

(define* (delete-directory dir #:optional (checkonly? #f))
  (cond
   ((and (file-is-directory? dir) (file-exists? dir))
    (system (format #f "rm -f ~a" dir)))
   (else
    (and (not checkonly?)
         (error delete-directory "Not a directory or doesn't exist " dir)))))

;; TODO: handle it more elegantly
(define* (handle-existing-file path #:optional (dir? #f))
  (let* ((pp (if dir? (dirname path) path))
         (component (basename (dirname pp)))
         (name (car (string-split (basename pp) #\.))))
    (cond
     ((cmd:is-force?)
      (if (file-is-directory? path)
          (delete-directory path)
          (delete-file path)))
     ((cmd:is-skip?)
      (format (artanis-current-output) "skip ~10t app/~a/~a~%" component name))
     (else
      (format (artanis-current-output)
              "~a `~a' exists! (Use --force/-f to overwrite or --skip/-s to ignore)~%"
              (string-upcase component) name)
      (exit 1)))))

;; Check if all methods are valid
(define (check-drawing-method lst)
  (define errstr "Invalid drawing method, shouldn't contain '/' ")
  (for-each (lambda (name)
              (when (not (irregex-match "[^/]+" name))
                (error check-drawing-method errstr name)))
            lst)
  lst)

(define (subbv->string bv encoding start len)
  (call-with-output-string
   (lambda (port)
     (set-port-encoding! port encoding)
     (put-bytevector port bv start len))))

(define* (bv-u8-index bv u8 #:optional (time 1))
  (let ((len (bytevector-length bv)))
    (let lp((i 0) (t 1))
      (cond
       ((>= i len) #f)
       ((= (bytevector-u8-ref bv i) u8)
        (if (= t time) i (lp (1+ i) (1+ t))))
       (else (lp (1+ i) t))))))

(define* (bv-u8-index-right bv u8 #:optional (time 1))
  (let ((len (bytevector-length bv)))
    (let lp((i (1- len)) (t 1))
      (cond
       ((< i 0) #f)
       ((= (bytevector-u8-ref bv i) u8)
        (if (= t time) i (lp (1- i) (1+ t))))
       (else (lp (1- i) t))))))

(define* (subbv=? bv bv2 #:optional (start 0) (end (1- (bytevector-length bv))))
  (and (<= (bytevector-length bv2) (bytevector-length bv))
       (let lp((i end) (j (1- (bytevector-length bv2))))
         (cond
          ((< i start) #t)
          ((= (bytevector-u8-ref bv i) (bytevector-u8-ref bv2 j))
           (lp (1- i) (1- j)))
          (else #f)))))

;; return position after delim
(define* (bv-read-delimited bv delim #:optional (start 0) (end (bytevector-length bv)))
  (define len (- end start -1))
  (let lp((i start))
    (cond
     ((> i end) #f)
     ((= (bytevector-u8-ref bv i) delim) i)
     (else (lp (1+ i))))))

;; return position after newline
(define* (bv-read-line bv #:optional (start 0) (end (bytevector-length bv)))
  (bv-read-delimited bv 10 start end))

(define (put-bv port bv from to)
  (put-bytevector port bv from (- to from -1)))

;; TODO: build a char occurence indexing table
(define (build-bv-lookup-table bv)
  (let ((ht (make-hash-table)))
    (for-each (lambda (i)
                (hash-set! ht (bytevector-u8-ref bv i) #t))
              (iota (bytevector-length bv)))
    ht))

(define Gbytes (ash 1 30))
(define Mbytes (ash 1 20))
(define Kbytes (ash 1 10))
(define (filesize size)
  (cond
   ((>= size Gbytes)
    (format #f "~,1fGiB" (/ size Gbytes)))
   ((>= size Mbytes)
    (format #f "~,1fMiB" (/ size Mbytes)))
   ((>= size Kbytes)
    (format #f "~,1fKiB" (/ size Kbytes)))
   (else (format #f "~a Bytes" size))))

(define* (plist-remove lst k #:optional (no-value? #f))
  (let lp((next lst) (kk '__) (ret '()))
    (cond
     ((null? next) (values (reverse ret) kk))
     ((eq? (car next) k)
      (if no-value?
          (lp (cdr next) (car next) ret)
          (lp (cddr next) (list (car next) (cadr next)) ret)))
     (else (lp (cdr next) kk (cons (car next) ret))))))

(define *name-re* (string->irregex "([^.]+)\\.scm"))
(define (gen-migrate-module-name f)
  (cond
   ((irregex-search *name-re* (basename f))
    => (lambda (m) (irregex-match-substring m 1)))
   (else (throw 'artanis-err 500 gen-migrate-module-name
                "Wrong parsing of module name, shouldn't be here!" f))))

(define (gen-local-conf-file)
  (format #f "~a/conf/artanis.conf" (current-toplevel)))

(define-syntax-rule (with-dbd dbd0 body ...)
  (let ((dbd1 (get-conf '(db dbd))))
    (cond
     ((or (and (list? dbd0) (memq dbd1 dbd0)) (eq? dbd1 dbd0)) body ...)
     (else
      (throw 'artanis-err 500 'with-dbd
             "This is only supported by `~a', but the current dbd is `~a'"
             dbd0 dbd1
             'body ...)))))

(define-syntax-rule (exclude-dbd dbds body ...)
  (let ((dbd (get-conf '(db dbd))))
    (cond
     ((memq dbd dbds)
      (throw 'artanis-err 500 'exclude-dbd
             "This isn't supported by `~a', please check it out again!"
             dbds
             'body ...))
     (else body ...))))

(define-syntax-rule (DEBUG fmt args ...)
  (when (get-conf 'debug-mode)
    (format (artanis-current-output) fmt args ...)))

(define call-with-sigint
  (if (not (provided? 'posix))
      (lambda (thunk handler-thunk) (thunk))
      (lambda (thunk handler-thunk)
        (let ((handler #f))
          (catch 'interrupt
            (lambda ()
              (dynamic-wind
                (lambda ()
                  (set! handler
                        (sigaction SIGINT (lambda (sig)
                                            (run-when-sigint-hook)
                                            (throw 'interrupt)))))
                thunk
                (lambda ()
                  (if handler
                      ;; restore Scheme handler, SIG_IGN or SIG_DFL.
                      (sigaction SIGINT (car handler) (cdr handler))
                      ;; restore original C handler.
                      (sigaction SIGINT #f)))))
            (lambda (k . _) (handler-thunk)))))))

(define-syntax-rule (define-box-type name)
  (define-record-type name (fields treasure)))

(define-macro (make-box-type bt v)
  (list (symbol-append 'make- bt) v))

(define-syntax-rule (box-type-treasure t)
  ((@ (rnrs) record-accessor) (record-rtd t) 0))

(define-syntax-rule (unbox-type t)
  (let ((treasure-getter (box-type-treasure t)))
    (treasure-getter t)))

(define (socket-port? sp)
  (and (port? sp) (eq? (port-filename sp) 'socket)))

(define (detect-type-name o)
  (define r6rs-record? (@ (rnrs) record?))
  (define r6rs-record-type-name (@ (rnrs) record-type-name))
  (define guile-specific-record? (@ (guile) record?))
  (define (guile-specific-record-name o)
    ((@ (guile) record-type-name)
     ((@ (guile) record-type-descriptor) o)))
  (cond
   ;; NOTE: record? is for R6RS record-type, but record-type? is not
   ((r6rs-record? o) (r6rs-record-type-name (record-rtd o)))
   ((guile-specific-record? o) (guile-specific-record-name o))
   ((symbol? o) 'symbol)
   ((string? o) 'string)
   ((integer? o) (if (positive? o) '+int '-int))
   ((number? o) 'num)
   ((thunk? o) 'thunk)
   ((procedure? o) 'proc)
   ((vector? o) 'vector)
   ((pair? o) 'pair)
   ((list? o) 'list)
   ((bytevector? o) 'bv)
   ((port? o) 'port)
   ((boolean? o) 'boolean)
   (else 'ANY)))

(define (check-args-types op args)
  (define (check-eq? actual-type expect-type)
    (case expect-type
      ((+int) (eq? actual-type '+int))
      ((-int) (eq? actual-type '-int))
      ((int) (memq actual-type '(-int +int)))
      (else (eq? expect-type actual-type))))
  (match (procedure-property op 'type-anno)
    (((targs ...) '-> (func-types ...))
     (for-each
      (lambda (v e)
        (or (eq? e 'ANY)
            (check-eq? (detect-type-name v) e)
            (begin
              (DEBUG "(~{~a~^ ~}) =? (~{~a~^ ~})~%" targs args)
              (throw 'artanis-err 500 check-args-types
                     "~a: Argument ~a is a `~a' type, but I expect type `~a'"
                     op v (detect-type-name v) e))))
      args targs))
    (else (throw 'artanis-err 500 check-args-types "Invalid type annotation `~a'"
                 (procedure-property op 'type-anno)))))

(define (check-function-types op fret)
  (match (procedure-property op 'type-anno)
    (((targs ...) '-> (func-types ...))
     (for-each
      (lambda (v e)
        (or (eq? e 'ANY)
            (eq? (detect-type-name v) e)
            (throw 'artanis-err 500 check-function-types
                   "`Return value ~a(~a) is expected to be type `~a'"
                   v (detect-type-name v) e)))
      fret func-types))
    (else (throw 'artanis-err 500 check-function-types
                 "Invalid type annotation `~a'"
                 (procedure-property op 'type-anno)))))

(define (detect-and-set-type-anno! op ftypes atypes)
  (let ((type `(,atypes -> ,ftypes)))
    (set-procedure-property! op 'type-anno type)
    type))

;; NOTE: This macro can detect multi return values.
;; TODO:
;; 1. support multi-types, say, string/bv, maybe not easy to do it faster?
(define-syntax ::define
  (syntax-rules (-> :anno:)
    ((_ (op args ...) (:anno: (targs ...) -> func-types ...) body ...)
     (begin
       (define (op args ...)
         (when (get-conf 'debug-mode) (check-args-types op (list args ...)))
         (call-with-values
             (lambda () body ...)
           (lambda ret
             (when (get-conf 'debug-mode)
               (eq? (detect-type-name ret) (check-function-types op ret)))
             (apply values ret))))
       (detect-and-set-type-anno! op '(func-types ...) '(targs ...))))))

(define-syntax-rule (did-not-specify-parameter what)
  (format #f "`current-~a' isn't specified, it's likely a bug!" what))

;; Text-coloring helper functions, borrowed from guile-colorized
(define *color-list*
  `((CLEAR       .   "0")
    (RESET       .   "0")
    (BOLD        .   "1")
    (DARK        .   "2")
    (UNDERLINE   .   "4")
    (UNDERSCORE  .   "4")
    (BLINK       .   "5")
    (REVERSE     .   "6")
    (CONCEALED   .   "8")
    (BLACK       .  "30")
    (RED         .  "31")
    (GREEN       .  "32")
    (YELLOW      .  "33")
    (BLUE        .  "34")
    (MAGENTA     .  "35")
    (CYAN        .  "36")
    (WHITE       .  "37")
    (ON-BLACK    .  "40")
    (ON-RED      .  "41")
    (ON-GREEN    .  "42")
    (ON-YELLOW   .  "43")
    (ON-BLUE     .  "44")
    (ON-MAGENTA  .  "45")
    (ON-CYAN     .  "46")
    (ON-WHITE    .  "47")))

(define (get-color color)
  (assq-ref *color-list* color))

(define (generate-color colors)
  (let ((color-list
         (filter-map get-color colors)))
    (if (null? color-list)
        ""
        (string-append "\x1b[" (string-join color-list ";" 'infix) "m"))))

(define* (colorize-string-helper color str control #:optional (rl-ignore #f))
  (if rl-ignore
      (string-append "\x01" (generate-color color) "\x02" str "\x01" (generate-color control) "\x02")
      (string-append (generate-color color) str (generate-color control))))

(define* (colorize-string str color)
  "Example: (colorize-string \"hello\" '(BLUE BOLD))"
  (colorize-string-helper color str '(RESET) (using-readline?)))

(define-syntax-rule (WARN-TEXT str) (colorize-string str '(YELLOW)))
(define-syntax-rule (ERROR-TEXT str) (colorize-string str '(RED)))
(define-syntax-rule (REASON-TEXT str) (colorize-string str '(CYAN)))
(define-syntax-rule (NOTIFY-TEXT str) (colorize-string str '(WHITE)))
(define-syntax-rule (STATUS-TEXT num) (colorize-string (object->string num)'(WHITE)))

(define (get-trigger)
  (case (get-conf '(server trigger))
    ((edge) (@ (artanis server epoll) EPOLLET))
    ((level) 0)
    (else (throw 'artanis-err 500 get-trigger "Invalid (server trigger)!"
                 (get-conf '(server trigger))))))

(define (get-family)
  (case (get-conf '(host family))
    ((ipv4) AF_INET)
    ((ipv6) AF_INET6)
    (else (throw 'artanis-err 500 get-family "Invalid (host family)!"
                 (get-conf '(host family))))))

(define (get-addr)
  (let ((host (get-conf '(host addr)))
        (family (get-family)))
    (if (and host (not (string=? host "127.0.0.1")))
        (inet-pton family host)
        INADDR_LOOPBACK)))

(define (request-path req)
  (uri-path (request-uri req)))

(define (response-keep-alive? response)
  (let ((v (response-version response)))
    (and (or (< (response-code response) 400)
             (= (response-code response) 404))
         (case (car v)
           ((1)
            (case (cdr v)
              ;; NOTE: HTTP/1.1 treat all connection keep-alive
              ;;       unless it requires `close' explicityly
              ((1) (not (memq 'close (response-connection response))))
              ;; HTTP/1.0 needs explicit keep-alive notice
              ((0) (memq 'keep-alive (response-connection response)))))
           (else #f)))))

;; NOTE: The order matters
(define (request-keep-alive? request)
  (or (equal? (request-upgrade request) '(websocket))
      (equal? (request-connection request) '(keep-alive))
      (equal? (request-version request) '(1 . 1))))

(define (procedure-name->string proc)
  (symbol->string (procedure-name proc)))

(define-syntax-rule (proper-toplevel)
  (or (current-toplevel) ""))

(define-record-type file-sender
  (fields size thunk))

(define (gen-content-length body)
  (let ((get-length (lambda ()
                      (cond
                       ((bytevector? body) (bytevector-length body))
                       ((file-sender? body) (file-sender-size body))
                       ((string? body)
                        (throw 'artanis-err 500 gen-content-length
                               "BUG: body should have been converted to bytevector! ~a"
                               body))
                       (else (throw 'artanis-err 500 gen-content-length
                                    "Invalid body ~a" body))))))
    `(content-length . ,(if body (get-length) 0))))

(define (get-string-all-with-detected-charset filename)
  (call-with-input-file filename
    (lambda (port)
      (set-port-encoding! port (get-conf '(server charset)))
      (get-string-all port))))

(define (get-syspage file)
  (let ((local-syspage (format #f "~a/sys/pages/~a"
                               (current-toplevel) file)))
    (if (file-exists? local-syspage)
        local-syspage
        (let ((sys-syspage (format #f "~a/~a" (get-conf '(server syspage path)) file)))
          (and (file-exists? sys-syspage)
               sys-syspage)))))

(define (syspage-show status)
  (let* ((file (format #f "~a.html" status))
         (syspage (get-syspage file)))
    (if syspage
        (bv-cat syspage #f)
        #vu8())))

;; ENHANCE: use colored output
(define* (artanis-log blame-who? status mime #:key (port (current-error-port))
                      (request #f))
  (case blame-who?
    ((client)
     (when (not request)
       (error "artanis-log: Fatal bug! Request shouldn't be #f here!~%"))
     (let* ((uri (request-uri request))
            (path (uri-path uri))
            (qstr (uri-query uri))
            (method (request-method request)))
       (format port "[Remote] ~a @ ~a~%" (remote-info request) (local-time-stamp))
       (format port "[Request] method: ~a, path: ~a, query: ~a~%" method path qstr)
       (format port "[Response] status: ~a, MIME: ~a~%~%" status mime)))
    ((server)
     (format port "[Server] ~a @ ~a~%" (get-conf '(host addr)) (local-time-stamp))
     (format port "[Response] status: ~a, MIME: ~a~%~%" status mime))
    (else (error "artanis-log: Fatal BUG here!"))))

(define *guile-compatible-server-core*
  '(guile fibers))

(define (is-guile-compatible-server-core? name)
  (memq name *guile-compatible-server-core*))

(define (render-sys-page blame-who? status request)
  (artanis-log blame-who? status 'text/html #:request request)
  (let* ((charset (get-conf '(server charset)))
         (mtime (generate-modify-time (current-time)))
         (guile-compt-serv? (is-guile-compatible-server-core? (get-conf '(server engine))))
         (response
          (build-response #:code status
                          #:headers `((server . ,(get-conf '(server info)))
                                      (last-modified . ,mtime)
                                      (content-type . (text/html (charset . ,charset))))))
         (body (cond
                ((resources-collecting?)
                 #vu8(0)) ; Don't open any file since we don't have resources now
                ((get-syspage-handler status)
                 => (lambda (thunk)
                      (let ((body (thunk)))
                        (cond
                         ((string? body)
                          (string->bytevector body (get-conf '(server charset))))
                         ((bytevector? body) body)
                         (else (syspage-show status))))))
                (else (syspage-show status)))))
    (if guile-compt-serv?
        (values response body)
        (values response body 'exception))))

(define (format-status-page/client status request)
  (format (current-error-port) (ERROR-TEXT "[EXCEPTION] ~a is abnormal request, status: ~a, ")
          (uri-path (request-uri request)) status)
  (display "rendering a sys page for it...\n" (current-error-port))
  (render-sys-page 'client status request))

(define (format-status-page/server status)
  (format (current-error-port) "[SERVER ERROR] Internal error from server-side, ")
  (format (current-error-port) "rendering a ~a page for client ...~%" status)
  (render-sys-page 'server status #f))

(define (exception-from-client request)
  (lambda (status)
    (format-status-page/client status request)))

(define (exception-from-server status)
  (format-status-page/server status))

(define *rf-re* (string->irregex ".*/artanis/artanis/(.*)$"))
(define (->reasonable-file filename)
  (if (string? filename)
      (let ((m (irregex-search *rf-re* filename)))
        (if m
            (format #f "artanis/~a" (irregex-match-substring m 1))
            filename))
      "In unknown file"))
(define-syntax-rule (make-unstop-exception-handler syspage-generator)
  (let ((port (current-error-port))
        (filename (current-filename)))
    (lambda (k . e)
      (match e
        (((? procedure? subr) (? string? msg) . args)
         (format port "Captured in <~a>~%" (WARN-TEXT (->reasonable-file filename)))
         (when subr (format port "In procedure ~a :~%"
                            (WARN-TEXT (procedure-name->string subr))))
         (apply format port
                (REASON-TEXT (string-append "[REASON] " msg))
                args)
         (newline port))
        (((? integer? status) (or (? symbol? subr) (? procedure? subr))
          (? string? msg) . args)
         (format port "HTTP ~a~%" (STATUS-TEXT status))
         (format port "Captured in <~a>~%" (WARN-TEXT (->reasonable-file filename)))
         (when subr (format port "Threw in procedure ~a :~%"
                            (WARN-TEXT (cond
                                        ((procedure? subr)
                                         (procedure-name->string subr))
                                        ((symbol? subr)
                                         (symbol->string subr))
                                        (else subr)))))
         (apply format port
                (REASON-TEXT (string-append "[REASON] " msg))
                args)
         (newline port)
         (syspage-generator status))
        (else
         (format port "~a - ~a~%"
                 (WARN-TEXT
                  "BUG: invalid exception format, but we throw it anyway!")
                 e)
         (apply throw k e))))))

(define* (bv-copy/share bv #:key (from 0) (type 'vu8)
                        (size (- (bytevector-length bv) from)))
  (when (> size (- (bytevector-length bv) from))
    (error bv-copy/share
           (format #f "Size(~a) is larger than the length(~a) - from(~a)!"
                   size (bytevector-length bv) from)))
  (when (>= from (bytevector-length bv))
    (error bv-copy/share
           (format #f "Can't copy from the end of the bytevector (~a)!"
                   from)))
  (let* ((ptr (bytevector->pointer bv))
         (new-ptr (make-pointer (+ (pointer-address ptr) from))))
    (pointer->bytevector new-ptr size 0 type)))

(define* (bv-backward bv offset #:key (type 'vu8) (extend 0))
  (let* ((ptr (bytevector->pointer bv))
         (new-ptr (make-pointer (- (pointer-address ptr) offset)))
         (len (bytevector-length bv)))
    (pointer->bytevector new-ptr (+ len extend) 0 type)))

(define (artanis-list-matches irx str)
  (let lp ((start 0) (ret '()))
    (let ((m (irregex-search irx str start)))
      (if m
          (lp (irregex-match-end-index m) (cons m ret))
          ret))))

(define (artanis-sys-response status port bv-body)
  (build-response #:code status
                  #:port port
                  #:headers `((server . ,(get-conf '(server info)))
                              ,(gen-content-length bv-body)
                              (content-type . (text/html)))))

(define (char-predicate string)
  (let ((cs (string->char-set string)))
    (lambda (c)
      (and (not (eof-object? c)) (char-set-contains? cs c)))))

(define (handle-upload thunk)
  (catch 'system-error
    thunk
    (lambda e
      (let ((errno (system-error-errno e)))
        (cond
         ((= errno ENOMEM)
          ;; NOTE: Out of memory, call (gc) and throw 507
          (format (artanis-current-output) "No memory! Run GC now!~%")
          (gc)
          (throw 'artanis-err 507 handle-upload
                 "Server is out of RAMs, please extend more RAMs!~%"))
         ((= errno EIO)
          ;; NOTE: The storage device was disconnected, just throw 507
          (throw 'artanis-err 507 handle-upload
                 "Server is not available, maybe storage media was disconnected?~%"))
         ((= errno ENOSPC)
          ;; NOTE: no space for uploading, just throw 507
          (throw 'artanis-err 507 handle-upload
                 "Server has insufficient storage space!~%"))
         (else
          ;; nothing noticed, re-throw it to next level.
          (apply throw e)))))))

;;for verify db table name
(define invalid-char-set? (char-predicate "*&-{}[]?.\\%$#@!,"))

(define is-valid-table-name?
  (lambda (name)
    (not (string-any invalid-char-set? name))))

(define (positive-integer? x)
  (and (integer? x) (positive? x)))

(define (negative-integer? x)
  (and (integer? x) (negative? x)))

;; WARNING: Don't use = here, must use eqv? since it's not always numbers
(define (io-exception:peer-is-shutdown? e)
  (and (eq? (car e) 'system-error)
       (let ((errno (system-error-errno e)))
         (or (eqv? errno EPIPE) ; broken pipe
             (eqv? errno EIO) ; write to a closed socket
             (eqv? errno ECONNRESET))))) ; shutdown by peer

;; WARNING: Don't use = here, must use eqv? since it's not always numbers
(define (io-exception:out-of-memory? e)
  (and (eq? (car e) 'system-error)
       (let ((errno (system-error-errno e)))
         (or (eqv? errno ENOMEM) ; no memory
             (eqv? errno ENOBUFS)))))  ; no buffer could be allocated

(define (out-of-system-resources? e)
  (and (eq? (car e) 'system-error)
       (let ((errno (system-error-errno e)))
         (eqv? errno EMFILE)))) ; no more file could be opened

(define (allow-long-live-connection?)
  (> (get-conf '(server timeout)) 0))

;; Story: When I released GNU Artanis-0.2.1, RMS had asked me if I can
;;        support LibreJS for freeing Javascript code in the generated code.
;;        I promised I will. This took almost one year since I was very
;;        busy on developing a new product (partly use GNU Artanis, of course),
;;        so it's delayed. Till few months ago, RMS sent mail to ask me
;;        if I'm ready for it, I realized that the release of GNU Artanis
;;        is much delayed.
;;        LibreJS is a way to detect non-free JS code to help you to avoid
;;        the non-trivial JS code for certain hidden features. Some of them
;;        are dangerous, some are stolen your privacy, some are just don't
;;        let you know what's going on.
;;        GNU Artanis may generate JS code automatically, they're licensed as
;;        free software definitely. This free-JS-announcement is used to
;;        license all the generated JS code in the page to GPLv3.
;;        This announcement is useful when you have LibreJS plugin in your
;;        browser to detect the JS automatically.
;;        Although you're free to relicense the code to whatever you prefer,
;;        I wish you could free the code, no matter what license name it is.
(define free-JS-announcement
  "
    <script>
       /*
        @licstart  The following is the entire license notice for the
        JavaScript code in this page.

        Copyright (C) 2014  Loic J. Duros

        The JavaScript code in this page is free software: you can
        redistribute it and/or modify it under the terms of the GNU
        General Public License (GNU GPL) as published by the Free Software
        Foundation, either version 3 of the License, or (at your option)
        any later version.  The code is distributed WITHOUT ANY WARRANTY;
        without even the implied warranty of MERCHANTABILITY or FITNESS
        FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.

        As additional permission under GNU GPL version 3 section 7, you
        may distribute non-source (e.g., minimized or compacted) forms of
        that code without the copy of the GNU GPL normally required by
        section 4, provided you include this license notice and a URL
        through which recipients can access the Corresponding Source.


        @licend  The above is the entire license notice
        for the JavaScript code in this page.
        */
    </script>
")

;; FIXME: MD5 may not be the best choice
(define-syntax-rule (generate-rule-uid rule)
  (string->md5 rule))

(define (gen-cache-file path)
  (define-syntax-rule (-> str)
    (string-trim-both
     (irregex-replace/all "[/]+" str "_")
     (lambda (x) (memv x '(#\sp #\_)))))
  (let ((p (-> path)))
    (if (string-null? p)
        (format #f "~a/tmp/cache/index.html" (current-toplevel))
        (format #f "~a/tmp/cache/~a.html" (current-toplevel) (-> path)))))
