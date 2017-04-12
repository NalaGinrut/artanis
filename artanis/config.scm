;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2016
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

(define-module (artanis config)
  #:use-module (artanis version)
  #:use-module (artanis env)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:export (init-config 
            conf-set!
            get-conf
            current-conf-file
            init-database-config
            current-myhost))

(define server-info artanis-version)
(define *default-conf-file* "/etc/artanis/artanis.conf")

(define (conf-set! k v)
  (hash-set! *conf-hash-table* k v))
(define (get-conf k)
  (hash-ref *conf-hash-table* k))

;; NOTE: These default values will NOT be parsed, so please make sure they're the final
;;       proper values. E.g, if you expect (->list "") to get '() while parsing config file,
;;       you MUST specify '() here. Since it's default value, and it is not going to call
;;       `->list' anywhere.
(define (default-conf-values)
  `(;; for DB namespace
    ((db enable) false)
    ((db dbd) mysql)
    ((db port) 3306)
    ((db addr) "localhost")
    ((db socket) #f)
    ((db username) "root")
    ((db passwd) "")
    ((db name) ,(or (current-appname) "artanis"))
    ((db engine) InnoDB)

    ;; for server namespace
    ((server info) ,artanis-version)
    ((server nginx) #f)
    ((server charset) "utf-8")
    ;; FIXME: use local pages
    ((server syspage path) "/etc/artanis/pages")
    ((server backlog) 128)
    ((server workers) 1)
    ((server wqlen) 64) ; work queue maxlen
    ((server trigger) edge)
    ((server engine) ragnarok)
    ((server polltimeout) 500) ; in miliseconds
    ;; From "HOP, A Fast Server for the Diffuse Web", Serrano.
    ((server bufsize) 12288) ; in Bytes

    ;; for host namespace
    ((host name) #f)
    ((host addr) "127.0.0.1")

    ((host port) 3000)
    ((host family) ipv4)

    ;; for session namespace
    ((session path) "session")
    ((session backend) simple)

    ;; for upload namespace
    ((upload types) (jpg png gif))
    ((upload path) "upload")

    ;; for mail namespace
    ;; ((mail sender) "/usr/sbin/sendmail")

    ;; for cache namespace
    ((cache maxage) 3600) ; in seconds

    ;; for debug mode
    ((debug monitor) '()))) ; user specified monitoring paths

;; Init all fields with default values
(for-each (lambda (x) (conf-set! (car x) (cadr x))) (default-conf-values))

(define-syntax-rule (->bool x)
  (case (string->symbol (string-downcase x))
    ((true on yes enable) #t)
    ((false off no disable) #f)
    (else (error "Invalid boolean item!" x))))

(define-syntax-rule (->list x)
  (map (lambda (e)
         (string->symbol (string-trim-both e)))
       (filter (lambda (s) (not (string-null? s))) (string-split x #\,))))

(define-syntax-rule (->none/str x)
  (case (string->symbol (string-downcase x))
    ((none null #{}#) "")
    (else x)))

(define-syntax-rule (->integer x)
  (let ((i (string->number x)))
    (if (and (number? i) (>= i 0))
        i
        (error "Invalid integer!" x))))

(define-syntax-rule (->symbol x)
  (cond
   ((string? x) (string->symbol x))
   ((symbol? x) x)
   (else (error "Invalid value, should be string or symbol!" x))))

(define (parse-namespace-db item)
  (match item
    (('enable usedb) (conf-set! 'use-db? (->bool usedb))) 
    (('dbd dbd) (conf-set! '(db dbd) (->symbol dbd)))
    (('port port) (conf-set! '(db port) (->integer port)))
    (('addr addr) (conf-set! '(db addr) addr))
    (('name name) (conf-set! '(db name) name))
    (('socket sock) (conf-set! '(db sock) (string->symbol sock)))
    (('username username) (conf-set! '(db username) username))
    (('passwd passwd) (conf-set! '(db passwd) passwd))
    (('engine engine) (conf-set! '(db engine) engine))
    (else (error parse-namespace-db "Config: Invalid item" item))))

(define (parse-namespace-server item)
  (match item
    (('info info) (conf-set! '(server info) (->none/str info)))
    (('nginx nginx) (conf-set! '(server nginx) (->bool nginx)))
    (('charset charset) (conf-set! '(server charset) charset))
    (`(syspage path ,path) (conf-set! '(server syspage path) path))
    (('workers workers) (conf-set! '(server workers) (->integer workers)))
    (('backlog backlog) (conf-set! '(server backlog) (->integer backlog)))
    (('wqlen wqlen) (conf-set! '(server wqlen) (->integer wqlen)))
    (('trigger trigger) (conf-set! '(server trigger) (string->symbol trigger)))
    (('polltimeout polltimeout) (conf-set! '(server polltimeout) (->integer polltimeout)))
    (('bufsize bufsize) (conf-set! '(server bufsize) (->integer bufsize)))
    (('impl impl) (conf-set! '(server impl) (string->symbol impl)))
    (else (error parse-namespace-server "Config: Invalid item" item))))

(define (parse-namespace-host item)
  (match item
    (('name name) (conf-set! '(host name) name))
    (('family family) (conf-set! '(host family) family))
    (('addr addr) (conf-set! '(host addr) addr))
    (('port port) (conf-set! '(host port) (->integer port)))
    (else (error parse-namespace-host "Config: Invalid item" item))))

(define (parse-namespace-session item)
  (match item
    (('path path) (conf-set! '(session path) path))
    (('backend backend) (conf-set! '(session backend) (string->symbol backend)))
    (else (error parse-namespace-session "Config: Invalid item" item))))

(define (parse-namespace-upload item)
  (match item
    (('types types) (conf-set! '(upload types) (->list types)))
    (('path path) (conf-set! '(upload path) path))
    (else (error parse-namespace-upload "Config: Invalid item" item))))

(define (parse-namespace-mail item)
  (match item
    (('sender sender) (conf-set! '(mail sender) sender))
    (else (error parse-namespace-mail "Config: Invalid item" item))))

(define (parse-namespace-cache item)
  (match item
    (('maxage maxage) (conf-set! '(cache maxage) (->integer maxage)))
    (else (error parse-namespace-mail "Config: Invalid item" item))))

(define (parse-namespace-debug item)
  (match item
    (('monitor monitor) (conf-set! '(debug monitor) (->list monitor)))
    (else (error parse-namespace-cache "Config: Invalid item" item))))

(define (parse-config-item item)
  (match item
    (('db rest ...) (parse-namespace-db rest))
    (('server rest ...) (parse-namespace-server rest))
    (('host rest ...) (parse-namespace-host rest))
    (('session rest ...) (parse-namespace-session rest))
    (('upload rest ...) (parse-namespace-upload rest))
    (('mail rest ...) (parse-namespace-mail rest))
    (('cache rest ...) (parse-namespace-cache rest))
    (('debug rest ...) (parse-namespace-debug rest))
    (((? string-null?)) #t) ; skip
    (else (error parse-config-item "Unsupported config namespace!" item))))

(define (parse-line line)
  (call-with-input-string 
   line
   (lambda (port)
     (let lp((next (read-char port)) (key? #t) (word '()) (ret '()))
       (cond
        ((or (eof-object? next)
             (char=? next #\#)) ; skip comment 
         (reverse (cons (list->string (reverse word)) ret)))
        ((char-set-contains? char-set:whitespace next)
         ;; skip all whitespaces
         (lp (read-char port) key? word ret))
        ((and key? (char=? next #\.))
         ;; a namespace end
         (lp (read-char port) key? '() (cons (list->symbol (reverse word)) ret)))
        ((and key? (char=? next #\=))
         ;; value start
         (lp (read-char port) #f '() (cons (list->symbol (reverse word)) ret)))
        ((not key?)
         ;; store chars of value
         (lp (read-char port) key? (cons next word) ret))
        (else
         ;; store chars of key
         (lp (read-char port) key? (cons next word) ret)))))))

(define (init-inner-database-item)
  (define dbd (get-conf '(db dbd)))
  (define port (get-conf '(db port)))
  (define addr (get-conf '(db addr)))
  (define sock (and=> (get-conf '(db sock)) (lambda (x) (not (eq? x 'disable)))))
  (define name (get-conf '(db name)))
  (define username (get-conf '(db username)))
  (define passwd (get-conf '(db passwd)))

  (cond
   ((or (and port addr sock)
        (and port sock)
        (and addr sock))
    (error init-inner-database-item
           (format #f "Conf: either addr:port or sock mode! port=~a, addr=~a, sock=~a"
                   port addr sock)))
   ((eq? dbd 'sqlite3) ; sqlite3 needs only dbname
    (conf-set! 'database `(sqlite3 ,username ,passwd ,name)))
   ((and addr port) ; addr:port mode
    (conf-set! 'database `(,dbd ,username ,passwd ,name (port ,addr ,port))))
   ((and (eq? dbd 'mysql) sock) ; mysql has socket file mode
    (conf-set! 'database `(mysql ,username ,passwd ,name (socketfile ,sock))))
   (else (error init-inner-database-item "Fatal: Invalid database config"))))

(define (init-inner-config-items)
  (and (get-conf 'use-db?) (init-inner-database-item)))

(define (init-database-config dbd user passwd dbname)
  ;; if dbd is not specified, it's mysql in default.
  (conf-set! '(db dbd) (or dbd (get-conf '(db dbd)) 'mysql))
  ;; if username is not specified, it's root in default.
  (conf-set! '(db username) (or user (get-conf '(db username))"root"))
  ;; if passwd is not specified, it's null in default.
  (conf-set! '(db passwd) (or passwd (get-conf '(db passwd)) ""))
  ;; if dbname is not specified. it's artanis in default.
  (conf-set! '(db name) (or dbname (get-conf '(db name)) "artanis"))
  ;; start to init database
  (init-inner-database-item))

;; Could be used by cli for specifying user customized config file.
;; TODO: Users don't have to call init-config themselves, but call cli:
;;       art work -c ./my.conf
;; And init-server should be called automatically.
(define current-conf-file (make-parameter #f))
(define (current-myhost)
  (format #f "http://~a:~a" (get-conf '(host addr)) (get-conf '(host port))))

(define (init-config)
  (define conf-file (current-conf-file)) ; user specified config
  (define fp (open-input-file
              (cond
               ((and conf-file (file-exists? conf-file))
                (format (artanis-current-output) "Loading conf/artanis.conf...")
                conf-file)
               ((file-exists? *default-conf-file*)
                (format (artanis-current-output) "Loading ~a..." *default-conf-file*)
                *default-conf-file*)
               (else 
                (error init-config
                       "Fatal error! Do you have /etc/artanis/artanis.conf?")))))
  (let lp((line (read-line fp)))
    (cond
     ((eof-object? line) #t)
     ((parse-line line) 
      => (lambda (item)
           (parse-config-item item)
           (lp (read-line fp))))
     (else (error init-config "Invalid line" line))))
  (init-inner-config-items)
  (display "done.\n" (artanis-current-output)))
