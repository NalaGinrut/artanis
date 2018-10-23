;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2016,2017,2018
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
  #:use-module (artanis irregex)
  #:use-module (artanis env)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
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
    ((db enable) #f "")
    ((db dbd) mysql "")
    ((db proto) tcp "")
    ((db addr) "127.0.0.1:3306" "")
    ((db socketfile) #f "")
    ((db username) "root" "")
    ((db passwd) "" "")
    ((db name) ,(or (current-appname) "artanis") "")
    ((db engine) InnoDB "")
    ((db poolsize) 64 "")
    ((db pool) increase "") ; increase or fixed
    ;; whether to encode params each time
    ;; NOTE: If you enable db.encodeparams then it's better to decode the related value
    ;;       twice in the client-side, since some requests may be sent from browsers, and
    ;;       they're already encoded.
    ((db encodeparams) #f "")
    ((db lpc) #f "") ; enable LPC, this may require Redis

    ;; for server namespace
    ((server info) ,artanis-version "")
    ((server nginx) #f "")
    ((server charset) "utf-8" "")
    ;; FIXME: use local pages
    ((server syspage path) "/etc/artanis/pages" "")
    ((server backlog) 128 "")
    ((server wqlen) 64 "") ; work queue maxlen
    ((server trigger) edge "")
    ((server engine) ragnarok "")
    ((server timeout) 60 "") ; in seconds, zero for always short live connections.
    ((server polltimeout) 500 "") ; in miliseconds
    ;; From "HOP, A Fast Server for the Diffuse Web", Serrano.
    ((server bufsize) 12288 "") ; in Bytes
    ;; NOTE: Only for Linux-3.9+
    ;;       One kernel features is necessary:
    ;;       SO_REUSEPORT (since 3.9)
    ;;       Allows mutiple servers to listen to the same socket port, say 8080.
    ((server multi) #t "")
    ((server websocket) #t "")
    ((server pub) "pub" "") ; the public directory
    ((server sendfile) #f "")

    ;; for WebSocket
    ((websocket maxpayload) ,(1- (ash 1 64)) "") ; in bytes (only for fragment)
    ((websocket minpayload) 1 "") ; enlarge it to avoid slow 1-byte attack (only for fragment)
    ((websocket fragment) 4096 "") ; the fragment size in bytes
    ((websocket maxsize) ,(ash 1 10) "") ; in bytes, the upload size from websocket
    ((websocket timeout) 64 "") ; timeout in websocket connnection, in seconds

    ;; for host namespace
    ((host name) #f "")
    ((host addr) "127.0.0.1" "")

    ((host port) 3000 "")
    ((host family) ipv4 "")

    ;; for session namespace
    ((session path) "session" "")
    ((session backend) simple "")

    ;; for upload namespace
    ((upload types) (jpg png gif) "")
    ((upload path) "upload" "")
    ((upload size) 5242880 "") ; 5M

    ;; for mail namespace
    ;; ((mail sender) "/usr/sbin/sendmail")

    ;; for cache namespace
    ((cache maxage) 3600 "") ; in seconds

    ;; for debug mode
    ((debug enable) #f "")
    ((debug monitor) () ""))) ; user specified monitoring paths

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

(define-syntax-rule (->none/boolean x)
  (case (string->symbol (string-downcase x))
    ((false no none null) #f)
    (else x)))

(define-syntax-rule (->integer x)
  (let ((i (string->number x)))
    (if (and (number? i) (>= i 0))
        i
        (error "Invalid integer!" x))))

(define-syntax-rule (->ws-payload x)
  (let ((size (->integer x)))
    (if (and (> size 0) (< size (ash 1 64)))
        size
        (error "Invalid websocket payload size" x))))

(define-syntax-rule (->symbol x)
  (cond
   ((string? x) (string->symbol x))
   ((symbol? x) x)
   (else (error "Invalid value, should be string or symbol!" x))))

(define-syntax-rule (->dbname x)
  (if (string-null? x )
      #f
      x))

(define-syntax-rule (->dbd x)
  (let ((d (->symbol x)))
    (if (eq? 'mariadb d)
        'mysql
        d)))

(define *pool-modes* '(increase fixed))
(define-syntax-rule (->pool-mode x)
  (let ((p (string->symbol x)))
    (if (memq p *pool-modes*)
        p
        (error (format
                #f
                "Invalid db.pool value, we accept: ~{~a~^,~}" *pool-modes*)))))

(define (parse-namespace-db item)
  (match item
    (('enable usedb) (conf-set! '(db enable) (->bool usedb)))
    (('dbd dbd) (conf-set! '(db dbd) (->dbd dbd)))
    (('proto proto) (conf-set! '(db proto) (->symbol proto)))
    (('socketfile socketfile) (conf-set! '(db socketfile) (->none/boolean socketfile)))
    (('addr addr) (conf-set! '(db addr) addr))
    (('name name) (conf-set! '(db name) (->dbname name)))
    (('username username) (conf-set! '(db username) username))
    (('passwd passwd) (conf-set! '(db passwd) passwd))
    (('engine engine) (conf-set! '(db engine) engine))
    (('poolsize poolsize) (conf-set! '(db poolsize) (->integer poolsize)))
    (('pool pool) (conf-set! '(db pool) (->pool-mode pool)))
    (('encodeparams encodeparams) (conf-set! '(db encodeparams) (->bool encodeparams)))
    (('lpc lpc) (conf-set! '(db lpc) lpc) (->bool lpc))
    (else (error parse-namespace-db "Config: Invalid item" item))))

(define (parse-namespace-server item)
  (match item
    (('info info) (conf-set! '(server info) (->none/str info)))
    (('nginx nginx) (conf-set! '(server nginx) (->bool nginx)))
    (('charset charset) (conf-set! '(server charset) charset))
    (`(syspage path ,path) (conf-set! '(server syspage path) path))
    (('backlog backlog) (conf-set! '(server backlog) (->integer backlog)))
    (('wqlen wqlen) (conf-set! '(server wqlen) (->integer wqlen)))
    (('trigger trigger) (conf-set! '(server trigger) (string->symbol trigger)))
    (('timeout timeout) (conf-set! '(server timeout) (->integer timeout)))
    (('polltimeout polltimeout) (conf-set! '(server polltimeout) (->integer polltimeout)))
    (('bufsize bufsize) (conf-set! '(server bufsize) (->integer bufsize)))
    (('impl impl) (conf-set! '(server impl) (string->symbol impl)))
    (('multi multi) (conf-set! '(server multi) (->bool multi)))
    (('engine engine) (conf-set! '(server engine) (->symbol engine)))
    (('websocket websocket) (conf-set! '(server websocket) (->bool websocket)))
    (('pub pub) (conf-set! '(server pub) (basename pub)))
    (('sendfile v) (conf-set! '(server sendfile) (->bool v)))
    (else (error parse-namespace-server "Config: Invalid item" item))))

(define (parse-namespace-websocket item)
  (match item
    (('maxpayload maxpayload) (conf-set! '(websocket maxpayload) (->ws-payload maxpayload)))
    (('minpayload minpayload) (conf-set! '(websocket minpayload) (->ws-payload minpayload)))
    (('fragment fragment) (conf-set! '(websocket fragment) (->ws-payload fragment)))
    (('maxsize maxsize) (conf-set! '(websocket maxsize) (->integer maxsize)))
    (('timeout timeout) (conf-set! '(websocket timeout) (->integer timeout)))
    (else (error parse-namespace-websocket "Config: Invalid item" item))))

(define (parse-namespace-host item)
  (match item
    (('name name) (conf-set! '(host name) name))
    (('family family) (conf-set! '(host family) (->symbol family)))
    (('addr addr) (conf-set! '(host addr) addr))
    (('port port) (conf-set! '(host port) (->integer port)))
    (else (error parse-namespace-host "Config: Invalid item" item))))

(define (parse-namespace-backend backend)
  (cond
   ((irregex-search "([^@]+)@([^@]*):([0-9]+)" backend)
    => (lambda (m)
         (list (string->symbol (irregex-match-substring m 1))
               (irregex-match-substring m 2)
               (string->number (irregex-match-substring m 3)))))
   (else
    (if (string? backend)
        (string->symbol backend)
        (error parse-namespace-backend
               (format #f "Parse session backend (~a) failed!~%" backend))))))

(define (parse-namespace-session item)
  (match item
    (('path path) (conf-set! '(session path) path))
    (('backend backend) (conf-set! '(session backend) (parse-namespace-backend backend)))
    (else (error parse-namespace-session "Config: Invalid item" item))))

(define (parse-namespace-upload item)
  (match item
    (('types types) (conf-set! '(upload types) (->list types)))
    (('path path) (conf-set! '(upload path) path))
    (('size size) (conf-set! '(upload size) (->integer size)))
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
    (('enable enable) (conf-set! 'debug-mode (->bool enable)))
    (('monitor monitor) (conf-set! '(debug monitor) (->list monitor)))
    (else (error parse-namespace-cache "Config: Invalid item" item))))

(define (parse-config-item item)
  (match item
    (('db rest ...) (parse-namespace-db rest))
    (('server rest ...) (parse-namespace-server rest))
    (('websocket rest ...) (parse-namespace-websocket rest))
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
  (define proto (get-conf '(db proto)))
  (define addr (get-conf '(db addr)))
  (define sock (and=> (get-conf '(db socketfile)) (lambda (x) (not (eq? x 'disable)))))
  (define dbname (get-conf '(db name)))
  (define username (get-conf '(db username)))
  (define passwd (get-conf '(db passwd)))

  (cond
   ((or (and (eq? proto 'tcp) sock) ; tcp with socketfile is impossible
        (and (eq? proto 'socketfile) (not sock))) ; socketfile without socketfile
    (error init-inner-database-item
           (format #f "Conf: either addr:port or sock mode! addr=~a, sock=~a" addr sock)))
   ((eq? dbd 'sqlite3) ; sqlite3 needs only dbname
    (conf-set! 'database `(sqlite3 ,username ,passwd ,dbname)))
   ((eq? proto 'tcp) ; addr:port mode
    (conf-set! 'database `(,dbd ,username ,passwd ,dbname (tcp ,addr))))
   ((eq? proto 'socketfile) ; socket file mode
    (conf-set! 'database `(,dbd ,username ,passwd ,dbname (socketfile ,sock))))
   (else (error init-inner-database-item "Fatal: Invalid database config"))))

(define (init-inner-config-items)
  (and (get-conf '(db enable)) (init-inner-database-item)))

(define (init-database-config dbd user passwd db-name db-addr db-proto)
  (define (default-addr)
    (case dbd
      ((mysql) "localhost:3306")
      ((postgresql) "localhost:5432")
      ((sqlite3) #f)
      (else (error init-database-config "GNU Artanis doesn't support this DBD: " dbd))))
  ;; if dbd is not specified, it's mysql in default.
  (conf-set! '(db dbd) (or dbd (get-conf '(db dbd)) 'mysql))
  ;; if username is not specified, it's root in default.
  (conf-set! '(db username) (or user (get-conf '(db username))"root"))
  ;; if passwd is not specified, it's null in default.
  (conf-set! '(db passwd) (or passwd (get-conf '(db passwd)) ""))
  ;; if db-name is not specified, it's artanis in default.
  (conf-set! '(db name) (or db-name (get-conf '(db name)) "artanis"))
  ;; if db-addr is not specified, it's default address of DB respectivly.
  (conf-set! '(db addr) (or db-addr (get-conf '(db addr)) (default-addr)))
  ;; if db-proto is not specified, it's tcp in default.
  (conf-set! '(db proto) (or db-proto (get-conf '(db proto)) 'tcp))
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
  (close fp)
  (display "done.\n" (artanis-current-output)))
