;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2016,2017,2018,2019,2021,2022
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
            current-myhost
            default-conf-values))

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
    ((db enable)
     #f
     "Whether to use a database, if disabled, the database won't be initialized
in the beginning, which saves memory and boot time.
Some users may want to use GNU Artanis without configuring any databases, if
that's your case please set it to `false' to avoid unintended errors.
db.enable = <boolean>")

    ((db dbd)
     mysql
     "What database server should be used, depends on the database installed
on your machine.
NOTE: If you use MariaDB then you should set it to mysql.
db.dbd = mysql | postgresql | sqlite3")

    ((db proto)
     tcp
     "The protocol for connecting the databse. If you use tcp, a socket port
must be specified in the address. And if you choose socketfile, then you should
specify the unix socket file managed by database server.
db.proto = tcp | socketfile")

    ((db addr)
     "127.0.0.1:3306"
     "The address of the database server.
For example, MariaDB by default uses localhost:3306.
db.addr = <string>")

    ((db socketfile)
     #f
     "If you configured the database server to connect to a unix socket file,
then you should fill this field with the file name.
db.socketfile = <string>")

    ((db username)
     "root"
     "User name to connect to the database server.
db.username = <string>")

    ((db passwd)
     ""
     "Password of the user to connect to the database server.
db.passwd = <string>")

    ((db name)
     ,(or (current-appname) "artanis")
     "The database name.
db.name = <string>")

    ((db engine)
     InnoDB
     "The engine of the database server.
NOTE: for sqlite3, you must leave this field blank, as `db.engine = '
If you remove this item, the default value InnoDB will be used.
db.engine = <string>")

    ((db poolsize)
     64
     "The size of DB connection pool. If the specified size is less then required,
then the task will be scheduled and sleep till there's available DB connection.
db.poolsize = <integer>")

    ((db pool)
     increase
     "The management mode of DB connection pool:
`increase' for increasing the pool size if any necessary, however it won't reduce back.
`fixed' will not increase the pool size, if it lacks of DB connections, then it will
be scheduled.
db.pool = increase | fixed")

    ((db encodeparams)
     #f
     "Whether to encode params automatically.
(params rc \"your_key\") will be encoded by uri-encode.
The username and passwd in :auth will be encoded by uri-encode.
NOTE: It's your duty to call uri-decode to get proper value.
NOTE: If you enable db.encodeparams then it's better to decode the related value
twice in the client-side, since some requests may be sent from browsers, and
they're already encoded.
db.encodeparams = <boolean>")

    ((db lpc)
     #f
     "Enable LPC (Lightweight Persistent Cache), this may require Redis.
db.lpc = <boolean>")

    ;; for server namespace
    ((server info)
     ,artanis-version
     "Specify your own server info. It'll be Artanis-x.x.x by default, adding
the artanis version number.
server.info = <string>")

    ((server nginx)
     #f
     "If you want to use Nginx for reversed-proxy, please enable it.
server.nginx = enable | disable")

    ((server charset)
     "utf-8"
     "Charset in server side. utf-8 in default.
NOTE: Don't change it unless you know what you're doing!
server.charset = <string>")

    ;; FIXME: use local pages
    ((server syspage path)
     "/etc/artanis/pages"
     "The path of status page. You may customize your own status pages.
If you're using application folder from `art create', then you may add your
customized status pages in sys/pages, for example, if you create 404.html in
sys/pages, then it will overload the original 404 page.
server.syspage.path = <string>")

    ((server backlog)
     128
     "Backlog of the socket.
NOTE: Don't change it unless you really know what you're doing!
server.backlog = <integer>")

    ((server wqlen)
     64
     "The length of the work queue in Artanis server.
server.wqlen = <integer>")

    ((server trigger)
     edge
     "The trigger mode of epoll. Please read epoll man page to know more.
server.trigger = edge | level")

    ((server engine)
     ragnarok
     "The server core which is used for holding high concurrent connections.
Artanis has a strong server core named Ragnarok, which is based on
delimited-continuations to provide asynchronous non-blocking high concurrent serving.
You may choose guile inner server which is weak, but useful when you are running
Artanis on an operating system lacking key features to run Raganrok. For example,
in GNU/Hurd, which has no epoll.
You may choose fibers server implemented with threads and delimited continuations,
which is preemptable by the timer you set.
For more details please see https://github.com/wingo/fibers/wiki/Manual.
server.engine = ragnarok | guile | fibers | <customized engine>")

    ((server timeout)
     60
     "Timeout for any connection to Artanis (in seconds).
0 for always short live connections.
server.timeout = <integer>")

    ((server polltimeout)
     500
     "The the timeout for each event polling round, in miliseconds.
server.polltimeout = <integer>")

    ((server bufsize)
     12288
     "The buffer size (in bytes) of the connecting socket.
In Ragnarok server core, the request handling will be scheduled when the socket
buffer is full. This item affects the performance of socket I/O largely.
Usually, if you're handling a large amount of small requests, it's better to set
the buffer size to a small value.
But if you're providing some kind of downloading or uploading service, it's better
to set it to a larger one.
A large buffer size will increase the latency of unserved requests.
Please read the `Ragnarok server core' chapter to learn the design principle,
if you need to do some tweaking.
server.bufsize = <integer>")

    ;; NOTE: Only for Linux-3.9+
    ;;       One kernel features is necessary:
    ;;       SO_REUSEPORT (since 3.9)
    ;;       Allows mutiple servers to listen to the same socket port, say 8080.
    ((server multi)
     #f
     "This is the most significant feature of Ragnarok server core.
Please remember that there're no threads in GNU Artanis.
All the tasks are based on delimited continuations, this kind of design is the
so-called Green Threads, or modern terminology, Co-routines.
GNU/Linux has introduced a feature named SO_REUSEPORT since 3.9.
This feature let us start multiple Artanis instances listening to the same socket
port. When requests come, the Linux kernel will do the necessary lock and allocation
work for us to dispatch requests to these Artanis instances.
server.multi = <boolean>")

    ((server websocket)
     #t
     "Enable WebSocket.
server.websocket = <boolean>")

    ((server pub)
     "pub"
     "The path to public directory, this is useful for public static resources,
for exaample, css/img/js, etc.
server.pub = <string>")

    ((server sendfile)
     #f
     "Whether to use Linux specified sendfile interface.
server.sendfile = <boolean>")

    ((server mmapped)
     #f
     "Whether to use POSIX specific mmap for file I/O.
server.mmapped = <boolean>")

    ((server allowedmethods)
     (HEAD GET POST PUT)
     "The allowed HTTP methods.
server.allowedmethods = <methods-list>")

    ((server jsmanifest)
     "pub"
     "The path to find \"manifest.json\".
server.jsmanifest = <string>")

    ((websocket maxpayload)
     ,(1- (ash 1 64))
     "The maximum payload size of WebSocket request in bytes. If it exceeds, then it
will be segemented.
websocket.maxpayload = <integer>")

    ((websocket minpayload)
     1
     "The minimum payload size of WebSocket request in bytes.
Enlarge it to avoid slow 1-byte attack (only for fragment).
websocket.minpayload = <integer>")

    ((websocket fragment)
     4096
     "If fragment >= 0, then it's the size of the websocket frame fragment.
If fragment = 0, then the websocket frame will not be fragmented.
websocket.fragment = <integer>")

    ((websocket maxsize)
     ,(ash 1 10)
     "Maximum upload size in bytes from WebSocket request, the exceeded request
will be fobidden.
websocket.maxsize = <integer>")

    ((websocket timeout)
     64
     "Timeout of WebSocket request, in seconds.
websocket.timeout = <integer>")

    ;; for host namespace
    ((host name)
     #f
     "If disabled, you will have to use the IP address to connect instead of the
hostname. e.g. `host.name = lambdachip.com'.
host.name = <string> | disable")

    ((host addr)
     "127.0.0.1"
     "The URL/IP of your hosting site.
host.addr = <URL> | <IP>")

    ((host port)
     3000
     "The listening port of your hosting site.
host.port = <integer>")

    ((host family)
     ipv4
     "Specify the protocol family.
host.family = ipv4 | ipv6")

    ;; for session namespace
    ((session path)
     "session"
     "Specify the session files path. Change according to your session engine.
session.path = <PATH>")

    ((session hijackcheck)
     "false"
     "Check the valid session, if it appears in multiple client IP, then fail. This may
cause problems when your hosting is behind CDN, you may need to disable it if so.
session.hijackcheck = <boolean>")

    ((session backend)
     simple
     "Specify session engine. Here're supported backends:
simple: uses hash table for memcache.
db: uses RDBMS for storing sessions.
file: stores session information into text files.
redis: uses Redis for managing sessions.
session.engine = simple | db | file | redis | redis@addr:port | <third-party-engine>")

    ((upload types)
     (jpg png gif)
     "Specify allowed upload file type, say, upload.types = jpg,png,gif.
upload.types = <item-list>")

    ((upload path)
     "upload"
     "The path to put the uploaded files.
upload.path = <PATH>")

    ((upload size)
     5242880  ; 5MB
     "The size limitation of uploaded file in bytes.
upload.size = <interger>")

    ;; for mail namespace
    ((mail sender) "/usr/bin/msmtp"
     "The command called by Sendmail module. It's strongly recommended to use `msmtp' to replace `sendmail'.
mail.sender = <string>")

    ((cache maxage)
     3600
     "The maximum age of a cached page in seconds.
This is the global maxage of any cache.
If you want to specify maxage for certain page, please read the manual about the Cache.
cache.maxage = <integer>")

    ((debug enable)
     #f
     "Wheather to enable debug mode.
If you enable debug mode, Artanis will print debug information verbosely.
The module you modified will be reloaded instantly, and the page view will be rendered as well.
NOTE: This option will affect the performance, please use it for debug purposes only./
debug.enable = <boolean>")

    ((debug monitor)
     ()
     "The paths that needs to be monitored in debug-mode.
This will take advantage of `inotify' in GNU/Linux kernel.
NOTE: We may support GNU/Hurd as well, with its file monitor mechanism, in the future.
debug.monitor = <PATHs>")
    ((cookie expires)
     3600
     "Cookie expiration time in seconds.
       1 hour is 3600
       6 hours 21600
       1 month 2592000
 cookie.expires = <integer>")))

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

(define-syntax-rule (->string-list x)
  (map (lambda (e)
         (string-trim-both e))
       (filter (lambda (s) (not (string-null? s))) (string-split x #\,))))

(define-syntax-rule (->none/str x)
  (case (string->symbol (string-downcase x))
    ((none null #{}#) "")
    (else x)))

(define-syntax-rule (->none/boolean x)
  (case (string->symbol (string-downcase x))
    ((false no none null disable) #f)
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

(define-syntax-rule (->methods ml)
  (map (lambda (m)
         (string->symbol (string-upcase m)))
       (->string-list ml)))

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
    (('mmapped v) (conf-set! '(server mmapped) (->bool v)))
    (('allowedmethods ml) (conf-set! '(server allowedmethods) (->methods ml)))
    (('jsmanifest jm) (conf-set! '(server jsmanifest) (->none/str jm)))
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
    (('name name) (conf-set! '(host name) (->none/boolean name)))
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
    (('hijackcheck check) (conf-set! '(session hijackcheck) (->bool check)))
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
    (else (error parse-namespace-cache "Config: Invalid item" item))))

(define (parse-namespace-debug item)
  (match item
    (('enable enable) (conf-set! 'debug-mode (->bool enable)))
    (('monitor monitor) (conf-set! '(debug monitor) (->list monitor)))
    (else (error parse-namespace-debug "Config: Invalid item" item))))

(define (parse-namespace-cookie item)
  (match item
    (('expires expires) (conf-set! '(cookie expires) (->integer expires)))
    (else (error parse-namespace-cookie "Config: Invalid item" item))))

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
    (('cookie rest ...) (parse-namespace-cookie rest))
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
  (define sock (get-conf '(db socketfile)))
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
(define* (current-myhost #:key (for-header? #f))
  (let* ((host (get-conf '(host name)))
         (real-host (if host host (get-conf '(host addr))))
         (port (get-conf '(host port))))
    (if for-header?
        (cons real-host #f)
        (format #f "http://~a:~a" real-host port))))

(define (init-config)
  (define conf-file (current-conf-file)) ; user specified config
  (define fp (open-input-file
              (cond
               ((and conf-file (file-exists? conf-file))
                (format (artanis-current-output) "Loading ~a..." conf-file)
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
