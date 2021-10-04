;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2016,2017,2018,2021
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

(define-module (artanis session)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis route)
  #:use-module (artanis db)
  #:use-module (artanis lpc)
  #:use-module (artanis fprm)
  #:use-module (artanis config)
  #:use-module (artanis third-party redis)
  #:use-module ((rnrs) #:select (define-record-type))
  #:use-module (web request)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (session-set!
            session-ref
            session-expired?
            session-spawn
            session-destroy!
            session-restore
            session-from-correct-client?
            add-new-session-backend
            session-init

            session-backend?
            make-session-backend
            session-backend-name
            session-backend-init
            session-backend-store!
            session-backend-destroy!
            session-backend-restore
            session-backend-set!
            session-backend-ref))

(define (make-session args)
  (let ((ht (make-hash-table)))
    (for-each (lambda (e)
                (hash-set! ht (car e) (cdr e)))
              args)
    ht))

;; Session identifiers should be at least 128 bits (16 chars)
;; long to prevent brute-force session guessing attacks.
;; Here, we use 256 bits sid.
(define (get-new-sid)
  (get-random-from-dev #:length 16)) ; NOTE: one hex contains two chars

(define (session->alist session)
  (hash-map->list cons session))

;; FIXME: maybe optional according to conf?
(define (session-from-correct-client? session rc)
  (let* ((ip (remote-info (rc-req rc)))
         (client (hash-ref session "client"))
         (ret (string=? client ip)))
    (when (not ret)
      (format (current-error-port)
              "[Session Hijack!] Valid sid from two different client: ~a - ~a!~%"
              ip client))
    ret))

(define (get-session-file sid)
  (let ((ct (current-toplevel)))
    (if ct
        (format #f "~a/prv/~a/~a.session" (current-toplevel)
                (get-conf '(session path)) sid)
        (format #f "session/~a.session" sid))))

(define (new-session rc data expires)
  (let ((expires-str (make-expires expires))
        (ip (remote-info (rc-req rc))))
    (make-session `(("expires" . ,expires-str)
                    ("client"  . ,ip)
                    ("data"    . ,data))))) ; data is assoc list

;; TODO: session key-values should be flushed into set-cookie in rc, and should be encoded
;;       with base64.

(define-record-type session-backend
  (fields
   name ; symbol
   meta ; anything necessary for a specific backend
   init ; -> session-backend
   store! ; -> session-backend -> string -> hash-table
   destroy! ; -> session-backend -> string
   restore ; -> session-backend -> string
   set! ; -> session-backend -> string -> string -> object
   ref)) ; -> session-backend -> string -> string

;; Support session-engine:
;; session.engine = redis or memcached, for taking advantage of k-v-DB.
(define (backend:session-init/redis sb)
  (DEBUG "Init session redis backend is done!  ~%" sb))

(define (backend:session-store/redis sb sid ss)
  (let ((redis (session-backend-meta sb))
        (s (object->string (session->alist ss))))
    (backend-impl:set!/redis redis sid s)))

(define (backend:session-destroy/redis sb sid)
  (let ((redis (session-backend-meta sb)))
    (if (backend:session-restore/redis sb sid)
        (backend-impl:remove!/redis redis sid))
    (backend-impl:destroy!/redis redis)))

(define (backend:session-restore/redis sb sid)
  (let* ((redis (session-backend-meta sb))
         (line (backend-impl:ref/redis redis sid))
         (ss (and line
                  (and=> (call-with-input-string line read) make-session))))
    ss))

(define (backend:session-set/redis sb sid k v)
  (let ((redis (session-backend-meta sb)))
    (cond
     ((backend:session-restore/redis sb sid)
      => (lambda (ss)
           (hash-set! ss k v)
           (backend-impl:set!/redis
            redis
            sid
            (object->string (session->alist ss)))))
     (else
      (throw 'artanis-err 500 backend:session-restore/redis
             "Session id (~a) doesn't hit anything!~%" sid)))))

(define (backend:session-ref/redis sb sid k)
  (let ((redis (session-backend-meta sb)))
    (cond
     ((backend:session-restore/redis sb sid)
      => (lambda (ss) (assoc-ref (hash-ref ss "data") k)))
     (else
      (throw 'artanis-err 500 backend:session-ref/redis
             "Session id (~a) doesn't hit anything!~%" sid)))))

(define* (new-session-backend/redis #:key (host "127.0.0.1") (port 6379))
  (make-session-backend 'redis
                        (new-lpc-backend/redis #:host host  #:port port)
                        backend:session-init/redis
                        backend:session-store/redis
                        backend:session-destroy/redis
                        backend:session-restore/redis
                        backend:session-set/redis
                        backend:session-ref/redis))

;; session.engine = db, for managing sessions with DB support.
(define (backend:session-init/db sb)
  (DEBUG "Initilizing session backend `~:@(~a~)'...~%" 'db)
  (let* ((mt (map-table-from-DB (session-backend-meta sb)))
         (defs '((sid varchar 32)
                 (data text)
                 ;; NOTE: expires should be string, NOT datetime!
                 ;;       Because datetime is not compatible with
                 ;;       expires format, so we just store it as
                 ;;       string, that's enough.
                 (expires varchar 29)
                 (client varchar 39) ; 39 for IPv6
                 ;; NOTE: Since Boolean type is not supported by all
                 ;;       DBDs, so we choose Integer to make them happy.
                 )))
    (mt 'create 'Sessions defs #:if-exists? 'ignore #:primary-keys '(sid))
    (DEBUG "Init session DB backend is done!~%")))

(define (backend:session-store/db sb sid ss)
  (let ((mt (map-table-from-DB (session-backend-meta sb)))
        (expires (hash-ref ss "expires"))
        (client (hash-ref ss "client"))
        (data (object->string (hash-ref ss "data"))))
    (mt 'set 'Sessions #:sid sid #:expires expires #:client client
        #:data data)))

(define (backend:session-destroy/db sb sid)
  (let ((mt (map-table-from-DB (session-backend-meta sb))))
    (mt 'set 'Sessions)))

(define (backend:session-restore/db sb sid)
  (let* ((mt (map-table-from-DB (session-backend-meta sb)))
         (cnd (where #:sid sid))
         (valid (mt 'get 'Sessions #:condition cnd #:ret 'top)))
    (DEBUG "[backend:session-restore/db] ~a~%" valid)
    (and (not (null? valid)) (apply make-session valid))))

(define (backend:session-set/db sb sid k v)
  (define-syntax-rule (-> x) (and x (call-with-input-string x read)))
  (let* ((mt (map-table-from-DB (session-backend-meta sb)))
         (cnd (where #:sid sid))
         (data (-> (mt 'ref 'Sessions #:columns '(data) #:condition cnd))))
    (and data
         (mt 'set 'Sessions
             #:data (object->string (assoc-set! data k v))
             #:condition cnd))))

(define (backend:session-ref/db sb sid k)
  (define-syntax-rule (-> x) (and x (call-with-input-string x read)))
  (let* ((mt (map-table-from-DB (session-backend-meta sb)))
         (cnd (where #:sid sid))
         (data (-> (mt 'ref 'Sessions #:columns '(data) #:condition cnd))))
    (and data (assoc-ref data k))))

(define (new-session-backend/db)
  (make-session-backend 'db
                        (current-connection)
                        backend:session-init/db
                        backend:session-store/db
                        backend:session-destroy/db
                        backend:session-restore/db
                        backend:session-set/db
                        backend:session-ref/db))

;; session.engine = simple, for managing sessions with simple memory caching.
(define (backend:session-init/simple sb)
  (DEBUG "Initilizing session backend `~:@(~a~)'...~%" 'simple))

(define (backend:session-store/simple sb sid ss)
  (hash-set! (session-backend-meta sb) sid ss))

;; FIXME: lock needed?
(define (backend:session-destroy/simple sb sid)
  (hash-remove! (session-backend-meta sb) sid))

(define (backend:session-restore/simple sb sid)
  (hash-ref (session-backend-meta sb) sid))

;; FIXME: lock needed?
(define (backend:session-set/simple sb sid k v)
  (cond
   ((backend:session-restore/simple sb sid)
    => (lambda (ss) (hash-set! ss k v)))
   (else
    (throw 'artanis-err 500 backend:session-restore/simple
           "Session id (~a) doesn't hit anything!~%" sid))))

(define (backend:session-ref/simple sb sid k)
  (cond
   ((backend:session-restore/simple sb sid)
    => (lambda (ss) (hash-ref (hash-ref ss "data") k)))
   (else
    (throw 'artanis-err 500 backend:session-ref/simple
           "Session id (~a) doesn't hit anything!~%" sid))))

(define (new-session-backend/simple)
  (make-session-backend 'simple
                        (make-hash-table) ; here, meta is session table
                        backend:session-init/simple
                        backend:session-store/simple
                        backend:session-destroy/simple
                        backend:session-restore/simple
                        backend:session-set/simple
                        backend:session-ref/simple))

(define (load-session-from-file sid)
  (let ((f (get-session-file sid)))
    (and (file-exists? f) ; if cookie file exists
         (call-with-input-file f read))))

(define (save-session-to-file sid session)
  (let* ((f (get-session-file sid))
         (fp (open-file f "w"))); if file exists, the contents will be removed.
    (write session fp)
    (close fp)))

;; session.engine = file, for managing sessions with files.
(define (backend:session-init/file sb)
  (DEBUG "Initilizing session backend `~:@(~a~)'...~%" 'file)
  (let ((path (format #f "~a/prv/~a" (current-toplevel)
                      (get-conf '(session path)))))
    (cond
     ((not (file-exists? path))
      (mkdir path)
      (DEBUG "Session path `~a' doesn't exist, created it!~%" path))
     ((file-is-directory? path)
      (DEBUG "Session path `~a' exists, keep it for existing sessions!~%" path))
     (else
      (throw 'artanis-err 500 backend:session-init/file
             "Session path `~a' conflict with an existed file!~%"
             path)))))

(define (backend:session-store/file sb sid ss)
  (let ((s (session->alist ss)))
    (DEBUG "[Session] store session `~a' to file~%" sid)
    (save-session-to-file sid s)))

(define (backend:session-destroy/file sb sid)
  (let ((f (get-session-file sid)))
    (and (file-exists? f)
         (delete-file f))))

(define (backend:session-restore/file sb sid)
  (cond
   ((string-null? sid)
    (DEBUG "[Session] No sid specified!~%")
    #f) ; no sid, just do nothing.
   (else
    (DEBUG "[Session] Try to restore session `~a' from file~%" sid)
    (and=> (load-session-from-file sid) make-session))))

(define (backend:session-set/file sb sid k v)
  (let ((ss (load-session-from-file sid)))
    (cond
     ((not ss)
      (DEBUG "[Session] session `~a' doesn't exist!~%" sid)
      #f)
     (else
      (DEBUG "[Session] set ~a to ~a in session `~a'~%" k v sid)
      (let ((data (assoc-ref ss "data")))
        (save-session-to-file
         sid
         (assoc-set! ss "data" (assoc-set! data k v))))))))

(define (backend:session-ref/file sb sid k)
  (let ((ss (load-session-from-file sid)))
    (cond
     ((not ss)
      (DEBUG "[Session] session `~a' doesn't exist!~%" sid)
      #f)
     (else (assoc-ref (assoc-ref ss "data") k)))))

(define (new-session-backend/file)
  (make-session-backend 'file
                        #f ; here, no meta is needed.
                        backend:session-init/file
                        backend:session-store/file
                        backend:session-destroy/file
                        backend:session-restore/file
                        backend:session-set/file
                        backend:session-ref/file))

(define (session-set! sid k v)
  ((session-backend-set! (current-session-backend))
   (current-session-backend)
   k v))

(define (session-ref sid k)
  ((session-backend-ref (current-session-backend))
   (current-session-backend)
   k))

(define (session-destroy! sid)
  ((session-backend-destroy! (current-session-backend))
   (current-session-backend)
   sid))

(define (session-expired? session)
  (let ((expir (hash-ref session "expires")))
    (and expir (time-expired? expir))))

(define (session-restore sid)
  (let ((session ((session-backend-restore (current-session-backend))
                  (current-session-backend) sid)))
    (if session
        (cond
         ((session-expired? session)
          (DEBUG "[Session] sid: ~a is expired, destroy!~%" sid)
          (session-destroy! sid)
          'expired) ; expired then return #f
         (else
          (DEBUG "[Session] Restored session: ~a~%"
                 (hash-map->list cons session))
          session)) ; non-expired, return session
        'not-found)))
;; if no session then return #f

(define (session-store! sid session)
  ((session-backend-store! (current-session-backend))
   (current-session-backend)
   sid session)
  session)

(define* (session-spawn rc #:key (data '()) (expires (get-conf '(cookie expires))))
  (define (is-valid-session? s)
    (case s
      ((expired not-found) #f)
      (else s)))
  (let* ((sid (get-new-sid))
         (session (or (is-valid-session? (session-restore sid))
                      (session-store! sid (new-session rc data expires)))))
    (DEBUG "Session spawned: sid - ~a, data - ~a~%" sid data)
    (values sid session)))

(define *session-backend-table*
  `((simple . ,new-session-backend/simple)
    (db     . ,new-session-backend/db)
    (file   . ,new-session-backend/file)
    (redis  . ,new-session-backend/redis)))

(define (add-new-session-backend name maker)
  (set! *session-backend-table*
    (assoc-set! *session-backend-table* name maker)))

(define (create-new-session conf)
  (match conf
    (('redis host port)
     (lambda ()
       ((assoc-ref *session-backend-table* 'redis) #:host host #:port port)))
    (else (assoc-ref *session-backend-table* conf))))

(define (session-init)
  (let ((conf (get-conf '(session backend))))
    (cond
     ((create-new-session conf)
      => (lambda (maker)
           (let ((sb (maker)))
             ((session-backend-init sb) sb)
             (change-session-backend! sb))))
     (else (error (format #f "Invalid session backdend: ~:@(~a~)" conf))))))
