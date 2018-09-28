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

(define-module (artanis session)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis route)
  #:use-module (artanis db)
  #:use-module (artanis fprm)
  #:use-module (artanis config)
  #:use-module ((rnrs) #:select (define-record-type))
  #:use-module (web request)
  #:use-module (ice-9 format)
  #:export (session-set!
            session-ref
            session-expired?
            session-spawn
            session-destory!
            session-restore
            session-from-correct-client?
            add-new-session-backend
            session-init

            session-backend?
            make-session-backend
            session-backend-name
            session-backend-init
            session-backend-store!
            session-backend-destory!
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

;; TODO: Support session-engine:
;; session.engine = redis or memcached, for taking advantage of k-v-DB.

;; TODO: session key-values should be flushed into set-cookie in rc, and should be encoded
;;       with base64.

(define-record-type session-backend
  (fields
   name ; symbol
   meta ; anything necessary for a specific backend
   init ; -> session-backend
   store! ; -> session-backend -> string -> hash-table
   destory! ; -> session-backend -> string
   restore ; -> session-backend -> string
   set! ; -> session-backend -> string -> string -> object
   ref)) ; -> session-backend -> string -> string

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
                 (valid integer)))) ; 1 for valid, 0 for expired
    (mt 'create 'Sessions defs #:if-exists? 'ignore #:primary-keys '(sid))
    (DEBUG "Init session DB backend is done!~%")))

(define (backend:session-store/db sb sid ss)
  (let ((mt (map-table-from-DB (session-backend-meta sb)))
        (expires (hash-ref ss "expires"))
        (client (hash-ref ss "client"))
        (data (object->string (hash-ref ss "data")))
        (valid "1"))
    (mt 'set 'Sessions #:sid sid #:expires expires #:client client
        #:data data #:valid valid)))

(define (backend:session-destory/db sb sid)
  (let ((mt (map-table-from-DB (session-backend-meta sb))))
    (mt 'set 'Sessions #:valid "0")))

(define (backend:session-restore/db sb sid)
  (let* ((mt (map-table-from-DB (session-backend-meta sb)))
         (cnd (where #:sid sid #:valid "1"))
         (valid (mt 'get 'Sessions #:condition cnd #:ret 'top)))
    (DEBUG "[backend:session-restore/db] ~a~%" valid)
    (and (not (null? valid)) (apply make-session valid))))

(define (backend:session-set/db sb sid k v)
  (define-syntax-rule (-> x) (and x (call-with-input-string x read)))
  (let* ((mt (map-table-from-DB (session-backend-meta sb)))
         (cnd (where #:sid sid #:valid "1"))
         (data (-> (mt 'ref 'Sessions #:columns '(data) #:condition cnd))))
    (and data
         (mt 'set 'Sessions
             #:data (object->string (assoc-set! data k v))
             #:condition cnd))))

(define (backend:session-ref/db sb sid k)
  (define-syntax-rule (-> x) (and x (call-with-input-string x read)))
  (let* ((mt (map-table-from-DB (session-backend-meta sb)))
         (cnd (where #:sid sid #:valid "1"))
         (data (-> (mt 'ref 'Sessions #:columns '(data) #:condition cnd))))
    (and data (assoc-ref data k))))

(define (new-session-backend/db)
  (make-session-backend 'db
                        (current-connection)
                        backend:session-init/db
                        backend:session-store/db
                        backend:session-destory/db
                        backend:session-restore/db
                        backend:session-set/db
                        backend:session-ref/db))

;; session.engine = simple, for managing sessions with simple memory caching.
(define (backend:session-init/simple sb)
  (DEBUG "Initilizing session backend `~:@(~a~)'...~%" 'simple))

(define (backend:session-store/simple sb sid ss)
  (hash-set! (session-backend-meta sb) sid ss))

;; FIXME: lock needed?
(define (backend:session-destory/simple sb sid)
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
           (format #f "Session id (~a) doesn't hit anything!~%" sid)))))

(define (backend:session-ref/simple sb sid k)
  (cond
   ((backend:session-restore/simple sb sid)
    => (lambda (ss) (hash-ref (hash-ref ss "data") k)))
   (else
    (throw 'artanis-err 500 backend:session-ref/simple
           (format #f "Session id (~a) doesn't hit anything!~%" sid)))))

(define (new-session-backend/simple)
  (make-session-backend 'simple
                        (make-hash-table) ; here, meta is session table
                        backend:session-init/simple
                        backend:session-store/simple
                        backend:session-destory/simple
                        backend:session-restore/simple
                        backend:session-set/simple
                        backend:session-ref/simple))

(define (load-session-from-file sid)
  (let ((f (pk "sfile"(get-session-file sid))))
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
             (format #f "Session path `~a' conflict with an existed file!~%"
                     path))))))

(define (backend:session-store/file sb sid ss)
  (let ((s (session->alist ss)))
    (DEBUG "[Session] store session `~a' to file~%" sid)
    (save-session-to-file sid s)))

(define (backend:session-destory/file sb sid)
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
    (and=> (pk "sid file"(load-session-from-file sid)) make-session))))

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
                        backend:session-destory/file
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

(define (session-destory! sid)
  ((session-backend-destory! (current-session-backend))
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
          (DEBUG "[Session] sid: ~a is expired, destory!~%" sid)
          (session-destory! sid)
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

(define* (session-spawn rc #:key (data '()) (expires 3600))
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
    (file   . ,new-session-backend/file)))

(define (add-new-session-backend name maker)
  (set! *session-backend-table*
    (assoc-set! *session-backend-table* name maker)))

(define (session-init)
  (cond
   ((assoc-ref *session-backend-table* (get-conf '(session backend)))
    => (lambda (maker)
         (let ((sb (maker)))
           ((session-backend-init sb) sb)
           (change-session-backend! sb))))
   (else (error (format #f "Invalid session backdend: ~:@(~a~)"
                        (get-conf '(session backend)))))))
