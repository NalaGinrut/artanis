;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013
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

(define-module (artanis session)
  #:use-module (artanis utils)
  #:use-module (artanis artanis)
  #:use-module (srfi srfi-9)
  #:use-module (web request)
  #:export (session-set! session-ref session-spawn session-destory session-restore has-auth?))

(define *sessions-table* (make-hash-table))

(define (get-session id)
  (hash-ref *sessions-table* id))

(define (store-session id session)
  (hash-set! *sessions-table* id session))

(define (delete-session id)
  (hash-remove! *sessions-table* id))

(define (session-set! session key val)
  (hash-set! session key val))

(define (session-ref session key)
  (hash-ref session key))

(define (make-session . args)
  (let ((ht (make-hash-table)))
    (for-each (lambda (e)
                (hash-set! ht (car e) (cdr e)))
              args)
    ht))

(define (get-new-id)
  (let ((now (object->string (current-time)))
        (pid (object->string (getpid)))
        (rand (object->string (unsafe-random)))
        (me "nalaginrut"))
    (string->md5 (string-append now pid rand me))))
    
(define (session-expired? session)
  (let ((now (current-time))
        (expires (session-ref session "expires")))
    (> now expires)))

(define (session-destory sid)
  (delete-session sid))

(define (session-restore sid)
  (let ((session (get-session sid)))
    (cond
     ((or (not session) (session-expired? session))
      (session-destory sid)
      #f) ; expired then return #f
     (else session))))
    
(define (new-session rc)
  (let ((sid (get-new-id))
        (expires (params rc "session_expires"))
        (domain (params rc "sessioin_domain"))
        (secure (params rc "session_secure"))
        (path (rc-path rc)))
    (make-session `(("expires" . ,expires)
                    ("domain"  . ,domain)
                    ("secure"  . ,secure)
                    ("path"    . ,path)))))

(define (session-spawn rc)
  (let* ((sid (params rc "sid"))
         (session (or (and sid (session-restore sid)) (new-session rc))))
    (values sid 
            (store-session sid session))))

(define* (has-auth? rc #:key (uid_key "session_cur_uid") (key "sid"))
  (let* ((sid (params rc key))
         (session (get-session sid)))
    (and session (session-ref session uid_key))))
