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
  #:use-module (artanis config)
  #:use-module (srfi srfi-9)
  #:use-module (web request)
  #:export (session-set! session-ref session-spawn session-destory 
            session-restore get-session))

;; TODO: now we don't have swap algorithm yet, which means all the sessions
;;       are memcached.
;; memcached session
(define *sessions-table* (make-hash-table))

(define (mem:get-session sid)
  (hash-ref *sessions-table* sid))

;; FIXME: lock needed?
(define (mem:store-session! sid session)
  (hash-set! *sessions-table* sid session))

(define (mem:delete-session! sid)
  (hash-remove! *sessions-table* sid))
;; --- end memcached session

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

(define (get-new-sid)
  (get-random-from-dev))
    
(define (get-session sid)
  (or (mem:get-session sid)
      (load-session-from-file sid)))

(define (session-expired? session)
  (let ((expir (session-ref session "expires")))
    (and expir (time-expired? expir))))

(define (session-destory sid)
  (mem:delete-session! sid) ; delete from memcached if exists
  (delete-session-file sid))

(define (session-restore sid)
  (let ((session (get-session sid)))
    (and session (if (session-expired? session) ; expired then return #f
                     (begin (session-destory sid) #f)
                     session)))) ; non-expired, return session
;; no session will return #f
    
(define* (new-session rc #:key (expires 3600) (domain (current-myhost)) (secure #f))
  (let ((expires-str (make-expires expires))
        (path (rc-path rc)))
    (make-session `(("expires" . ,expires-str)
                    ("domain"  . ,domain)
                    ("secure"  . ,secure)
                    ("path"    . ,path)))))

(define (store-session sid session)
  (mem:store-session! sid session)
  (save-session-to-file sid session)
  session)

;; NOTE: memcache-it is a proc received two arguments, which could be used to pass
;;       your custom memcache handler, like memcached/redis.
;;       The default policy is all-in-memory which is an obvious naive way for that.
(define* (session-spawn rc #:optional (memcache-it mem:store-session!))
  (let* ((sid (get-new-sid))
         (session (or (session-restore sid)
                      (store-session sid (new-session rc)))))
    (memcache-it sid session)
    (values sid session)))

(define (session->alist session)
  (hash-map->list list session))

;; return filename if it exists, or #f
(define (get-session-file sid)
  (let ((f (format #f "~a/~a.session" *session-path* sid)))
    (and (file-exists? f) f)))

(define (load-session-from-file sid)
  (let ((f (get-session-file sid)))
    (and f ; if cookie file exists
         (call-with-input-file sid
           (lambda (port)
             (make-session (read port)))))))

(define (save-session-to-file sid session)
  (let ((s (session->alist session))
        (f (get-session-file sid)))
    ;; if file exists, it'll be removed then create a new one
    (when f 
      (delete-file f) 
      (call-with-output-file f
        (lambda (port)
          (write s f))))))

(define (delete-session-file sid)
  (let ((f (get-session-file sid)))
    (and f (delete-file f))))
