;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2018
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

;; -----------------------------------------
;; Local Persistent Cache
;; -----------------------------------------

(define-module (artanis lpc)
  #:use-module (artanis env)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis route)
  #:use-module (artanis third-party json)
  #:use-module (artanis third-party redis)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (lpc-backend-name
            lpc-backend-destroy!
            lpc-backend-ref
            lpc-backend-set!
            lpc-backend-remove!
            lpc-backend-flush!

            new-lpc
            lpc-read-only?
            lpc-destroy!
            lpc-set!
            lpc-ref
            lpc-remove!
            lpc-flush!

            get-lpc-instance!
            intern-lpc-instance!
            lpc-instance-recycle
            init-lpc))

(define (lpc-prefix key)
  (string-append "__artanis_lpc_" (get-conf '(db name)) "_" key))

(define-record-type lpc-backend-impl
  (fields
   name
   destroy!
   meta
   ref
   set!
   remove!
   flush!))

(define (backend-impl:destroy!/redis backend)
  (redis-close (lpc-backend-impl-meta backend)))

(define (backend-impl:ref/redis backend key)
  (redis-send (lpc-backend-impl-meta backend) (redis-get (lpc-prefix key))))

(define (backend-impl:set!/redis backend key value)
  (redis-send (lpc-backend-impl-meta backend) (redis-set (lpc-prefix key) value)))

(define (backend-impl:remove!/redis backend key)
  (redis-send (lpc-backend-impl-meta backend) (redis-del (list (lpc-prefix key)))))

(define (backend-impl:flush!/redis backend)
  (noop))

(define* (new-lpc-backend/redis #:key (host "127.0.0.1") (port 6379))
  (make-lpc-backend-impl 'redis
                         backend-impl:destroy!/redis
                         (redis-connect #:host host #:port port)
                         backend-impl:ref/redis
                         backend-impl:set!/redis
                         backend-impl:remove!/redis
                         backend-impl:flush!/redis))

(define (backend-impl:destroy!/json backend)
  #t)

(define (backend-impl:ref/json backend key)
  (let ((json (cdr (lpc-backend-impl-meta backend))))
    (hash-ref json key)))

(define (backend-impl:set!/json backend key value)
  (let ((json (cdr (lpc-backend-impl-meta backend))))
    (hash-set! json key value)))

(define (backend-impl:remove!/json backend key)
  (let ((json (cdr (lpc-backend-impl-meta backend))))
    (hash-remove! json key)))

(define (backend-impl:flush!/json backend)
  (let ((meta (lpc-backend-impl-meta backend)))
    (delete-file (car meta))
    (call-with-output-file (car meta)
      (lambda (port)
        (scm->json (cdr meta) port)))))

(define* (new-lpc-backend/json filename)
  (make-lpc-backend-impl 'json
                         backend-impl:destroy!/json
                         (cons filename (call-with-input-file filename json->scm))
                         backend-impl:ref/json
                         backend-impl:set!/json
                         backend-impl:remove!/json
                         backend-impl:flush!/json))

(define (lpc-backend-name backend)
  ((lpc-backend-impl-name backend) backend))

(define (lpc-backend-destroy! backend)
  ((lpc-backend-impl-destroy! backend) backend))

(define (lpc-backend-ref backend key)
  ((lpc-backend-impl-ref backend) backend key))

(define (lpc-backend-set! backend key value)
  ((lpc-backend-impl-set! backend) backend key value))

(define (lpc-backend-remove! backend key)
  ((lpc-backend-impl-remove! backend) backend key))

(define (lpc-backend-flush! backend)
  ((lpc-backend-impl-flush! backend) backend))

(define-record-type lpc
  (fields
   backend
   read-only?))

(define* (new-lpc #:key (backend 'redis) (read-only? #f))
  (lambda args
    (let ((ctor (case backend
                  ((redis) new-lpc-backend/redis)
                  ((json) new-lpc-backend/json))))
      (make-lpc (apply ctor args) read-only?))))

(::define (lpc-destroy! lpc)
  (:anno: (lpc) -> ANY)
  (let ((backend (lpc-backend lpc)))
    (lpc-backend-destroy! backend)))

(::define (lpc-set! lpc key value)
  (:anno: (lpc string ANY) -> ANY)
  (when (lpc-read-only? lpc)
    (throw 'artanis-err 500 lpc-set!
           "LPC is read only. please check your code."))
  (let ((backend (lpc-backend lpc)))
    (lpc-backend-set! backend key value)))

(::define (lpc-ref lpc key)
  (:anno: (lpc string) -> ANY)
  (let ((backend (lpc-backend lpc)))
    (lpc-backend-ref backend key)))

(::define (lpc-remove! lpc key)
  (:anno: (lpc string) -> ANY)
  (when (lpc-read-only? lpc)
    (throw 'artanis-err 500 lpc-remove!
           "LPC is read only!!! please check your code!!!"))
  (let ((backend (lpc-backend lpc)))
    (lpc-backend-remove! backend key)))

(::define (lpc-flush! lpc)
  (:anno: (lpc) -> ANY)
  (let ((backend (lpc-backend lpc)))
    (lpc-backend-flush! backend)))

(::define (get-lpc-instance!)
  (:anno: () -> lpc)
  (when (not *lpc-instance-pool*)
    (error "LPC is not enabled. Please check out your config!"))
  (and (not (queue-empty? *lpc-instance-pool*))
       (queue-out! *lpc-instance-pool*)))

(::define (intern-lpc-instance! lpc)
  (:anno: (lpc) -> lpc)
  (when (not *lpc-instance-pool*)
    (error "LPC is not enabled. Please check out your config!"))
  (DEBUG "Intern a new lpc instance!~%")
  (queue-in! *lpc-instance-pool* lpc)
  lpc)

;; NOTE: We only keep one instance in the queue as possible,
(define (lpc-instance-recycle lpc)
  (cond
   ((queue-empty? *lpc-instance-pool*)
    (intern-lpc-instance! lpc))
   (else
    (lpc-flush! lpc)
    (lpc-destroy! lpc))))

(define (init-lpc)
  (set! *lpc-instance-pool* (new-queue)))
