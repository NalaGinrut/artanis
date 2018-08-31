;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014,2015,2016,2017,2018
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

;; -----------------------------------------------------------------------
;; This is where all the global vars should be put.
;; Include global config table, and green-thread working queue in the future.
;; NOTE: This module should NEVER import any other modules in Artanis!!!

(define-module (artanis env)
  #:use-module (artanis version)
  #:use-module (ice-9 regex)
  #:re-export (artanis-version)
  #:export (*handlers-table*
            *conf-hash-table*
            *conn-pool*
            *before-response-hook*
            *after-request-hook*
            *before-run-hook*
            *after-websocket-handshake-hook*
            *DB-conn-init-hook*
            *sql-mapping-lookup-table*
            *artanis-entry*
            %current-toplevel
            current-toplevel
            find-ENTRY-path
            draw:is-dry-run?
            draw:is-force?
            draw:is-skip?
            draw:is-quiet?
            artanis-current-output
            *controllers-table*
            current-dbconn
            current-appname
            change-session-backend!
            current-session-backend
            protocol-add!
            lookup-protocol
            half-closed?
            %os-kernel
            %os-distro
            %kernel-version
            %system-arch
            kernel-version>=?
            linux-version>=?
            is-init-server-run?
            current-encoder
            *lpc-instance-pool*
            out-of-task-prompt?
            resources-collecting?))

;; WARNING:
;; For concurrency in green-thread, all these stuffs should be immutable
;; when the server-core start to work!!!
;; TODO:
;; set a global variable when server-core started, and each accessor here
;; should check it first.

;; table structure:
;; '((rule-handler-key (handler . keys)) ...)
;; for example:
;; `(("GET \"/photo/:id/edit\"" (,(lambda (req ..) ...) . id)))
(define *handlers-table* (make-hash-table))
(define *conf-hash-table* (make-hash-table))

;; NOTE: The init queue size equals to (server wqlen).
;;       If all the available DB conn were blocked, a new DB conn will be
;;       created, and never closed but just recycled by *conn-pool*.
(define *conn-pool* #f)

(define *before-response-hook* (make-hook 2))
(define *after-request-hook* (make-hook 2))
(define *before-run-hook* (make-hook))
(define *DB-conn-init-hook* (make-hook 1))
(define *after-websocket-handshake-hook* (make-hook 2))

;; TODO: I don't have much time for it but it should be RB-Tree in the future
(define *sql-mapping-lookup-table* (make-hash-table))

(define *artanis-entry* "ENTRY")

(define %current-toplevel (make-parameter #f))
(define* (find-ENTRY-path proc #:optional (check-only? #f))
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
(define (current-toplevel)
  (or (%current-toplevel)
      (find-ENTRY-path identity #t)))

;; parameters for command
(define draw:is-dry-run? (make-parameter #f))
(define draw:is-force? (make-parameter #f))
(define draw:is-skip? (make-parameter #f))
(define draw:is-quiet? (make-parameter #f))

(define *null-device-output-port*
  (open-input-file *null-device*))

(define (artanis-current-output)
  (if (draw:is-quiet?)
      *null-device-output-port*
      (current-output-port)))

(define *controllers-table* (make-hash-table))

(define current-dbconn (make-parameter #f))

(define (current-appname) (and=> (current-toplevel) basename))

(define *session-backend* #f)
(define (change-session-backend! x) (set! *session-backend* x))
(define (current-session-backend) *session-backend*)

(define *proto-table* (make-hash-table))

(define (protocol-add! name proto)
  (hashq-set! *proto-table* name proto))

(define (lookup-protocol name)
  (hashq-ref *proto-table* name))

(define half-closed? (make-parameter #f))

(define (%os-kernel)
  (vector-ref (uname) 0))

(define (%os-distro)
  (vector-ref (uname) 1))

(define (%kernel-version)
  (vector-ref (uname) 2))

(define (%system-arch)
  (vector-ref (uname) 4))

;; Simple version strings comparator.
;; It is not performance-friendly, but since it is to be used once
;;  -- not so critical.
;; It can compare complex version strings like
;;  linux-4.0.74-special-build-1a and linux-4.0.74-special-build-1b
;; And, of course, can compare the following:
;; 3.9 and 3.10
;; or 4.5 and 4.11
(define (version>=? s1 s2)
  (let ((sm1 (string-match "[0-9]+" s1))
        (sm2 (string-match "[0-9]+" s2)))
    ;; if one of the strings doesn't contain numeric values
    ;;  compare as usual strings
    (if (or (not sm1) (not sm2)) (string>=? s1 s2)
        ;; otherwise -- compare prefixes, then numeric parts,
        ;; then, recursively, suffixes.
        (let ((mp1 (match:prefix sm1))
              (mp2 (match:prefix sm2))
              (ms1 (match:substring sm1))
              (ms2 (match:substring sm2)))
          (cond
           ((string>? mp1 mp2) #t)
           ((string<? mp1 mp2) #f)
           ((> (string->number ms1) (string->number ms2)) #t)
           ((< (string->number ms1) (string->number ms2)) #f)
           (else
            (version>=? (match:suffix sm1) (match:suffix sm2))))))))

(define (kernel-version>=? vstr)
  (version>=? (%kernel-version) vstr))

(define (linux-version>=? vstr)
  (and (string=? (%os-kernel) "Linux")
       (kernel-version>=? vstr)))

;; TODO: check uname here and provide features
;; either 'epoll or 'kqueue to use in cond-expand inside ragnarok
;; to select proper module for event-queue implementation.

(define is-init-server-run? #f)

(define current-encoder (make-parameter identity))

(define *lpc-instance-pool* #f)

(define out-of-task-prompt? (make-parameter #f))

(define resources-collecting? (make-parameter #f))
