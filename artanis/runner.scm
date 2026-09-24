;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2025-2026
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
;;  Artanis is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  Artanis is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.
;;  If not, see <http://www.gnu.org/licenses/>.

;; ========== Runner ==========
;; A runner executes a thunk in a runner thread, and the Ragnarok task calling
;; call-with-runner is suspended until the thunk is done. It's for blocking
;; operations that can't be handled by non-blocking I/O, NOT for parallel
;; computing.
;;
;; The design borrows from Guile's (ice-9 futures) (LGPLv3+, by Ludovic Courtès):
;; a global queue of closures consumed by a fixed thread pool, and the result
;; kept as a thunk, so that multiple values and exceptions are handled in the
;; same way. The difference is how the waiter is notified: the waiter is a
;; suspended Ragnarok task rather than a thread, so the worker notifies it with
;; wake-up-task! (see server-context.scm).
;;
;; NOTE: We don't use (ice-9 futures) because:
;;       1. Guile's (ice-9 futures) sizes its pool as (1- cores), so there's no
;;          worker on a single-core platform, and futures run only when
;;          touched. The binding is inlined in the declarative module, so it
;;          can't be monkey-patched from outside. Our pool size comes from
;;          server.runners.
;;       2. There's no hook to notify a suspended Ragnarok task on completion.
;;
;; NOTE: Runner threads run in a clean dynamic state captured when this module
;;       is loaded, so the server's parameters, e.g. current-client and
;;       current-server, keep their default values there. Get whatever you need
;;       before call-with-runner, and don't call server core APIs (break-task,
;;       ws send, session, etc.) in the thunk.

(define-module (artanis runner)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis server server-context)
  #:use-module (artanis server scheduler)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 suspendable-ports)
  #:use-module (srfi srfi-9)
  #:export (call-with-runner))

(define-record-type <runner>
  (make-runner thunk state result mutex client)
  runner?
  (thunk  runner-thunk)
  (state  runner-state  set-runner-state!)  ; pending | done
  (result runner-result set-runner-result!) ; a thunk to return or rethrow
  (mutex  runner-mutex)
  (client runner-client))                   ; client of the waiting task

(define *runner-queue* (new-queue))
(define *runner-mutex* (make-mutex))
(define *runner-available* (make-condition-variable))
(define *runners* '())

;; Inside a runner thread?
(define within-runner? (make-parameter #f))

(define (runner-done? r)
  (with-mutex (runner-mutex r)
              (eq? (runner-state r) 'done)))

(define (execute-runner! r)
  (let ((result (catch #t
                  (lambda ()
                    (call-with-values (runner-thunk r)
                      (lambda results
                        (lambda () (apply values results)))))
                  (lambda args
                    (lambda () (apply throw args))))))
    ;; NOTE: The result must be ready before waking up the task. Otherwise the
    ;;       task may be resumed, find it pending, and be suspended forever.
    (with-mutex (runner-mutex r)
                (set-runner-result! r result)
                (set-runner-state! r 'done))
    (wake-up-task! (runner-client r))))

;; Captured at load time, before the server binds any parameter.
(define *clean-dynamic-state* (current-dynamic-state))

(define (runner-loop)
  ;; Threads inherit the parameters of their creator, which is a Ragnarok task
  ;; since runner threads are created lazily. That would leak the stale
  ;; current-client, current-server, etc. of that task into every runner, so we
  ;; run in the clean dynamic state instead.
  (with-dynamic-state
   *clean-dynamic-state*
   (lambda ()
     ;; There's no task prompt here, so make sure the default blocking waiters
     ;; are used even if the clean state was captured with async ones.
     (parameterize ((current-read-waiter
                     (@@ (ice-9 suspendable-ports) default-read-waiter))
                    (current-write-waiter
                     (@@ (ice-9 suspendable-ports) default-write-waiter))
                    (within-runner? #t))
       (runner-work-loop)))))

(define (runner-work-loop)
  (let lp ()
    (let ((r (with-mutex
              *runner-mutex*
              (let wait ()
                (if (queue-empty? *runner-queue*)
                    (begin
                      (wait-condition-variable *runner-available*
                                               *runner-mutex*)
                      (wait))
                    (queue-out! *runner-queue*))))))
      (execute-runner! r)
      (lp))))

(define (submit-runner! r)
  (with-mutex
   *runner-mutex*
   (when (null? *runners*)
     (let ((n (get-conf '(server runners))))
       (set! *runners*
             (map (lambda (_) (call-with-new-thread runner-loop))
                  (iota n)))))
   (queue-in! *runner-queue* r)
   (signal-condition-variable *runner-available*)))

(define (call-with-runner thunk)
  (cond
   ((or (within-runner?) (not (ragnarok-client? (current-client))))
    ;; Already in a runner thread, or not in a Ragnarok task (so there's no
    ;; task to suspend). Just run it here.
    (thunk))
   (else
    (let* ((client (current-client))
           (r (make-runner thunk 'pending #f (make-mutex) client)))
      ;; NOTE: We tag runners as high prio task, so that the scheduler will
      ;;       return to the client as soon as possible when the runner is
      ;;       done.
      (high-prio-task-add! client)
      (submit-runner! r)
      (let lp ()
        (when (not (runner-done? r))
          (DEBUG "Runner is still running, suspend the task ~a~%" client)
          (break-task)
          (lp)))
      (high-prio-task-remove! client)
      ((runner-result r))))))
