;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2016-2024
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

(define-module (artanis server scheduler)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis config)
  #:use-module (artanis server server-context)
  #:use-module (ice-9 match)
  #:export (ragnarok-scheduler
            schedule-with-command
            break-task
            close-task
            simply-quit
            schedule-if-locked
            close-current-task!))

;; NOTE: We must pass parameters here, say, current-proto, etc.
;;       Because the abort handler (here, the scheduler) will not capture
;;       the parameters bound in prompt thunk.
(define (schedule-with-command cmd)
  (abort-to-prompt
   'serve-one-request
   (current-proto)
   (current-server)
   (current-client)
   cmd))

(define (break-task)
  (schedule-with-command 'save))

(define (close-task)
  (schedule-with-command 'close))

(define (simply-quit)
  (schedule-with-command 'simply-quit))

(define-syntax-rule (schedule-if-locked mutex body ...)
  (let lp ()
    (cond
     ((mutex-locked? mutex)
      (break-task)
      (lp))
     (else
      (lock-mutex mutex)
      body ...
      (unlock-mutex mutex)))))

(define (compute-prio proto client server)
  ;; TODO: how to compute priority
  #t)

;; NOTE: When we fetch the current task from work-table, we'll remove it
;;       from work-table. We don't remove the head when we close it.
(define (save-current-task! k proto client server)
  (let* ((wt (current-work-table server))
         (task (current-task)))
    (task-kont-set! task k)
    (update-task-time! task) ; we update task timestamp each time it's blocked
    (task-prio-set! task (compute-prio proto client server))
    (DEBUG "Save current task ~a!~%" client)
    (add-a-task-to-work-table! wt client task)
    (DEBUG "Save ok~%")))

;; clean task from work-table
(define* (close-current-task! server client #:optional (peer-shutdown? #f))
  ;; NOTE: current task is the head of work-table
  (DEBUG "close task ~a~%" (client-sockport client))
  (let ((task (current-task)))
    (DEBUG "task-keepalive?: ~a~%" (task-keepalive? task))
    (task-keepalive?-set! task #f)
    (when (not (eq? client 'init-now-client))
      (remove-from-work-table! (current-work-table server)
                               client peer-shutdown?))))

(define customized-scheduler #f)

(define-syntax-rule (register-new-scheduler! body ...)
  (set! customized-scheduler
        (lambda (cmd)
          (match cmd
            body ...))))

;; NOTE: We don't call prompt in this scheduler again, since we will get
;;       new task from outside. That means the scheduler doesn't bump new
;;       task for the next, but leave this work to the main-loop.
;; NOTE: We must pass proto/server/client as arguments, since current-proto is
;;       parameter, it can't be captured by abort handler (say,
;;       ragnarok-scheduler). So we must pass them in with abort-to-prompt. Or
;;       we will lose the correct bound parameter value.
(define (ragnarok-scheduler k proto server client cmd)
  (DEBUG "Enter ragnarok scheduler!~%")
  (match cmd
    ('save
     (save-current-task! k proto client server))
    ('close
     ;; NOTE: This will close task by removing task from the work-table.
     ;;       The related socket port should be closed before here.
     (close-current-task! server client #t))
    ('simply-quit #t)
    (else
     (if customized-scheduler
         (customized-scheduler cmd)
         (throw 'artanis-err ragnarok-scheduler "Invalid command ~a" cmd)))))
