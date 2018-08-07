;;; (artanis third-party redis upstream commands define) --- redis module for Guile.

;; Copyright (C) 2013-2017 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of guile-redis.
;;
;; guile-redis is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; guile-redis is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with guile-redis; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; Redis module for Guile

;; The following is a convenient Emacs interactive function to create
;; new Redis command definitions. Uncomment it temporarily, go to the
;; end of the function and M-x eval-defun.

;; Once the function is loaded in Emacs, M-x redis-add-command.

;; (defun redis-add-command ()
;;   (interactive)
;;   (let ((command (read-string "Command: "))
;;         (args (read-string "Arguments: "))
;;         (opt-args (read-string "Optional arguments: "))
;;         (rest-args (read-string "Rest arguments: ")))
;;     (if (> (length opt-args) 0)
;;         (insert "(define* (" command)
;;         (insert "(define (" command))
;;     (if (> (length args) 0) (insert " " args))
;;     (if (> (length opt-args) 0) (insert " #:optional " opt-args))
;;     (if (> (length rest-args) 0) (insert " #:rest " rest-args))
;;     (insert ")\n")
;;     (insert "  (")
;;     (if (> (length rest-args) 0) (insert "apply "))
;;     (insert "make-command \"" (upcase command) "\"")
;;     (if (> (length args) 0) (insert " " args))
;;     (if (> (length opt-args) 0) (insert " " opt-args))
;;     (if (> (length rest-args) 0) (insert " " rest-args))
;;     (insert "))\n")))

;;; Code:

(define-module (artanis third-party redis upstream commands define)
  #:use-module (artanis third-party redis upstream utils)
  #:use-module (srfi srfi-9)
  #:export (make-command
            redis-command?
            redis-cmd-name
            redis-cmd-params
            redis-cmd-reply
            cons-list->list))

(define-record-type <redis-command>
  (create-command name params reply)
  redis-command?
  (name redis-cmd-name)
  (params redis-cmd-params)
  (reply redis-cmd-reply))

(define* (make-command name #:key (proc read-reply) #:rest args)
  (create-command name args proc))

(define (cons-list->list pairs)
  (cond
   ((null? pairs) '())
   (else (cons (car (car pairs))
               (cons (cdr (car pairs))
                     (cons-list->list (cdr pairs)))))))

;;; (artanis third-party redis upstream commands define) ends here
