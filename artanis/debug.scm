;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015-2025
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
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

(define-module (artanis debug)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis config)
  #:use-module (artanis inotify)
  #:use-module (artanis irregex)
  #:use-module (artanis mvc controller)
  #:use-module (artanis mvc model)
  #:use-module (ice-9 match)
  #:export (init-debug-monitor
            reload-monitored-files))

;; === NOTE!!! ===
;; DEBUG mode provides module-files monitoring for convinient, but it'll
;; be very slow when you enable it (slower than 500ms per request).
;; So please make sure you disable it when you done debug.

;; FIXME: Here we just add MVC modules to monitored list, feel free
;;        to add more if it's necessary.
(define *monitored-files*
  `(,(format #f "~a/app/models" (current-toplevel))
    ,(format #f "~a/app/controllers" (current-toplevel))))

(define debug-file-watcher-loops "[ERR] You're not in debug mode!")

;; FIXME: do we need to monitor deleted files? how?
(define (init-debug-monitor)
  (set! debug-file-watcher-loops
        (map (lambda (p)
               (cons p (make-inotify-watching-loop p)))
             (append *monitored-files* (get-conf '(debug monitor))))))

;; TODO: Here we should get rid of various temp files, include Emacs'.
;;       Feel free to add more if any necessary.
(define *valid-mod-re* (string->irregex "^[^.#$~]+[.]scm$"))
(define (get-all-changed-files p)
  (define (is-valid-module-file? f)
    (irregex-match *valid-mod-re* f))
  (define (get-watcher-iterator)
    (let ((getter (assoc-ref debug-file-watcher-loops p)))
      (if getter
          (getter)
          (throw 'artanis-err 500
                 "BUG: The debug mode is enabled but the watcher didn't init"))))
  (let ((itorator (get-watcher-iterator)))
    (let lp((event (itorator)) (ret '()))
      (match event
        ('inotify-event-itorator-end
         (format (current-error-port) "[DEBUG] ~a monitor traverse end!~%" p)
         ret)
        (($ watch-event wd mask cookie len name)
         (cond
          ((and (is-valid-module-file? name) ; ignore non-module file
                (logand mask IN_CREATE IN_MODIFY IN_DELETE))
           (format (current-error-port)
                   "File `~a/~a' changed, need to reload!~%" p name)
           (lp (itorator) (cons name ret)))
          (else (lp (itorator) ret))))
        (else (throw 'artanis-err 500 get-all-changed-files
                     "[DEBUG] get-all-changed-files: Invalid event returned!"
                     event))))))

(define (reload-rules)
  ((@@ (artanis commands work) register-rules))
  ((@@ (artanis commands work) load-rules)))

(define (reload-monitored-files)
  (define (reload-module-with-path path)
    (define (re-init-work)
      ((@@ (artanis commands work) clean-stuffs))
      (when (string=? path "app/controllers")
        (hash-clear! *controllers-table*))) ; clean all rules
    (define (do-some-magic)
      (match path
        ("app/models" (use-modules (artanis mvc model)))
        ("app/controllers" (use-modules (artanis mvc controller)))
        (else #t)))
    (re-init-work)
    (for-each
     (lambda (f)
       (do-some-magic)
       (load (format #f "~a/~a" path f)))
     (get-all-changed-files path))
    (reload-rules))
  (for-each reload-module-with-path
            (append *monitored-files* (get-conf '(debug monitor)))))
