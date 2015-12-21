;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015
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

(define-module (artanis debug)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis inotify)
  #:use-module (ice-9 match)
  #:export (init-debug-monitor
            reload-monitored-files))

;; FIXME: Here we just add MVC modules to monitored list, feel free
;;        to add more if it's necessary.
(define *monitored-files*
  '("app/models" "app/controllers"))

(define debug-file-watcher-loop "[ERR] You're not in debug mode!")

;; FIXME: do we need to monitor deleted files? how?
(define (init-debug-monitor)
  (set! debug-file-watcher-loop
        (make-inotify-watching-loop *monitored-files*)))

(define (get-all-changed-files)
  (let ((itorator (debug-file-watcher-loop)))
    (let lp((event (itorator)) (ret '()))
      (match event
        ('inotify-event-itorator-end
         (when (get-conf 'debug-mode)
               (format (current-error-port)
                       "[DEBUG] Files monitoring traverse to end!")))
       (($ watch-event _ wd mask cookie len name)
        (if (logand mask IN_CREATE IN_MODIFY IN_DELETE) 
            (lp (itorator) (cons name ret))
            (lp (itorator) ret)))
       (else (throw 'artanis-err 500
                    "[DEBUG] get-all-changed-files: Invalid event returned!"
                    event))))))

(define (reload-monitored-files)
  (define (file->module filename)
    (let ((ll (map string-trim-both (string-split filename #\/))))
      (resolve-module ll 'autoload))) ; enable autoload
  (for-each
   (lambda (f) (reload-module (file->module f)))
   get-all-changed-files))
