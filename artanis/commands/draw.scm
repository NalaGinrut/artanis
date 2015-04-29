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

(define-module (artanis commands draw)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis commands)
  #:use-module (artanis irregex)
  #:use-module (artanis mvc model)
  #:use-module (artanis mvc controller)
  #:use-module (artanis mvc view)
  #:use-module (artanis mvc migration)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match))

(define %summary "Generate component automatically, say, MVC.")

(define option-spec
  '((help (single-char #\h) (value #f))
    (dry (single-char #\d) (value #f))
    (force (single-char #\f) (value #f))
    (skip (single-char #\s) (value #f))
    (quiet (single-char #\q) (value #f))))

(define help-str
"
Usage:
  art draw <component> NAME [options]

component list:
  model
  controller
  migration

Options:
  -h, [--help]     # Print this screen
  -d, [--dry]      # Dry run but do not make any changes
  -f, [--force]    # Overwrite files that already exist
  -s, [--skip]     # Skip files that already exist
                   # If -s and -f are both provided, -f will be enabled
  -q, [--quiet]    # Suppress status output                   

Example:
  art draw model myblog
")

(define (show-help)
  (display announce-head)
  (display help-str)
  (display announce-foot))

(define (draw:create maker cname filename methods)
  (define name (basename filename))
  (define component (basename (dirname filename)))
  (format (artanis-current-output) "create ~10t app/~a/~a~%" component name)
  (cond
   ((draw:is-dry-run?)
    (call-with-output-file *null-device*
      (lambda (port) (maker cname methods port))))
   ((draw:is-force?)
    (delete-file filename)
    (call-with-output-file filename
      (lambda (port) (maker cname methods port))))
   ((draw:is-skip?)
    (format (artanis-current-output) "skip ~10t app/~a/~a~%" component name))
   (else
    (when (file-exists? filename) (handle-existing-file filename))
    (call-with-output-file filename
      (lambda (port) (maker cname methods port))))))

;; TODO: handle it more elegantly
(define (handle-existing-file path)
  (define component (basename (dirname path)))
  (define name (car (string-split (basename path) #\.)))
  (cond
   ((draw:is-force?) (delete-file path))
   (else
    (format #t "~a `~a' exists! (Use --force/-f to overwrite or --skip/-s to ignore)~%"
            (string-capitalize component) name)
    (exit 1))))

(define (%draw-model name . methods)
  (let* ((path (find-ENTRY-path identity))
         (entry (string-append path "/ENTRY"))
         (cpath (string-append path "/app/model/" name ".scm")))
    (cond
     ((not (verify-ENTRY entry))
      (error "You're not in a valid Artanis app directory! Or ENTRY is invalid!"))
     (else
      (%draw-migration name)
      (draw:create do-model-create name cpath methods)
      ;; TODO: maybe others
      (%draw-test name)))))

(define (%draw-view name . methods)
  (let* ((path (find-ENTRY-path identity))
         (entry (string-append path "/ENTRY"))
         (cpath (string-append path "/app/view/" name ".tpl")))
    (cond
     ((not (verify-ENTRY entry))
      (error "You're not in a valid Artanis app directory! Or ENTRY is invalid!"))
     (else
      (draw:create do-view-create name cpath methods)
      ;; TODO: maybe others
      (%draw-test name)))))

(define (%draw-controller name . methods)
  (let* ((path (find-ENTRY-path identity))
         (entry (string-append path "/ENTRY"))
         (cpath (string-append path "/app/controller/" name ".scm")))
    (cond
     ((not (verify-ENTRY entry))
      (error "You're not in a valid Artanis app directory! Or ENTRY is invalid!"))
     (else
      (draw:create do-controller-create name cpath methods)
      (%draw-view name)
      ;; TODO: maybe others
      (%draw-test name)))))

(define (%draw-migration name)
  (let* ((path (find-ENTRY-path identity))
         (entry (string-append path "/ENTRY"))
         (cpath (string-append path "/db/migration/" name)))
    (cond
     ((not (verify-ENTRY entry))
      (error "You're not in a valid Artanis app directory! Or ENTRY is invalid!"))
     (else (draw:create do-migration-create cpath)))))

(define (%draw-test name)
  #t)

(define *component-handlers*
  `(("model"      . ,%draw-model)
    ("controller" . ,%draw-controller)
    ("migration"  . ,%draw-migration)
    ;; ("api"     . ,%draw-api)
    ))

(define (do-draw component name . methods)
  (format #t "drawing ~10t ~a ~a~%" component name)
  (or (and=> (assoc-ref *component-handlers* component)
             (lambda (h) (apply h name methods)))
      (error do-draw "Invalid component, please see help!" component)))

(define (draw . args)
  (let ((options (if (null? args) '() (getopt-long args option-spec))))
    (define-syntax-rule (->opt k) (option-ref options k #f))
    (cond
     ((or (null? args) (->opt 'help)) (show-help))
     (else
      (parameterize ((draw:is-dry-run? (->opt 'dry))
                     (draw:is-force? (->opt 'force))
                     (draw:is-skip? (->opt 'skip))
                     (draw:is-quiet? (->opt 'quiet)))
        (apply do-draw (->opt '())))))))

(define main draw)
