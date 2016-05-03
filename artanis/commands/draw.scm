;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015,2016
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
  #:use-module (ice-9 format)
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
  lib

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

;; NOTE: mode set to #f means create-file, so the maker needs a port
(define* (draw:create maker cname filename methods #:optional (mode #f))
  (define name (basename filename))
  (define component (basename (dirname filename)))
  (format (artanis-current-output) "working ~10t ~a `~a'~%"
          (string-capitalize component) name)
  (cond
   ((draw:is-dry-run?)
    (cond
     (mode (maker cname methods)) ; should be handled in actual maker
     (else
      (call-with-output-file *null-device*
        (lambda (port) (maker cname methods port))))))
   (else
    (case mode
      ((create-dir)
       (when (file-exists? filename) (handle-existing-file filename))
       (mkdir filename)
       (maker cname methods))
      ((enter-dir)
       (if (file-exists? filename)
           ;; remove non-dir if exists
           (when (not (file-is-directory? filename)) (handle-existing-file filename))
           ;; create dir if doesn't exist
           (mkdir filename))
       (maker cname methods))
      ((#f)
       (when (file-exists? filename) (handle-existing-file filename))
       (call-with-output-file filename
         (lambda (port) (maker cname methods port))))
      (else (error draw:create "BUG: wrong mode!" mode))))))

(define (%draw-model name . methods)
  (let* ((path (current-toplevel))
         (entry (string-append path "/ENTRY"))
         (cpath (string-append path "/app/models/" name ".scm")))
    (cond
     ((not (verify-ENTRY entry))
      (error "You're not in a valid Artanis app directory! Or ENTRY is invalid!"))
     (else
      (draw:create do-model-create name cpath methods)
      ;; TODO: maybe others
      (%draw-test name)))))

(define (%draw-view name . methods)
  (let* ((path (current-toplevel))
         (entry (string-append path "/ENTRY"))
         (cpath (string-append path "/app/views/" name)))
    (cond
     ((not (verify-ENTRY entry))
      (error "You're not in a valid Artanis app directory! Or ENTRY is invalid!"))
     (else
      (draw:create do-view-create name cpath methods 'enter-dir)
      ;; TODO: maybe others
      (%draw-test name)))))

(define (%draw-controller name . methods)
  (let* ((path (current-toplevel))
         (entry (string-append path "/ENTRY"))
         (cpath (string-append path "/app/controllers/" name ".scm")))
    (cond
     ((not (verify-ENTRY entry))
      (error "You're not in a valid Artanis app directory! Or ENTRY is invalid!"))
     (else
      (draw:create do-controller-create name cpath methods)
      (apply %draw-view name methods)
      ;; TODO: maybe others
      (%draw-test name)))))

(define (%draw-migration name)
  (let* ((path (current-toplevel))
         (entry (string-append path "/ENTRY"))
         (t (strftime "%Y%m%d%H%M%S" (localtime (current-time))))
         (f (format #f "~a_~a.scm" t name))
         (cpath (string-append path "/db/migration/" f)))
    (cond
     ((not (verify-ENTRY entry))
      (error "You're not in a valid Artanis app directory! Or ENTRY is invalid!"))
     (else (draw:create do-migration-create name cpath '())))))

(define (%draw-test name)
  #t)

(define *lib-header*
  ";;;; This lib file was generated by GNU Artanis, please add your license header below.
;;;; And please respect the freedom of other people, please.
;;;; <YOUR LICENSE HERE>

(define-module (~a ~a)
  ;;#:use-module (uncomment_to_add_your_imported_module_here)
  #:export (#;uncomment_to_add_your_exported_stuffs_here))
")

(define (%draw-lib name . _)
  (define (gen-lib-header)
    (format #f *lib-header* (current-appname) name))
  (define (do-lib-create _ __ port)
    (display (gen-lib-header) port))
  (let* ((cpath (current-toplevel))
         (entry (string-append cpath "/ENTRY"))
         (mpath (format #f "~a/lib/~a" cpath (current-appname)))
         (lpath (format #f "~a/~a.scm" mpath name)))
    (cond
     ((not (verify-ENTRY entry))
      (error "You're not in a valid Artanis app directory! Or ENTRY is invalid!"))
     (else
      (when (and (not (draw:is-dry-run?)) (not (file-exists? mpath)))
            (mkdir mpath))
      (display lpath) (newline)
      (draw:create do-lib-create name lpath '())))))

(define *component-handlers*
  `(("model"      . ,%draw-model)
    ("view"       . ,%draw-view)
    ("controller" . ,%draw-controller)
    ("migration"  . ,%draw-migration)
    ("lib"        . ,%draw-lib)
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
