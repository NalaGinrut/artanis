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
  #:use-module (artanis mvc controller)
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
  view
  controller
  test

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

;; TODO: handle it more elegantly
(define (handle-existing-path name)
  (format #t "~a exists!" name)
  (exit 1))

(define (%draw-model name)
  #t)

(define (%draw-view name)
  #t)

(define (%draw-controller name)
  (let* ((path (find-ENTRY-path identity))
         (entry (string-append path "/ENTRY"))
         (cpath (string-append path "/app/controller/" name)))
    (cond
     ((not (verify-ENTRY entry))
      (error "You're not in a valid Artanis app directory! Or ENTRY is invalid!"))
     ((file-exists? cpath)
      (handle-existing-file cpath))
     (else
      (controller-create cpath)
      (%draw-view name)
      ;; TODO: maybe others
      (%draw-test name)))))

(define (%draw-test name)
  #t)

(define *component-handlers*
  `(("model"      . ,%draw-model)
    ("view"       . ,%draw-view)
    ("controller" . ,%draw-controller)
    ("test"       . ,%draw-test)))

(define (do-draw component name)
  (format #t "dry-run: ~a~%force: ~a~%skip: ~a~%quiet: ~a~%" (is-dry-run?) (is-force?) (is-skip?) (is-quiet?))
  (format #t "drawing ~a ~a~%" component name)
  (or (and=> (assoc-ref *component-handlers* component)
             (lambda (h) (h name)))
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
