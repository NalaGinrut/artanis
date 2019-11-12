;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2019
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

(define-module (artanis commands api)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis commands)
  #:use-module (artanis irregex)
  #:use-module (artanis webapi restful)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 readline)
  #:use-module (srfi srfi-1))

(define %summary "Generate WebAPI automatically, say, RESTful.")

(define option-spec
  '((help (single-char #\h) (value #f))
    (dry (single-char #\d) (value #f))
    (force (single-char #\f) (value #f))
    (skip (single-char #\s) (value #f))
    (quiet (single-char #\q) (value #f))
    (list (single-char #\l) (value #f))
    (mode (single-char #\m) (value #t))
    (ver (single-char #\v) (value #t))
    (create (single-char #\c) (value #f))))

(define help-str
  "
Usage:
  art api [options]

Options:
  -h, [--help]     # Print this screen
  -d, [--dry]      # Dry run but do not make any changes
  -f, [--force]    # Overwrite files that already exist
  -s, [--skip]     # Skip files that already exist
                   # If -s and -f are both provided, -f will be enabled
  -q, [--quiet]    # Suppress status output
  -l, [--list]     # List all defined APIs
  -m, [--mode]     # Specify the WebAPI mode, the default is \"restful\"
  -v, [--ver]      # Specify the WebAPI version, omit it to auto detect
  -c, [--create]   # Create WebAPI

Example:
  art api -v v3
")

(define (show-help)
  (display announce-head)
  (display help-str)
  (display announce-foot))

;; NOTE: mode set to #f means create-file, so the maker needs a port
(define* (api:create maker version filename)
  (cond
   ((cmd:is-dry-run?)
    (call-with-output-file *null-device*
      (lambda (port) (maker version port))))
   (else
    (when (file-exists? filename) (handle-existing-file filename))
    (call-with-output-file filename
      (lambda (port) (maker version port))))))

(define (gen-restful-api version)
  (let* ((path (current-toplevel))
         (entry (string-append path "/ENTRY"))
         (cpath (string-append path "/app/api/" version ".scm")))
    (cond
     ((not (verify-ENTRY entry))
      (error "You're not in a valid Artanis app directory! Or ENTRY is invalid!"))
     (else
      (when (not (file-exists? (dirname cpath)))
        (mkdir (dirname cpath)))
      (api:create do-restful-api-create version cpath)))))

(define *api-handlers*
  `(("restful" . ,gen-restful-api)
    ;; ("graphql"     . ,gen-graphql-api)
    ))

(define (do-api version mode)
  (format #t "creating ~10t ~a API ~a~%" mode version)
  (cond
   ((assoc-ref *api-handlers* mode)
    => (lambda (h) (h version)))
   (else
    (format #t "Invalid mode, the supported API mode are: ~{~a~^,~}~%"
            (map car *api-handlers*)))))

(define (option-spec-str)
  (fold (lambda (value acc) (string-append acc " --" (symbol->string (car value))))
        (string-append "--" (symbol->string (caar option-spec)))
        (cdr option-spec)))

(define (detect-version)
  (define-syntax-rule (ask-for-answer version)
    (let ((prompt (format #f "The highest API is `v~a', do you want to create `v~a'? (N/y) "
                          version (1+ version))))
      (match (readline prompt)
        ((or "y" "Y" "yes") (1+ version))
        (else #f))))
  (let* ((api-files (scan-app-components "api" #f))
         (ver (fold (lambda (f p)
                      (cond
                       ((irregex-match "v([0-9]+)" f)
                        => (lambda (m) (irregex-match-substring m 1)))
                       (else p)))
                    #f api-files)))
    (cond
     ((not ver) "v1")
     ((ask-for-answer (string->number ver))
      => (lambda (v) (format #f "v~a" v)))
     (else (exit 0)))))

(define (list-all-api)
  (let ((api-files (and (scan-app-components "api") '())))
    (format #t "~{~a~%~}~%" api-files)))

(define (api . args)
  (let ((options (if (null? args) '() (getopt-long args option-spec))))
    (define-syntax-rule (->opt k) (option-ref options k #f))
    (cond
     ((or (null? args) (->opt 'help)) (show-help))
     ((->opt 'options-list)
      (format #t "~a~%" (option-spec-str)))
     ((->opt 'list) (list-all-api))
     ((->opt 'create)
      (parameterize ((cmd:is-dry-run? (->opt 'dry))
                     (cmd:is-force? (->opt 'force))
                     (cmd:is-skip? (->opt 'skip))
                     (cmd:is-quiet? (->opt 'quiet)))
        (do-api
         (or (->opt 'ver) (detect-version))
         (or (->opt 'mode) "restful"))))
     (else (show-help)))))

(define main api)
