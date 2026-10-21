;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2021
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

(define-module (artanis tpl utils)
  #:use-module (artanis utils)
  #:use-module (ice-9 q)
  #:use-module (system base language)
  #:use-module (system base compile)
  #:use-module (system base lalr)
  #:use-module (system base pmatch)
  #:export (is-whitespace?
            not!
            group-checker
            location
            unget-char1
            syntax-error
            lex-error
            *eof-object*
            make-reader
            port-source-location
            return
            ->
            pmatch/source
            hash-keys
            make-token-checker
            *tpl-include-views-dir*
            unsafe-tpl-include-path?
            tpl-include-roots
            resolve-tpl-include-path))

(module-export-all! (current-module))

(define is-whitespace?
  (lambda (c)
    (and (char? c) (char-set-contains? char-set:whitespace c))))

(define-syntax-rule (not! x)
  (set! x (not x)))

(define-syntax-rule (group-checker what c)
  (and (not (eof-object? c))
       (string-contains what (string c))))

(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
         (and (not (null? props))
              props))))

(define (unget-char1 c port)
  (and (char? c) (unread-char c port)))

(define* (syntax-error message #:optional token)
  (if (lexical-token? token)
      (throw 'syntax-error #f message
             (and=> (lexical-token-source token)
                    source-location->source-properties)
             (or (lexical-token-value token)
                 (lexical-token-category token))
             #f)
      (throw 'syntax-error #f message #f token #f)))

(define (lex-error what loc form . args)
  (throw 'lex-error #f what
         (and=> loc source-location->source-properties)
         form #f args))

(define *eof-object*
  (call-with-input-string "" read-char))

(define (make-reader make-parser make-tokenizer port)
  (let ((parse (make-parser)))
    (parse (make-tokenizer port) syntax-error)))

(define-syntax-rule (port-source-location port)
  (make-source-location (port-filename port)
                        (port-line port)
                        (port-column port)
                        (false-if-exception (ftell port))
                        #f))

(define-syntax-rule (return port category value)
  (make-lexical-token category (port-source-location port) value))

(define-syntax-rule (-> (type arg ...))
  `(type ,arg ...))

(define-syntax-rule (pmatch/source x clause ...)
  (let ((x x))
    (let ((res (pmatch x
                 clause ...)))
      (let ((loc (location x)))
        (if loc
            (set-source-properties! res (location x))))
      res)))

(define (hash-keys ht)
  (hash-map->list (lambda (k v) k) ht))

;; ----------------------------------------------------------------------
;; The `<@include file>' command
;; ----------------------------------------------------------------------

;; NOTE: `<@include file>' has always resolved under `server.pub', and every app
;;       in the wild relies on that, so the pub dir is always tried first. The
;;       MVC view dir is tried next, so a partial can live next to the templates
;;       using it. That also keeps partials out of the static namespace: every
;;       file under `pub' is served as-is, so a `.html' partial there is
;;       downloadable and leaks the raw `<@include>' markers, while anything
;;       under `app/views' isn't reachable with a URL at all.
(define *tpl-include-views-dir* "app/views")

;; Return the include roots (relative to the toplevel), in the order they're
;; tried. It's a procedure rather than a constant because the first root comes
;; from the config.
(define (tpl-include-roots pub)
  (list pub *tpl-include-views-dir*))

;; NOTE: Artanis constrains the path of sources in the application directory for
;;       security reasons, so an include must never escape its roots. The
;;       argument is raw template text, and the old code just concatenated it to
;;       the pub dir without any check, so `<@include ../../../etc/passwd %>'
;;       read whatever file was reachable from the toplevel. Reject NUL,
;;       absolute paths and every `..' component textually; symlink escapes are
;;       caught later, once we know which file we're about to read.
(define (unsafe-tpl-include-path? path)
  (define (has-parent-dir? segs)
    (and (pair? segs)
         (or (string=? (car segs) "..") (has-parent-dir? (cdr segs)))))
  (and (or (string-index path #\nul)
           (absolute-file-name? path)
           (has-parent-dir? (string-split path #\/)))
       #t))

;; NOTE: canonicalize-path throws when the path is gone (or a symlink loop is
;;       hit), and `file-exists?' already told us everything we need, so swallow
;;       it and let the caller treat #f as "not inside the root".
(define (canonical-path-or-#f path)
  (false-if-exception (canonicalize-path path)))

;; Resolve the argument of the `<@include ...>' command to a file we may read.
;;   path     - the raw text after the command, as written in the template
;;   toplevel - the app dir (i.e. `(current-toplevel)')
;;   pub      - the `(server pub)' dir name
;; Both are passed in instead of being fetched here, so this is a pure function
;; of its arguments (the only I/O is probing the candidates) and can be tested
;; without a server.
;; The first root holding the file wins, hence the legacy `pub' behavior is
;; preserved. An unsafe name, or a match whose canonical path leaves its root,
;; throws `artanis-err' 500 rather than falling back to the next root: silently
;; rendering the wrong file is worse than a loud failure.
(define (resolve-tpl-include-path path toplevel pub)
  (define roots (tpl-include-roots pub))
  (let ((arg (string-trim-both path)))
    (define (->candidate root)
      (format #f "~a/~a/~a" toplevel root arg))
    (define (->searched)
      (string-join (map ->candidate roots) "', `"))
    (define (inside-root? root candidate)
      ;; NOTE: A file under the root may still be a symlink pointing outside of
      ;;       it, so compare the canonical paths, not the textual ones.
      (let ((croot (canonical-path-or-#f (format #f "~a/~a" toplevel root)))
            (cfile (canonical-path-or-#f candidate)))
        (and croot cfile
             (string-prefix? (string-append croot "/") cfile))))
    (cond
     ((or (string-null? arg) (unsafe-tpl-include-path? arg))
      (throw 'artanis-err 500 resolve-tpl-include-path
             "Refusing to include `~a': the name has to be a relative path without any `..' component!"
             arg))
     (else
      (let lp((rest roots))
        (cond
         ((null? rest)
          (throw 'artanis-err 500 resolve-tpl-include-path
                 "Included file `~a' in template doesn't exist! (searched `~a')"
                 arg (->searched)))
         (else
          (let* ((root (car rest))
                 (candidate (->candidate root)))
            (cond
             ((not (file-exists? candidate)) (lp (cdr rest)))
             ((inside-root? root candidate) candidate)
             (else
              (throw 'artanis-err 500 resolve-tpl-include-path
                     "Refusing to include `~a': `~a' escapes the allowed root `~a'!"
                     arg candidate root)))))))))))

;; debug utils

(define* (make-token-checker tokenizer)
  (lambda* (src #:optional (mode 'slim))
    (let ((tokens (call-with-input-string src tokenizer)))
      (case mode
        ((slim) (map lexical-token-category tokens))
        ((all) tokens)
        (else (error make-token-checker "wrong mode" mode))))))
