;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2016,2017,2018,2019,2021,2022
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

(define-module (artanis tpl parser)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis config)
  #:use-module (artanis irregex)
  #:use-module (artanis tpl utils)
  #:use-module (artanis tpl lexer)
  #:use-module (artanis third-party json)
  #:use-module (system base lalr)
  #:export (tpl-read))

(define should-expand? (make-parameter #t))

(define (tpl-read port)
  (make-reader make-parser make-tpl-tokenizer port))

(define *url-re* (string->irregex "(http|https)://*"))
(define *charset-re* (string->irregex "charset=([^ ]+)"))
(define *file-re* (string->irregex "(.+[.][^ ]+)"))

(define (include-the-file args)
  (let* ((pub (basename (get-conf '(server pub))))
         (filename (string-trim-both
                    (format #f "~a/~a/~a" (current-toplevel) pub args))))
    (if (file-exists? filename)
        ;;(format #f "~s" (cat filename #f))
        (call-with-input-string (cat filename #f) tpl-read)
        (throw 'artanis-err 500 include-the-file
               "Included file `~a' in template doesn't exist!" filename))))

(define (gen-command cmd args)
  (define-syntax-rule (-> x)
    (cond
     ((irregex-search *file-re* x)
      => (lambda (m)
           (string-trim-both
            (irregex-match-substring m 1)
            (lambda (ch) (char-set-contains? char-set:whitespace ch)))))
     (else
      (throw 'artanis-err 500 gen-command
             "Tempate rendering error: invalid args `~a'!" args))))
  (define-syntax-rule (->js-hash filename)
    (let* ((dir (dirname filename))
           (file (basename filename))
           (path (get-conf '(server jsmanifest)))
           (mfile (format #f "~a/~a/manifest.json" (current-toplevel) path))
           (jsmap (and mfile (file-exists? mfile)
                       (call-with-input-file mfile json->scm))))
      (cond
       ((or (and jsmap (assoc-ref jsmap file)) filename)
        => (lambda (target)
             (if (string=? "." dir)
                 target
                 (format #f "~a/~a" dir target))))
       (else filename))))
  (define-syntax-rule (->url args)
    (let ((file (-> args)))
      (cond
       ((irregex-search *url-re* file)
        args)
       (else
        (case cmd
          ((css) (format #f "/css/~a" file))
          ((icon) (format #f "/img/~a" file))
          ((js module) (format #f "/js/~a" (->js-hash file)))
          (else
           (throw 'artanis-err 500 gen-command
                  "Invalid command `~a' in template!" cmd)))))))
  (define-syntax-rule (->charset args)
    (format #f
            "charset=\\\"~a\\\""
            (cond
             ((irregex-search *charset-re* args)
              => (lambda (m)
                   (irregex-match-substring m 1)))
             (else "utf-8"))))
  (case cmd
    ((css)
     (format #f "\"<link rel=\\\"stylesheet\\\" href=\\\"~a\\\" />\""
             (->url args)))
    ((icon)
     (format #f "\"<link rel=\\\"icon\\\" href=\\\"~a\\\" type=\\\"image/x-icon\\\" />\""
             (->url args)))
    ((module)
     (format #f "\"<script type=\\\"module\\\" src=\\\"~a\\\"> </script>\""
             (->url args)))
    ((js)
     (format #f "\"<script type=\\\"text/javascript\\\" ~a src=\\\"~a\\\"> </script>\""
             (->charset args) (->url args)))
    ((free-js-ann) (object->string free-JS-announcement))
    (else
     (throw 'artanis-err 500 gen-command
            "Invalid command `~a' in template!" cmd))))

(define (make-parser)
  (lalr-parser
   (code disp-code html include command) ; terminal tokens

   (tpls (tpls tpl) : (string-concatenate (list $1 $2))
         (tpl) : $1
         (*eoi*) : *eof-object*)

   (tpl (html) : (string-trim-both $1)
        (include) : (include-the-file (cdr $1))
        (command) : (string-concatenate `("(display " ,(gen-command (car $1) (cdr $1)) ")"))
        (program) : $1)

   (program (code) : $1
            (disp-code) : (string-concatenate `("(display " ,$1 ")")))))
