;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2016,2017,2018
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
  #:use-module (artanis tpl utils)
  #:use-module (artanis tpl lexer)
  #:use-module (system base lalr)
  #:export (tpl-read))

(define (tpl-read port)
  (make-reader make-parser make-tpl-tokenizer port))

(define (gen-command cmd args)
  (let ((pub (get-conf '(server pub))))
    (case cmd
      ((include)
       (let ((filename (string-trim-both
                        (format #f "~a/pub/~a" (current-toplevel) args))))
         (if (file-exists? filename)
             (format #f "~s" (cat filename #f))
             (throw 'artanis-err 500 gen-command
                    "Included file `~a' in template doesn't exist!" filename))))
      ((css)
       (format #f "\"<link rel=\\\"stylesheet\\\" href=\\\"~a/css/~a\\\">\"" pub args))
      ((icon)
       (format #f "\"<link rel=\\\"icon\\\" href=\\\"~a/img/~a\\\" type=\\\"image/x-icon\\\">\"" pub args))
      ((js)
       (format #f "\"<script type=\\\"text/javascript\\\" src=\\\"~a/js/~a\\\"> </script>\"" pub args))
      (else
       (throw 'artanis-err 500 gen-command
              "Invalid command `~a' in template!" cmd)))))

(define (make-parser)
  (lalr-parser
   (code disp-code html command) ; terminal tokens

   (tpls (tpls tpl) : (string-concatenate (list $1 $2))
         (tpl) : $1
         (*eoi*) : *eof-object*)

   (tpl (html) : (string-trim-both $1)
        (command) : (string-concatenate `("(display " ,(gen-command (car $1) (cdr $1)) ")"))
        (program) : $1)

   (program (code) : $1
            (disp-code) : (string-concatenate `("(display " ,$1 ")")))))
