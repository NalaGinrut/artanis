;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2016,2017,2019
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

(define-module (artanis tpl lexer)
  #:use-module (artanis tpl utils)
  #:use-module (artanis irregex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (system base lalr)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (make-tpl-tokenizer
            debug-tpl-tokenizer))

(define sstart #\<)
(define ssend #\>)
(define smiddle #\%)
(define sshow #\=)
(define scmd #\@)
(define end-sign (string-append "\\\"" (string smiddle)))

(define-record-type lex-ctx
  (fields
   (mutable enter-string)
   (mutable code-start)
   (mutable cmd-start)
   (mutable last-char)))

(define (new-ctx)
  (make-lex-ctx #f #f #f #\nl))

(define* (next-is-code-start? c port ctx #:optional (mode 'type))
  (let ((c2 (peek-char port))
        (enter-string (lex-ctx-enter-string ctx)))
    (cond
     ((and (not enter-string) (char=? c sstart) (char=? c2 smiddle))
      (case mode
        ((type)
         (read-char port)
         (let ((c3 (peek-char port)))
           (cond
            ((char=? c3 sshow)
             (read-char port)
             'disp-code)
            (else
             (read-char port)
             'code))))
        ((check) #t)
        (else (throw 'artanis-err 500 next-is-code-start?
                     "invalid mode `~a'" mode))))
     (else #f))))

(define (read-code port)
  (read-delimited end-sign port 'peek))

;; FIXME: how about recursive embedded tpl?
;;        My vote is that never support it!
(define (get-the-code port ctx)
  (let ((enter-string (lex-ctx-enter-string ctx)))
    (let lp((code (read-code port)) (ret '()))
      (let ((c (read-char port)))
        (cond
         ((eof-object? c)
          (throw 'artanis-err 500 get-the-code
                 "Invalid template text! No proper end sign!"))
         ((and (not enter-string) (char=? c smiddle) (char=? (peek-char port) ssend))
          (read-char port) ; skip ssend
          (string-concatenate-reverse (cons code ret))) ; exit
         ((char=? #\" c)
          (lex-ctx-enter-string-set! ctx (not enter-string))
          (lex-ctx-last-char-set! ctx c)
          (lp (read-code port) (cons "\"" (cons code ret))))
         ((and (char=? c #\\) (char=? #\" (peek-char port)))
          (lex-ctx-enter-string-set! ctx #t)
          (read-char port)
          (lex-ctx-last-char-set! ctx c)
          (lp (read-code port) (cons code ret)))
         (else
          (lex-ctx-enter-string-set! ctx c)
          (lp (read-code port) (cons (string c) (cons code ret)))))))))

;; NOTE: Don't wrap code in double-quotes, for example:
;; 1. <a href="<%= my-url %>">click me</a> (Wrong!)
;; 2. <a href=<%= my-url %>click me</a> (Correct!)
;;    If you need to output a string, please use object->string to convert
;;    in Scheme first.
(define (read-html port)
  (irregex-replace/all "\"" (read-delimited "<" port 'peek) "\\\""))

(define (get-the-html port ctx)
  (let ((enter-string (lex-ctx-enter-string ctx))
        (code-start (lex-ctx-code-start ctx)))
    (let lp((html (read-html port)) (ret '()))
      (let ((c (read-char port)))
        (cond
         ((or (eof-object? c)
              (and (not enter-string)
                   (or (next-is-code-start? c port ctx 'check)
                       (next-is-cmd-start? c port ctx 'check))))
          (unget-char1 c port) ; #\<
          (string-concatenate
           `("(display \""
             ,(string-concatenate-reverse (cons html ret))
             "\")"))) ; exit
         ((char=? #\" c)
          (lex-ctx-enter-string-set! ctx (not enter-string))
          (lp (read-html port) (cons html ret)))
         (else
          (lp (read-html port) (cons (string c) (cons html ret)))))))))

(define* (next-is-cmd-start? c port ctx #:optional (mode 'type))
  (let ((c2 (peek-char port))
        (enter-string (lex-ctx-enter-string ctx)))
    (cond
     ((and (not enter-string) (char=? c sstart) (char=? c2 scmd))
      (case mode
        ((type)
         (read-char port) ; skip scmd
         (let ((cmd (read-delimited " \n\t" port)))
           (string->symbol cmd)))
        ((check) #t)
        (else (throw 'artanis-err 500 next-is-code-start?
                     "invalid mode `~a'" mode))))
     (else #f))))

(define (next-token port ctx)
  (let* ((enter-string (lex-ctx-enter-string ctx))
         (code-start (lex-ctx-code-start ctx))
         (cmd-start (lex-ctx-cmd-start ctx))
         (last-char (lex-ctx-last-char ctx))
         (c (read-char port))
         (next (lambda ()
                 (lex-ctx-last-char-set! ctx c)
                 (next-token port ctx))))
    (cond
     ((eof-object? c) '*eoi*)
     ((or (and (not (char=? last-char #\\)) (char=? c #\")) (char=? c #\'))
      ;; not an escaped double-quote
      ;; NOTE: HTML string may contain #\' as string quote, and we'll
      ;;       handle all code-string in get-the-code, so it's OK to
      ;;       check #\' here.
      (lex-ctx-enter-string-set! ctx (not enter-string))
      (next))
     ((and (not enter-string) (not code-start) (next-is-code-start? c port ctx))
      => (lambda (type)
           (lex-ctx-code-start-set! ctx #f)
           (return port type (get-the-code port ctx))))
     ((and (not enter-string) (not cmd-start) (next-is-cmd-start? c port ctx))
      => (lambda (cmd)
           (lex-ctx-cmd-start-set! ctx #f)
           (return port (if (eq? cmd 'include)
                            'include
                            'command)
                   (cons cmd (get-the-code port ctx)))))
     (else
      (unget-char1 c port)
      (return port 'html (get-the-html port ctx))))))

(define (make-tpl-tokenizer port)
  (lambda ()
    (next-token port (new-ctx))))

(define* (make-token-checker tokenizer)
  (lambda* (src #:optional (mode 'slim))
    (let ((tokens (call-with-input-string src tokenizer)))
      (case mode
        ((slim) (map lexical-token-category tokens))
        ((all) tokens)
        (else (throw 'artanis-err 500 make-token-checker
                     "make-token-checker: wrong mode `~a'" mode))))))

(define (tpl-tokenizer port)
  (let lp ((out '()))
    (let ((tok (next-token port (new-ctx))))
      (if (eq? tok '*eoi*)
          (reverse! out)
          (lp (cons tok out))))))

(define (debug-tpl-tokenizer src)
  ((make-token-checker tpl-tokenizer) src))
