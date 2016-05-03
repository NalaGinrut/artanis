;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2016
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
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (system base lalr)
  #:export (make-tpl-tokenizer
            debug-tpl-tokenizer))

(define sstart #\<)
(define ssend #\>)
(define smiddle #\%)
(define sshow #\=)
(define scmd #\@)
(define end-sign (string-append "\\\"" (string smiddle)))

(define enter-string #f)
(define code-start #f)
(define last-char #\nl)
  
(define* (next-is-code-start? c port #:optional (mode 'type))
  (let ((c2 (peek-char port)))
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
        (else (throw 'artanis-err 500 "invalid mode" mode))))
     (else #f))))

(define (read-code port)
  (read-delimited end-sign port 'peek))

;; FIXME: how about recursive embedded tpl?
;;        My vote is never support it!
(define (get-the-code port)
  (let lp((code (read-code port)) (ret '()))
    (let ((c (read-char port)))
      (cond
       ((eof-object? c)
        (throw 'artanis-err 500 "Invalid template text! No proper end sign!"))
       ((and (not enter-string) (char=? c smiddle) (char=? (peek-char port) ssend))
        (read-char port) ; skip ssend
        (string-concatenate-reverse (cons code ret))) ; exit
       ((char=? #\" c)
        (not! enter-string)
        (set! last-char c)
        (lp (read-code port) (cons "\"" (cons code ret))))
       ((and (char=? c #\\) (char=? #\" (peek-char port)))
        (set! enter-string #t)
        (read-char port)
        (set! last-char c)
        (lp (read-code port) (cons "\\\"" (cons code ret))))
       (else
        (set! enter-string c)
        (lp (read-code port) (cons (string c) (cons code ret))))))))

(define (read-html port)
  (read-delimited "\"<" port 'peek))

(define (get-the-html port)
  (let lp((html (read-html port)) (ret '()))
    (let ((c (read-char port)))
      (cond
       ((or (eof-object? c) 
            (and (not enter-string) (next-is-code-start? c port 'check)))
        (unget-char1 c port) ; #\<
        (string-concatenate 
         `("(display \""
           ,(string-concatenate-reverse (cons html ret))
           "\")"))) ; exit
       ((char=? #\" c)
        (not! enter-string)
        (lp (read-html port) (cons "\\\"" (cons html ret))))
       (else
        (lp (read-html port) (cons (string c) (cons html ret))))))))

(define (next-is-cmd-start? c port)
  (let ((c2 (peek-char port)))
    (cond
     ((and (not enter-string) (char=? c sstart) (char=? c2 scmd))
      (read-char port) ; skip scmd
      (let ((cmd (read-delimited " " port)))
        (string->symbol cmd)))
     (else #f))))
      
(define next-token
  (lambda (port)
    (let* ((c (read-char port))
           (next (lambda ()
                   (set! last-char c)
                   (next-token port))))
      (cond
       ((eof-object? c) '*eoi*)
       ((or (and (not (char=? last-char #\\)) (char=? c #\")) (char=? c #\')) 
        ;; not an escaped double-quote
        ;; NOTE: HTML string may contain #\' as string quote, and we'll
        ;;       handle all code-string in get-the-code, so it's OK to
        ;;       check #\' here.
        (not! enter-string)
        (next))
       ((and (not enter-string) (not code-start) (next-is-code-start? c port))
        => (lambda (type)
             (set! code-start #f)
             (return port type (get-the-code port))))
       ((and (not enter-string) (not code-start) (next-is-cmd-start? c port))
        => (lambda (cmd)
             (set! code-start #f)
             (return port 'command (cons cmd (get-the-code port)))))
       (else
        (unget-char1 c port)
        (return port 'html (get-the-html port)))))))

(define (make-tpl-tokenizer port)
  (lambda ()
    (next-token port)))

(define* (make-token-checker tokenizer)
  (lambda* (src #:optional (mode 'slim))
    (let ((tokens (call-with-input-string src tokenizer)))
    (case mode
      ((slim) (map lexical-token-category tokens))
      ((all) tokens)
      (else (throw 'artanis-err 500 "make-token-checker: wrong mode" mode))))))

(define tpl-tokenizer
  (lambda (port)
    (let lp ((out '()))
      (let ((tok (next-token port)))
        (if (eq? tok '*eoi*)
            (reverse! out)
            (lp (cons tok out)))))))

(define (debug-tpl-tokenizer src)
  ((make-token-checker tpl-tokenizer) src))
