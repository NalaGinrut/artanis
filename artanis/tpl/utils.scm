;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015
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

(define-module (artanis tpl utils)
  #:use-module (artanis utils)
  #:use-module (ice-9 q)
  #:use-module (system base language)
  #:use-module (system base compile)
  #:use-module (system base lalr)
  #:use-module (system base pmatch))

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

;; debug utils

(define* (make-token-checker tokenizer)
  (lambda* (src #:optional (mode 'slim))
    (let ((tokens (call-with-input-string src tokenizer)))
    (case mode
      ((slim) (map lexical-token-category tokens))
      ((all) tokens)
      (else (error make-token-checker "wrong mode" mode))))))
