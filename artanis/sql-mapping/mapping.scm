;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  Artanis is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  Artanis is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (artanis sql-mapping mapping)
  #:use-module (artanis sql-mapping handlers)
  #:use-module (artanis irregex)
  #:use-module (artanis route)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module ((rnrs) #:select (get-string-all utf8->string))
  #:export (make-sm-string-template))

;; NOTE: literal RE couldn't use groups!
(define *vre* (string->irregex "re\"(.*)\""))
(define (valid-re? str)
  (let ((m (irregex-search *vre* str)))
    (and m
         (let ((re (string->irregex (irregex-match-substring m 1))))
           (lambda (name)
             (let ((m (irregex-search re name)))
               (and m (irregex-match-substring m))))))))

(define (sm-name-parser name)
  ;; NOTE: it's ok if there's strange symbols in the name, since it'll never
  ;;       be referenced by the template.
  (define (get-rest port)
    (string->symbol (string-trim-both (get-string-all port))))
  (define (get-name port)
    (string->symbol (string-trim-both (read-delimited ":" port))))
  (define (get-opts port)
    (map (lambda (opt)
           (let ((s (string-trim-both opt)))
             (or (get-sm-opt-handler s)
                 (valid-re? s))))
         (string-split (get-string-all port) #\,)))
  (define (%parser port)
    (let ((c (peek-char port)))
      (cond
       ((eof-object? c)
        (throw 'artanis-err 500 "sm-name-parser: Shouldn't be empty name!" name))
       ((char=? c #\@) ; post-qstr
        (read-char port) ; skip #\@
        `(post-qstr ,(get-name port) ,(get-opts port)))
       (else `(name ,(get-name port) ,(get-opts port))))))
  (call-with-input-string name %parser))

;; A modification of %make-string-template to support special syntax
;; of sql-mapping.
(define* (make-sm-string-template template #:optional (options '()))
  (define irx (sre->irregex '(or (=> dollar "$$")
                                 (: "${" (=> var (+ (~ #\}))) "}"))
                            'fast))
  (define *opt-table* options)
  (define (opt-ref k) (assoc-ref *opt-table* k))
  (define (opt-set! k v)
    (set! *opt-table* (assoc-set! *opt-table* k v)))
  (define *post-table* '())
  (define (post-table-add! k v)
    (set! *post-table* (assq-set! *post-table* k v)))
  (define (post-table-ref k) (assq-ref *post-table* k))
  (define (->string rc obj)
    (let* ((k (if (string? obj) obj (object->string obj)))
           (h (opt-ref k)))
      (if h
          (h k)
          k)))
  (define (optimize rev-items tail)
    (cond ((null? rev-items) tail)
          ((not (string? (car rev-items)))
           (optimize (cdr rev-items)
                     (cons (car rev-items) tail)))
          (else (receive (strings rest) (span string? rev-items)
                  (let ((s (string-concatenate-reverse strings)))
                    (if (string-null? s)
                        (optimize rest tail)
                        (optimize rest (cons s tail))))))))
  (define (parse-it name)
    (let ((nn (sm-name-parser name)))
      (match nn
        (`(name ,n ,opts)
         (and opt (opt-add! n (apply make-pipeline opts)))
         (names-table-add! n 'name))
        (`(post-qstr ,pn ,opts)
         (and opt (opt-set! pn (apply make-pipeline opts)))
         (names-table-add! pn 'post-qstr))
        (else (throw 'artanis-err 500 "make-sm-string-template: Invalid result after parse"
                     nn)))))
  (define (match->item m)
    (or (and (irregex-match-substring m 'dollar) "$")
        (let ((name (irregex-match-substring m 'var)))
          (parse-it name)
          (list name))))
  (let* ((rev-items (irregex-fold
                     irx
                     (lambda (idx m tail)
                       (cons* (match->item m)
                              (substring template
                                         idx
                                         (irregex-match-start-index m 0))
                              tail))
                     '()
                     template
                     (lambda (idx tail)
                       (cons (substring template idx) tail))))
         (items (optimize rev-items '())))
    (lambda (rc . kargs)
      (define post-data #f)
      (define (get-post-data rc)
        (define (-> x)
          (string-trim-both x (lambda (c) (member c '(#\sp #\: #\return)))))
        (or post-data
            (if (rc-body rc)
                (set! post-data
                      (map (lambda (x)
                             (map -> (string-split x #\=)))
                           (string-split (utf8->string (rc-body rc)) #\&)))
                '())))
      (define (post-ref key) (and=> (assoc-ref (get-post-data rc) key) car))
      (define (get-the-val lst key)
        (let ((str (kw-arg-ref lst key)))
          (string-concatenate (list "\"" (->string rc str) "\""))))
      (define (qstr-ref k)
        (and (post-table-ref k) (post-ref k)))
      (define (item->string item)
        (if (string? item)
            item
            (cond
             ((get-the-val kargs (car item)) => ->string)
             ((qstr-ref rc (car item)) => ->string)
             ((cdr item) (cdr item))
             (else (throw 'artanis-err 500 "item->string: Missing keyword" (car item))))))
      (string-concatenate (map item->string items))))))))
