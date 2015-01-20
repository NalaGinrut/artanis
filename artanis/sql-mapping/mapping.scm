;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014,2015
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

(define-module (artanis sql-mapping mapping)
  #:use-module (artanis sql-mapping handlers)
  #:use-module (artanis utils)
  #:use-module (artanis irregex)
  #:use-module (artanis route)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
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
             (and=> (irregex-search re name) irregex-match-substring))))))

(define *bre* (string->irregex "([^#]+)#([^:]*)"))
(define (sm-name-parser name)
  ;; NOTE: it's ok if there's strange symbols in the name, since it'll never
  ;;       be referenced by the template.
  (define (get-name port)
    (string-trim-both (read-delimited ":" port)))
  (define (get-opts port)
    (map (lambda (opt)
           (let ((s (string-trim-both opt)))
             (or (get-sm-opt-handler s)
                 (valid-re? s))))
         (string-split (get-string-all port) #\,)))
  (define (get-args port)
    (string-trim-both (get-string-all port)))
  (define (any-built-in? name)
    (let ((m (irregex-search *bre* name)))
      (if m
          (values (irregex-match-substring m 1)
                  (irregex-match-substring m 2))
          (values name #f))))
  (define (%parser port)
    (let ((c (peek-char port)))
      (cond
       ((eof-object? c)
        (throw 'artanis-err 500 "sm-name-parser: Shouldn't be empty name!" name))
       ((char=? c #\@) ; post-qstr
        (read-char port) ; skip #\@
        `(post-qstr ,(string->keyword (get-name port)) ,(get-opts port)))
       (else
        (receive (name built)
            (any-built-in? (get-name port))
          (if built
              `(built-in ,(string->keyword name) ,built ,(get-args port))
              `(name ,(string->keyword name) ,(get-opts port))))))))
  (call-with-input-string name %parser))

;; A modification of %make-string-template to support special syntax
;; of sql-mapping.
(define* (make-sm-string-template template #:optional (options '()))
  (define irx (sre->irregex '(or (=> dollar "$$")
                                 (: "${" (=> var (+ (~ #\}))) "}"))
                            'fast))
  (define *opt-table* options)
  (define (opt-ref k) (assq-ref *opt-table* k))
  (define (opt-add! k v)
    (set! *opt-table* (assq-set! *opt-table* k v)))
  (define *post-table* '())
  (define (post-table-add! name)
    (set! *post-table* (cons name *post-table*)))
  (define (post-table-ref name) (memq name *post-table*))
  (define (->string obj)
    (let* ((k (if (string? obj) obj (object->string obj)))
           (h (opt-ref k)))
      (string-concatenate (list "\"" (if h (h k) k) "\""))))
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
         (and opts (opt-add! n (apply make-pipeline opts)))
         n)
        (`(post-qstr ,pn ,opts)
         (and opts (opt-add! pn (apply make-pipeline opts)))
         (post-table-add! pn)
         pn)
        (`(built-in ,n ,func ,args)
         ;; TODO: finish built-in
         ;;(build-in-add! n func args)
         n)
        (else (throw 'artanis-err 500 "make-sm-string-template: Invalid result after parse"
                     nn)))))
  (define (match->item m)
    (or (and (irregex-match-substring m 'dollar) "$")
        (let ((name (irregex-match-substring m 'var)))
          (list (parse-it name)))))
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
      (define (get-post-data)
        (when (not post-data)
          (set! post-data
                (if (rc-body rc)
                    (generate-kv-from-post-qstr
                     (rc-body rc) #:no-evil? #t #:key-converter string->keyword)
                    '())))
        post-data)
      (define (post-ref key) (and=> (assoc-ref (get-post-data) key) car))
      (define (qstr-ref k)
        (and (post-table-ref k) (post-ref k)))
      (define (item->string item)
        (if (string? item)
            item
            (cond
             ((kw-arg-ref kargs (car item)) => ->string)
             ((qstr-ref (car item)) => ->string)
             (else (throw 'artanis-err 500 "make-sm-string-template: Missing keyword" (car item))))))
      (string-concatenate (map item->string items)))))
