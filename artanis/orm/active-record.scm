;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014
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

(define-module (artanis orm active-record)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis ssql)
  #:use-module (artanis db)
  #:use-module (artanis orm table)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:export ())

;; TODO:
;; 1. wrap a helper macro to specify the database for users in convinient way.
;; 2. All the methods should return the instance of <active-record> for cascading calling.

(define-class <active-record> ()
  ;; 'db' field should be specified in users class inherited from <active-record>.
  (db #:init-value #f #:init-keyword #:db #:accessor ar:db)
  ;; 'table' field will be filled by a <db-table> instance which is generated in initialize method automatically.
  (table #:init-value #f #:init-keyword #:table #:accessor ar:table)
  ;; 'conds' contains all the conditions of the instance of <active-record>. It's a list so you may add new conditions 
  ;; each time when you calling condition-method cascadingly.
  (conds #:init-keyword '() #:accessor ar:conds)
  ;; 'cache' field holds all generated SQL string for query.
  (cache #:init-value #f #:accessor ar:cache))

;; 'where' is used to generate condition string of SQL
;; There're several modes in it, and can be composited.
;; FIXME: Have to checkout sql-injection in the field value, especially '--'
(define (where . conds)
  (define (->string obj) (if (string? obj) obj (object->string obj)))
  (define (->range lst) 
    (match lst
      (((? integer? from) (? integer? to))
       (map ->string lst))
      (else (throw 'artanis-err 500 "[ORM] where: invalid range" lst))))
  (define (get-the-conds-str key val)
    (let ((k (symbol->string key))
          (v (if (list? val) (->range val) (->string val))))
      (case key
        ;; These are reversed key in SQL, we use string-concatenate for efficiency
        ((limit)
         (artanis-check-if-current-DB-support key)
         (if (list? v)
             (format #f " ~a ~{~a~^, ~} " k v)
             (string-concatenate (list " " k " " v " ")))) 
        (else (string-concatenate (list " " k "=\"" v "\" "))))))
  (match conds
    (() "")
    ;; If the only arg is string, return it directly
    ((? string? conds) conds)
    ;; string template mode
    ;; e.g: (where "name = ${name}" #:name nala) ==> "name = \"nala\""
    (((? string? stpl) . vals)
     (apply (make-orm-string-template stpl) vals))
    ;; AND mode:
    ;; (where #:name 'John #:age 15 #:email "john@artanis.com") 
    ;; ==> name="John" and age="15" and email="john@artanis.com"
    (((? keyword? key) (? non-list? val) . rest)
     (let* ((k (keyword->symbol key))
            (str (get-the-conds-str k val)))
       (string-concatenate `(,str ,(if (null? rest) "" " and ") ,(apply where rest)))))
    ;; OR mode:
    ;; (where #:name '(John Tom Jim)) ==> name="John" or name="Tom" or name="Jim"
    (((? keyword? key) (vals ...) . rest)
     (let ((fmt (string-concatenate `("~{" ,(keyword->string key) "=\"~a\"~^ or ~}"))))
       (format #f fmt vals)))
    (else (throw 'artanis-err 500 "[ORM] where: invalid condition pattern" conds))))

(define-method (ar:result-fetch (self <active-record>))

)

(define (%find-first ar name value)
  
  )

(define-method (find-first (self <active-record>) (name <symbol>) (value <top>))
  
)

(define-method (find-first (self <active-record>) (name <symbol>) (value <top>) (conds <string>))
  )
  

;;------------------------------condition methods----------------------------------
(define-method (first (self <active-record>))
  (set! (ar:conds self) (cons " limit 1 " (ar:conds self))))

;;------------------------------dump methods--------------------------------
(define-syntax-rule (%generate-conds conds)
  (string-concatenate (reverse conds)))

(define-method (save (self <active-record>))
  (let* ((conds (%generate-conds (ar:conds self)))
   )))
