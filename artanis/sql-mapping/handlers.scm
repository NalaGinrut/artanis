;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014,2015
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

(define-module (artanis sql-mapping handlers)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis irregex)
  #:export (get-sm-opt-handler))

;; NOTE: For handlers, there's convention for return value.
;;       #f for false, valid result for true.

(define (no-null-handler name)
  (if (string-null? name)
      #f
      name))

;; NOTE: it's 10x faster after SRE was compiled.
;; FIXME: drop escaped chars too.
;; TODO: add skyeye mode to conf.
;; TODO: maybe we should throw exception when ASIM hit?
(define *no-dash-re*
  (sre->irregex '(: (=> name (* (or (? (~ #\-))
                                    (: (? (~ #\-) "-" (? (~ #\-)))))))
                    (? (: "--" (=> drop (* print)))))
                'fast))
(define (no-dash-handler name)
  (let ((m (irregex-search *no-dash-re* name)))
    (and m
         (and (get-conf 'skyeye)
              (format (current-error-port)
                      "[ASIM: double-hyphen]: ~a~%"
                      (irregex-match-substring m 'drop)))
         (irregex-match-substring m 'name))))

(define *html-whitespace*
  '((#\nl . "<br/>") (#\sp . "&nbsp;")
    ("%0A" . "<br/>") ("%20" . "&nbsp;")))
(define (valid-whitespace content)
  (HTML-entities-replace *html-whitespace* content))

(define *sm-opt-handler-table*
  `(("no-null" . no-null-handler)
    ("no-dash" . no-dash-handler)
    ("valid-whitespace" . valid-whitespace-handler)))

(define (get-sm-opt-handler name)
  (assoc-ref *sm-opt-handler-table* name))
