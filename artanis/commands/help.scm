;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015
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

(define-module (artanis commands help)
  #:use-module (artanis utils)
  #:use-module (artanis commands)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format))


;; NOTE: put your command in the head of %load-path as possible
(define (get-all-commands)
  (map remove-ext
       (scandir (dirname (current-filename))
                (lambda (f) 
                  (not (or (string=? f ".") 
                           (string=? f "..")))))))

(define (get-info cmd)
  (let ((sym (string->symbol cmd)))
    (module-ref (resolve-module `(artanis commands ,sym)) '%summary)))

(define (show-cmds-info)
  (let ((cmds (get-all-commands)))
    (apply string-append
           (map (lambda (cmd)
                  (format #f "~a~20t~a~%"
                          cmd (get-info cmd)))
                cmds))))

(define (gen-help-str)
  (string-append announce-head
                 "\ncommands:\n"
                 (show-cmds-info)
                 announce-foot))

(define (show-help) (display (gen-help-str)))

(define %summary "Show this screen")
(define main show-help)
