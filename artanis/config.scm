;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013
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

(define-module (artanis config)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:export (init-config))

(define server-info "Artanis-0.0.1")

(define (parse-config-item item)
  (match item
    (('db rest ...) (parse-namespace-db item))
    (('server rest ...) (parse-namespace-server item))
    (('host rest ...) (parse-namespace-host item))
    (('error rest ...) (parse-namespace-error item))
    (('session rest ...) (parse-namespace-session item))
    (('upload rest ...) (parse-namespace-upload item))
    (('mail rest ...) (parse-namespace-mail item))
    (else (error parse-config-item "Unsupported config namespace!" item))))

;; TODO: implement all parser

(define (init-config)
  #t)
