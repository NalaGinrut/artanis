;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2017
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

(define-module (artanis websocket protocols)
  #:use-module (artanis server server-context)
  #:use-module (artanis websocket frame)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (ice-9 match)
  #:use-module (ice-9 iconv)
  #:export (register-websocket-protocol!))

(::define (websocket-echo-read frame)
  (:anno: (websocket-frame) -> string)
  (bytevector->string (websocket-frame-payload frame)
                      (get-conf '(server charset))))

(::define (websocket-echo-write bv)
  (:anno: (bv) -> bv)
  bv)

(define *websocket-redirector-constructors*
  `((echo ,websocket-echo-read ,websocket-echo-write)))

(::define (register-websocket-protocol! server client proto-name remote-port)
  (:anno: (ragnarok-server ragnarok-client symbol) -> redirector)
  (let* ((info (assq-ref *websocket-redirector-constructors* proto-name))
         (reader (car info))
         (writer (cadr info)))
   (register-redirector!
    server
    client
    reader
    writer
    proto-name
    ;; NOTE: For non-proxy, the remote-port is always #f
    (if (eq? proto-name 'proxy) remote-port #f))))
