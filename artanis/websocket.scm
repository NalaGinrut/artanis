;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2016,2017
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

(define-module (artanis websocket)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis irregex)
  #:use-module (artanis server server-context)
  #:use-module (artanis websocket handshake)
  #:use-module (artanis websocket frame)
  #:use-module ((rnrs)
                #:select (make-bytevector
                          bytevector-u8-set!
                          utf8->string
                          string->bytevector
                          bytevector-length
                          bytevector-u8-ref
                          bytevector-u8-ref
                          bytevector-u8-ref))
  #:export (detect-if-connecting-websocket
            websocket-read
            websocket-write)
  #:re-export (; from (artanis websocket handshake)
               do-websocket-handshake
               closing-websocket-handshake
               this-rule-enabled-websocket!
               
               ; from (artanis websocket frame)
               received-closing-frame?
               send-websocket-closing-frame

               make-websocket-frame
               websocket-frame?
               websocket-frame-parser
               websocket-frame-head
               websocket-frame-final-fragment?
               websocket-frame-opcode
               websocket-frame-type
               websocket-frame-payload

               new-websocket-frame/client
               write-websocket-frame/client))

;; If the URL hit and haven't registered, then register it.
;; NOTE: The hook requires (req body) two parameters, so we can't pass server/client
;;       explicitly.
(::define (detect-if-connecting-websocket req server client)
  (:anno: (<request> ragnarok-server ragnarok-client) -> boolean)
  (DEBUG "detect if connecting websocket~%")
  (let ((url (request-path req))
        (port (request-port req)))
    (cond
     ((get-the-redirector-of-websocket server client)
      (DEBUG "Client `~a' has already registered websocket in `~a'" (client-ip client) url)
      #t)
     ((url-need-websocket? url)
      (cond
       ((eq? (get-conf '(server engine)) 'guile)
        (throw 'artanis-err 1006 detect-if-connecting-websocket
               "Server engine `guile' doesn't support websocket!"))
       ((not (get-conf '(server websocket)))
        (throw 'artanis-err 1006 detect-if-connecting-websocket
               "Websocket is fobbiden since server.websocket is not enabled")))
      ;; NOTE: We have to do handshake here, since we have no chance to know
      ;;       if it's not registered in http-read without twice detection.
      (DEBUG "Register `~a' to use websocket for rule `~a'" (client-ip client) url)
      (do-websocket-handshake req server client)
      #t)
     (else #f))))

;; NOTE: auth in websocket is not handled by #:auth, but should be specified
;;       in #:websocket. So the authentication will be checked in websocket-read
(define (websocket-check-auth req)

  ;; return 401 or 3xx redirection if authentication failed
  #t)

;; TODO: Register protobuf handler to ragnarok-server when server start.
(::define (websocket-read req server client)
  (:anno: (<request> ragnarok-server ragnarok-client) -> websocket-frame)
  (cond
   ((websocket-check-auth req)
    (let* ((redirector (get-the-redirector-of-websocket server client))
           (reader (redirector-reader redirector))) ; reader: bytevector -> record-type
      (read-websocket-frame reader (client-sockport client))))
   (else
    (throw 'artanis-err 401 websocket-read
           "Authentication failed: ~a" (client-ip client)))))

(::define (websocket-write type res body server client)
  (:anno: (symbol <response> ANY ragnarok-server ragnarok-client) -> ANY)
  (let* ((redirector (get-the-redirector-of-websocket server client))
         (writer (redirector-writer redirector)) ; writer: record-type -> bytevector
         (frame (new-websocket-frame/client type #t (writer body))))
    ;; TODO: Check websocket.fragment and do fragmentation
    (write-websocket-frame/client res frame)))
