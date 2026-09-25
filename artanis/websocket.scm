;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013-2026
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

;; The entry of WebSocket from HTTP: http-read calls
;; detect-if-connecting-websocket for each request. After a successful
;; handshake, http-read switches the connection to the `websocket'
;; ragnarok-protocol, see (artanis server websocket), with a websocket-state
;; as its per-connection state.

(define-module (artanis websocket)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis websocket handshake)
  #:use-module (artanis websocket frame)
  #:use-module (artanis websocket named-pipe)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (detect-if-connecting-websocket
            new-websocket-state
            websocket-state?
            websocket-state-request
            websocket-state-rule
            websocket-state-reply-type
            websocket-state-reply-type-set!)
  #:re-export (; from (artanis websocket handshake)
               closing-websocket-handshake
               websocket-rule-add!
               websocket-rule-timeout-set!
               websocket-rules-defined?
               websocket-origins-init!
               websocket-request-path
               websocket-rule-timeout
               url-need-websocket?
               url-need-inexclusive-websocket?

               ;; from (artanis websocket frame)
               make-websocket-frame
               websocket-frame?
               websocket-frame-final?
               websocket-frame-opcode
               websocket-frame-payload
               websocket-frame-type
               print-websocket-frame
               write-websocket-message
               send-websocket-close
               websocket-output-closed?

               ;; from (artanis websocket named-pipe)
               register-websocket-pipe!
               pair-name-to-client!
               send-to-websocket-named-pipe
               detect-pipe-name
               get-named-pipe
               new-named-pipe
               named-pipe-clients named-pipe-clients-set!
               named-pipe-task-queue-set!))

;; Per-connection state of the `websocket' protocol, kept in the proto table.
;; request: the handshake request. The route handler is called with it for
;;          each message (layer-2 stopgap, see ws-read).
;; rule: the websocket-rule of the route.
;; reply-type: 'text or 'binary, the type of the last received message, which
;;             is also the type of the reply (layer-2 stopgap, see ws-write).
(define-record-type websocket-state
  (fields request rule (mutable reply-type)))

(define (new-websocket-state req)
  (make-websocket-state
   req (find-websocket-rule (websocket-request-path req)) 'text))

;; Returns:
;;  #f         not a WebSocket route, it's a plain HTTP request.
;;  'rejected  a WebSocket route, but the handshake request is invalid. The
;;             HTTP error has been sent, the caller must close the connection.
;;  'handshake the 101 response has been sent, the caller must switch the
;;             connection to the `websocket' protocol.
;; NOTE: The server core is checked at start-up, see `run'.
(define (detect-if-connecting-websocket req port)
  (cond
   ((not (url-need-websocket? (websocket-request-path req))) #f)
   ((websocket-request-error req)
    => (lambda (err)
         (reject-websocket-request req port err)
         'rejected))
   (else
    (do-websocket-handshake req port)
    'handshake)))
