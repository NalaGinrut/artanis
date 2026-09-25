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
  #:use-module (artanis route)
  #:use-module (artanis server server-context)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (detect-if-connecting-websocket
            websocket-state?
            websocket-state-request
            websocket-state-rule
            websocket-state-reply-type
            websocket-state-reply-type-set!
            websocket-state-authenticated?
            websocket-state-sid
            websocket-state-checked-at
            websocket-state-checked-at-set!
            websocket-authenticated?)
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
;; authenticated?: #t if the route has #:with-auth and it passed at the
;;                 handshake.
;; sid: the session the connection was authenticated with, or #f. The session
;;      is checked again periodically, see ws-recheck! in
;;      (artanis server websocket).
;; checked-at: when the session was checked last time, in seconds.
(define-record-type websocket-state
  (fields request rule (mutable reply-type) authenticated? sid
          (mutable checked-at)))

;; Is the client a WebSocket connection authenticated at its handshake?
;; LAYER-2 STOPGAP: #:with-auth uses it to skip the check for each message,
;;                  since each message goes through the route handler.
(define (websocket-authenticated? client)
  (and (ragnarok-client? client)
       (let ((state (proto-conn-state client)))
         (and (websocket-state? state)
              (websocket-state-authenticated? state)))))

;; AuthN of the handshake: run #:with-auth of the route, if any.
;; Returns (values ok? authenticated? sid).
;; The failure mode of #:with-auth (login page, redirect, ...) is for HTTP,
;; it's ignored here: a failed handshake is always 401 without body. A
;; browser can't see the status of a failed handshake anyway.
;; NOTE: Browsers send the cookies with the handshake, so the session is
;;       restored from the sid in the cookie, just like HTTP. The user logs
;;       in by HTTP first.
(define (handshake-auth req)
  (let* ((rc (new-route-context req #f))
         (with-auth (rc-oht-ref rc #:with-auth)))
    (cond
     ((not with-auth) (values #t #f #f))
     ((with-auth rc (lambda _ #f) (lambda () #t))
      ;; #:with-auth always comes with #:session. The sid is kept only if the
      ;; session is valid, a token mode may pass without session.
      (let ((session (rc-oht-ref rc #:session)))
        (values #t #t (and session
                           (session rc 'check)
                           (get-sid-from-client-cookie rc)))))
     (else (values #f #f #f)))))

;; Returns:
;;  #f               not a WebSocket route, it's a plain HTTP request.
;;  'rejected        a WebSocket route, but the handshake request is invalid
;;                   or unauthenticated. The HTTP error has been sent, the
;;                   caller must close the connection.
;;  websocket-state  the 101 response has been sent, the caller must switch
;;                   the connection to the `websocket' protocol with it.
;; NOTE: The server core is checked at start-up, see `run'.
(define (detect-if-connecting-websocket req port)
  (define (reject err)
    (reject-websocket-request req port err)
    'rejected)
  (cond
   ((not (url-need-websocket? (websocket-request-path req))) #f)
   ((websocket-request-error req) => reject)
   (else
    (call-with-values (lambda () (handshake-auth req))
      (lambda (ok? authenticated? sid)
        (cond
         ((not ok?) (reject '(401 "Authentication failed")))
         (else
          (do-websocket-handshake req port)
          (make-websocket-state
           req (find-websocket-rule (websocket-request-path req)) 'text
           authenticated? sid (current-time)))))))))
