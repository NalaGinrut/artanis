;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2017,2018,2019
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

(define-module (artanis websocket handshake)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis config)
  #:use-module (artanis crypto base64)
  #:use-module (artanis server server-context)
  #:use-module (artanis server scheduler)
  #:use-module (artanis irregex)
  #:use-module (artanis websocket frame)
  #:use-module (artanis websocket protocols)
  #:use-module (ice-9 iconv)
  #:use-module (rnrs bytevectors)
  #:use-module ((rnrs) #:select (define-record-type))
  #:use-module ((srfi srfi-1) #:select (any))
  #:export (do-websocket-handshake
            closing-websocket-handshake
            gen-accept-key
            valid-ws-request?
            this-rule-enabled-websocket!
            this-rule-enabled-inexclusive-websocket!
            url-need-websocket?
            url-need-inexclusive-websocket?))

(define *ws-magic* "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

(define *rules-with-websocket* '())
(define *rules-with-inexclusive-websocket* '())

(define (url-need-websocket? url)
  (DEBUG "url-need-websocket? ~a~%" url)
  (any (lambda (rule)
         (irregex-match (car rule) url)) *rules-with-websocket*))

(define (url-need-inexclusive-websocket? url)
  (DEBUG "url-need-inexclusive-websocket? ~a~%" url)
  (any
   (lambda (rule)
     (irregex-match (car rule) url))
   *rules-with-inexclusive-websocket*))

(define (this-rule-enabled-websocket! rule protocol)
  (DEBUG "this-rule-enabled-websocket! ~a~%" rule)
  (set! *rules-with-websocket*
        (cons (cons (string->irregex rule) protocol) *rules-with-websocket*)))

(define (this-rule-enabled-inexclusive-websocket! rule protocol)
  (DEBUG "this-rule-enabled-inexclusive-websocket! ~a~%" rule)
  (set! *rules-with-inexclusive-websocket*
        (cons (cons (string->irregex rule) protocol) *rules-with-inexclusive-websocket*)))

(define (get-websocket-protocol rule)
  (define (check pp)
    (and (irregex-search (car pp) rule) (cdr pp)))
  (DEBUG "get-websocket-protocol: ~a~%" rule)
  (or (any check *rules-with-websocket*)
      (any check *rules-with-inexclusive-websocket*)))

(define (gen-accept-key key)
  (let* ((realkey (string-append key *ws-magic*))
         (keyhash (string->sha-1 realkey))
         (keybv (list->u8vector (string->byteslist keyhash 2 16))))
    (base64-encode keybv)))

(define (validate-websocket-request req client)
  (define (not-proper-websocket-version? version)
    (not (string=? version "13")))
  (define (the-origin-is-not-acceptable? origin)
    ;; TODO: check origin
    #f)
  (let* ((headers (request-headers req))
         (upgrade (car (assoc-ref headers 'upgrade)))
         (connection (assoc-ref headers 'connection))
         (version (assoc-ref headers 'sec-websocket-version))
         (origin (assoc-ref headers 'origin)))
    (cond
     ((not (memq 'upgrade connection))
      (throw 'artanis-err 426 validate-websocket-request
             "Invalid connection `~a' request from ~a, expect 'upgrade!"
             connection (client-ip client)))
     ((not (string=? upgrade "websocket"))
      (throw 'artanis-err 426 validate-websocket-request
             "Invalid protocol `~a' request from ~a, expect \"websocket\"!"
             upgrade (client-ip client)))
     ((not-proper-websocket-version? version)
      (throw 'artanis-err 426 validate-websocket-request
             "Invalid websocket version `~a' from client ~a"
             version (client-ip client)))
     ((the-origin-is-not-acceptable? origin)
      (throw 'artanis-err 403 validate-websocket-request
             "Unacceptable origin `~a' from websocket client ~a"
             origin (client-ip client)))
     (else #t))))

(define (confirm-available-protocols client path request-protocols)
  (let ((protocol (get-websocket-protocol path)))
    (cond
     ((memq protocol request-protocols) protocol)
     (else
      (throw 'artanis-err 1002 validate-websocket-request
             "Websocket subprotocol `~a' is unacceptable from client ~a"
             protocol (client-ip client))))))

(define (run-after-websocket-handshake-hooks req client)
  (run-hook *after-websocket-handshake-hook* req client))

;; NOTE: Although we can get `port' from `request', we still need `client' for IP address.
(define (do-websocket-handshake req server client)
  (define-syntax-rule (->protocols pl)
    (if pl ; FIXME: how to deal with no-protocol-specified situation
        (map (lambda (p) (string->symbol (string-trim-both p)))
             (string-split pl #\,))
        '(echo)))
  (validate-websocket-request req client)
  (let* ((headers (request-headers req))
         (path (request-path req))
         (request-protocols (->protocols (assoc-ref headers 'sec-websocket-protocol)))
         (proto (confirm-available-protocols client path request-protocols))
         (port (request-port req))
         (key (assoc-ref headers 'sec-websocket-key))
         (accept-key (gen-accept-key key))
         (origin (or (assoc-ref headers 'origin) "unknown client"))
         (res (build-response #:code 101 #:headers `((Sec-WebSocket-Accept . ,accept-key)
                                                     ;;(Sec-WebSocket-Protocol . ,proto)
                                                     ;;(Sec-WebSocket-Extensions . "permessage-deflate")
                                                     (Sec-Websocket-Version . "13")
                                                     (Upgrade . "websocket")
                                                     (Connection . "Upgrade")))))
    (format (artanis-current-output)
            "[WebSocket] Handshake successfully from ~a~a~%"
            origin (request-path req))
    (format (artanis-current-output)
            "[Websocket] Initializing `~a' protocol for Websocket ..."
            proto)
    (DEBUG "run-after-websocket-handshake-hooks~%")
    (run-after-websocket-handshake-hooks req client)
    (DEBUG "register-websocket-protocol!~%")
    (register-websocket-protocol! server client proto port)
    (format (artanis-current-output) " done~%")
    (write-response res port)
    (force-output port)
    (DEBUG "Handshake done!~%")))

;; NOTE: The actual closing operation should be in http-close
;; NOTE: If peer-shutdown? is #t, then it means the websocket reader got closing-frame,
;;       So we just remove redirector then close the connection.
;; NOTE: If the shutdown is not required by client, then we should send closing frame, then
;;       waiting for the closing-frame from the client, then close the connection.
;; TODO: Finish all other exceptions which need close operation.
(define (closing-websocket-handshake server client peer-shutdown?)
  (cond
   (peer-shutdown?
    (format (artanis-current-output)
            "[Websocket] Closed by peer `~a'.~%" (client-ip client)))
   (else
    (format (artanis-current-output)
            "[Websocket] Client `~a' was closed by server.~%" (client-ip client))
    (when (not (client-sockport client))
      (send-websocket-closing-frame (client-sockport client)))
    (if (received-closing-frame? (client-sockport client))
        (format (artanis-current-output)
                "[Websocket] Closing `~a' normally.~%" (client-ip client))
        (throw 'artanis-err 1008 closing-websocket-handshake
               "The client didn't conform RFC-6544 to send closing frame~%")))))
