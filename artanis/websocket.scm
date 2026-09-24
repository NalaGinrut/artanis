;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2016,2017,2018
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

(define-module (artanis websocket)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis env)
  #:use-module (artanis irregex)
  #:use-module (artanis server server-context)
  #:use-module (artanis websocket handshake)
  #:use-module (artanis websocket frame)
  #:use-module (artanis websocket named-pipe)
  #:use-module (ice-9 format)
  #:use-module ((rnrs)
                #:select (make-bytevector
                          bytevector-u8-set!
                          bytevector?
                          utf8->string
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
               this-rule-enabled-inexclusive-websocket!
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
               named-pipe-subscribe
               detect-pipe-name
               get-named-pipe
               new-named-pipe
               named-pipe-clients named-pipe-clients-set!
               named-pipe-task-queue-set!))

;; If the URL hit and haven't registered, then register it.
;; NOTE: The hook requires (req body) two parameters, so we can't pass server/client
;;       explicitly.
(::define (detect-if-connecting-websocket req server client)
  (:anno: (<request> ragnarok-server ragnarok-client) -> boolean)
  (let ((url (request-path req))
        (port (request-port req)))
    (cond
     ((get-the-redirector-of-websocket server client)
      (DEBUG "Client `~a' has already registered websocket in `~a'~%"
             (client-ip client) url)
      'registered)
     ((or (url-need-websocket? url)
          (url-need-inexclusive-websocket? url))
      (cond
       ((is-guile-compatible-server-core? (get-conf '(server engine)))
        (throw 'artanis-err 1006 detect-if-connecting-websocket
               "Server engine `guile' doesn't support websocket!"))
       ((not (get-conf '(server websocket)))
        (throw 'artanis-err 1006 detect-if-connecting-websocket
               "Websocket is fobbiden since server.websocket is not enabled")))
      ;; NOTE: We have to do handshake here, since we have no chance to know
      ;;       if it's not registered in http-read without twice detection.
      (DEBUG "Register `~a' to use websocket for rule `~a'~%" (client-ip client) url)
      (task-timeout-set! (current-task) (get-conf '(websocket timeout)))
      (do-websocket-handshake req server client)
      'handshake)
     (else #f))))

;; NOTE: auth in websocket is not handled by #:auth, but should be specified
;;       in #:websocket. So the authentication will be checked in websocket-read
(define (websocket-check-auth req)

  ;; return 401 or 3xx redirection if authentication failed
  #t)

;; NOTE: The protocol reader of the redirector is not applied here, it
;;       belongs to the connection layer. The body is the websocket-frame of
;;       a whole message, or the received close frame (opcode 8).
;; NOTE: Until the connection layer exists, this is the boundary between the
;;       frame layer and Ragnarok: 'websocket-err must not leak to Ragnarok's
;;       outermost catch, which only logs it without closing the task.
(define (websocket-read req server client)
  (DEBUG "Enter websocket-read~%")
  (cond
   ((websocket-check-auth req)
    (let ((port (client-sockport client)))
      (catch 'websocket-err
        (lambda ()
          (read-websocket-message port))
        (lambda (k code thrower fmt . args)
          (let ((reason (apply format #f fmt args)))
            (format (artanis-current-output)
                    "[Websocket] Client `~a' failed with close code ~a: ~a~%"
                    (client-ip client) code reason)
            (cond
             ((= code 1006)
              ;; The peer is gone, nothing can be written anymore.
              (websocket-output-close! port))
             (else
              (catch #t
                (lambda () (send-websocket-close port #:code code))
                (lambda _ (websocket-output-close! port)))))
            ;; Rethrow with a valid HTTP status so that the task is closed.
            ;; Anything written for this error is dropped since the output
            ;; is closed now.
            (throw 'artanis-err (if (= code 1011) 500 400) thrower
                   "Websocket close code ~a: ~a" code reason))))))
   (else
    (throw 'artanis-err 401 websocket-read
           "Authentication failed: ~a" (client-ip client)))))

(define (websocket-write type body server client)
  (DEBUG "Enter websocket-write~%")
  (let* ((redirector (get-the-redirector-of-websocket server client))
         (writer (redirector-writer redirector)) ; writer: ANY -> bytevector
         (port (client-sockport client)))
    (write-websocket-message port 'text (writer body))))
