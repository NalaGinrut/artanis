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
  #:use-module (artanis server server-context)
  #:use-module (artanis websocket handshake)
  #:export (this-rule-enabled-websocket!
            detect-if-connecting-websocket
            websocket-read
            websocket-write)
  #:re-export (do-websocket-handshake
               closing-websocket-handshake))

;; Op-code Meaning
(define MSG_CONG #x0) ;	Message continuation [continuation]
(define TXT_MSG  #x1) ; Text message [non-control]
(define BIN_MSG  #x2) ; Binary message [non-control]
(define CONN_CLOSE #x8) ; Connection Close [control]
(define PING #x9) ; Ping [control]
(define PONG #xA) ; Pong [control]

(define (->pack size data)
  (let ((bv (make-bytevector size)))
    (bytevector-u8-set! bv (1- size) data)
    (utf8->string bv)))

(define (make-hybi00-frame data)
  "Make a HyBi-00 frame from some data.
   This function does exactly zero checks to make sure that the data is safe
   and valid text without any 0xff bytes."
  (format #f "\x00~a\xff" data))

(define* (make-hybi07-frame data #:key (opcode #x01))
  "Make a HyBi-07 frame.
   This function always creates unmasked frames, and attempts to use the
   smallest possible lengths."
  (let* ((len (string-length data))
         (length (cond
                  ((> len #xffff)
                   (format #f "\x7f~a" (->pack 8 len)))
                  ((> len #x7d)
                   ;; NOTE: `\x7e' happens to be `~' !!!
                   (format #f "~\x7e~a" (->pack 2 len)))
                  (else (integer->char len))))
         (head (integer->char (logior #x80 opcode))))
    (format #f "~a~a~a" head length data)))

;; frame header "\x81" (1000 0001 in binary)
(define *ws-frame-header* #\201)

;; TODO: we need more efficient frame spliter
(define (->frames frame)
  (let ((ll (regexp-split "(\x81[^\x81]+)" frame)))
    (filter (lambda (s) (not (string-null? s))) ll)))

(define (websocket:decode-body-string body)
  (let* ((bv (string->bytevector body "iso8859-1"))
         (bv-len (bytevector-length bv))
         (len (logand (bytevector-u8-ref bv 1) 127))
         (index-first-mask (cond
                            ((= len 126) 4)
                            ((= len 127) 10)
                            (else 2)))
         (index-first-data-byte (+ index-first-mask 4))
         (masks (bv-slice bv index-first-mask : index-first-data-byte)))
    (let lp((i index-first-data-byte) (j 0) (ret '()))
      (cond
       ((>= i bv-len) (apply string (reverse ret)))
       (else 
        (let ((c (integer->char 
                  (logxor 
                   (bytevector-u8-ref bv i) (bytevector-u8-ref masks (modulo j 4))))))
          (lp (1+ i) (1+ j) (cons c ret))))))))

(define *rules-with-websocket* '())

(define (this-rule-enabled-websocket! rule protocol)
  (set! *rules-with-websocket*
        (cons (cons (string->irregex rule) protocol) *rules-with-websocket*)))

(define (get-websocket-protocol rule)
  (any (lambda (pp)
         (irregex-search (car pp) rule))
       *rules-with-websocket*))

;; If the URL hit and haven't registered, then register it.
;; NOTE: The hook requires (req body) two parameters, so we can't pass server/client
;;       explicitly.
(::define (detect-if-connecting-websocket req)
  (:anno: (<request>) -> boolean)
  (define (url-need-websocket? url)
    (any (lambda (rule) (irregex-search rule url)) *rules-with-websocket*))
  (DEBUG "detect if connecting websocket~%")
  (let ((server (current-server))
        (client (current-client))
        (url (request-path req))
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
      ;; If the URL need websocket, and if it's not registered, then register it.
      ;; TODO: If the websocket was specified a protocol, then use the registered
      ;;       reader/writer to replace `identity'.
      (register-redirector! server client identity identity 'websocket port)
      (do-websocket-handshake req)
      (DEBUG "Register `~a' to use websocket for rule `~a'" (client-ip client) url)
      #t)
     (else #f))))

;; NOTE: auth in websocket is not handled by #:auth, but should be specified
;;       in #:websocket. So the authentication will be checked in websocket-read
(define (websocket-check-auth req)

  ;; return 401 or 3xx redirection if authentication failed
  #t)

;; TODO: Register protobuf handler to ragnarok-server when server start.
(::define (websocket-read req server client)
  (:anno: (<request> ragnaraok-server ragnarok-client) -> websocket-frame)
  (cond
   ((websocket-check-auth req)
    (let* ((redirector (get-the-redirector-of-websocket server client))
           (reader (redirector-reader redirector))) ; reader: bytevector -> record-type
      (read-websocket-frame reader (client-sockport client))))
   (else
    (throw 'artanis-err 401 websocket-read
           "Authentication failed: ~a" auth))))

(::define (websocket-write res body server client)
  (:anno: (<response> ANY ragnarok-server ragnarok-client) -> ANY)
  (let* ((redirector (get-the-redirector-of-websocket server client))
         (writer (redirector-writer redirector)) ; writer: record-type -> bytevector
         (frame (new-websocket-frame/client #t (writer body))))
    (write-response-body (write-response res) frame)))
