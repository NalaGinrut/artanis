;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015
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
  #:use-module (artanis crypto base64)
  #:use-module (ice-9 iconv)
  #:use-module (rnrs bytevectors)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:export (websocket:key->accept))

(define *ws-magic* "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

(define-record-type websocket-server
  (make-websocket-server onopen onmessage onclose send)
  websocket-server?
  (onopen ws:onopen ws:onopen-set!)
  (onmessage ws:onmessage ws:onmessage-set!)
  (onclose ws:onclose ws:onclose-set!)
  (send ws:send ws:send-set!))

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
                   (format #f "\x7e~a" (->pack 2 len)))
                  (else (integer->char len))))
         (head (integer->char (logior #x80 opcode))))
    (format #f "~a~a~a" head length data)))    

(define (websocket:key->accept wsk)
  (let* ((realkey (string-append wsk *ws-magic*))
         (keyhash (string->sha-1 realkey))
         (keybv (list->u8vector (string->byteslist keyhash 2 16))))
    (base64-encode keybv)))

(define (websocket-handler req body)
  (let* ((headers (request-headers req))
         (key (assoc-ref headers 'sec-websocket-key))
         (acpt (websocket:key->accept key))
         (res (build-response #:code 101 #:headers `((Sec-WebSocket-Accept . ,acpt)
                                                     (Upgrade . "websocket")
                                                     (Connection . "Upgrade")))))
    (values res "")))

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
