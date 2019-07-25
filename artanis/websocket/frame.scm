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

(define-module (artanis websocket frame)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis server)
  #:use-module (artanis env)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module ((rnrs) #:select (bytevector-u8-ref
                                 bytevector-u8-set!
                                 bytevector-u64-ref
                                 bytevector-u32-ref
                                 bytevector-u16-ref
                                 put-u8
                                 put-bytevector
                                 get-bytevector-all
                                 get-bytevector-n
                                 bytevector-length
                                 uint-list->bytevector
                                 define-record-type))
  #:export (received-closing-frame?
            send-websocket-closing-frame

            make-websocket-frame
            websocket-frame?
            websocket-frame-parser
            websocket-frame-head
            websocket-frame-final-fragment?
            websocket-frame-opcode
            websocket-frame-payload
            websocket-frame-mask

            websocket-frame/client-final?
            websocket-frame/client-type
            websocket-frame/client-length
            websocket-frame/client-payload

            print-websocket-frame
            new-websocket-frame/client
            write-websocket-frame/client
            read-websocket-frame))

(define-record-type websocket-frame
  (fields
   head
   parser
   payload-length
   mask
   payload))

(define-record-type websocket-frame/client
  (fields
   final?
   type
   length
   payload))

(define (%read-bytevector port n)
  (let ((bv (get-bytevector-n port n)))
    (cond
     ((eof-object? bv)
      (throw 'artanis-err 400 %read-bytevector "Websocket get EOF from peer!"))
     ((= (bytevector-length bv) n)
      bv)
     (else
      (throw 'artanis-err 400 %read-bytevector
             "Can't read from the client successfully!")))))

;;  %x0 denotes a continuation frame
(define (is-continue-frame? opcode) (= opcode #x0))
;;  %x1 denotes a text frame
(define (is-text-frame? opcode) (= opcode #x1))
;;  %x2 denotes a binary frame
(define (is-binary-frame? opcode) (= opcode #x2))
(define (is-control-frame? opcode)
  (logand #x8 opcode))
(define (is-non-control-frame? opcode)
  (not (is-continue-frame? opcode)))
;;  %x8 denotes a connection close
(define (is-close-frame? opcode) (= opcode #x8))
;;  %x9 denotes a ping
(define (is-ping-frame? opcode) (= opcode #x9))
;;  %xA denotes a pong
(define (is-pong-frame? opcode) (= opcode #xa))
;;  %xB-F are reserved for further control frames
(define (is-reserved-frame? opcode)
  (and (> opcode #xb) (< opcode #xf)))

(define (is-masked-frame? head)
  (logtest #x8000 head))

(define (is-final-frame? head)
  (logtest #x8000 head))

(define (received-closing-frame? port)
  ;; TODO: finish it
  #t)

(define (send-websocket-closing-frame port)
  (let ((close-frame (new-websocket-frame/client 'close #t #vu8(0))))
    (write-websocket-frame/client port close-frame)))

(define *opcode-list*
  '(continuation         ; #x0
    text                 ; #x1
    binary               ; #x2
    non-control-reserved ; #x3
    non-control-reserved ; #x4
    non-control-reserved ; #x5
    non-control-reserved ; #x6
    non-control-reserved ; #x7
    close                ; #x8
    ping                 ; #x9
    pong                 ; #xA
    control-reserved     ; #xB
    control-reserved     ; #xC
    control-reserved     ; #xD
    control-reserved     ; #xE
    control-reserved))   ; #xF

(define-syntax-rule (generate-opcode type)
  (list-index *opcode-list* type))

(::define (websocket-get-head port)
  (:anno: (port) -> int)
  (bytevector-u16-ref (%read-bytevector port 2) 0 'big))

(define-syntax-rule (get-mask port)
  (%read-bytevector port 4))

(define-syntax-rule (%get-opcode head)
  (ash (logand head #x0f00) -8))

(define-syntax-rule (%get-type opcode)
  (assoc-ref *opcode-list* opcode))

(define-syntax-rule (%verify-type type)
  (cond
   ((eq? type 'non-control-reserved)
    (throw 'artanis-err 500 websocket-type
           "The opcode `#x~:@(~x~)' is reserved for non-control frame" opcode))
   ((eq? type 'control-reserved)
    (throw 'artanis-err 500 websocket-type
           "The opcode `#x~:@(~x~)' is reserved for control frame" opcode))
   (else type)))

(::define (websocket-frame-final-fragment? frame)
  (:anno: (websocket-frame) -> boolean)
  (is-final-frame? (logand #xff00 (websocket-frame-head frame))))

(::define (websocket-frame-opcode frame)
  (:anno: (websocket-frame) -> int)
  (%get-opcode (ash (logand #xff00 (websocket-frame-head frame)) -8)))

(::define (websocket-frame-fin frame)
  (:anno: (websocket-frame) -> int)
  (if (websocket-frame-final-fragment? frame)
      #x80
      #x00))

;; NOTE: The frame will not be decoded or parsed into a record-type, on the contrary,
;;       it'll be kept as a binary frame read from client, and use bitwise operations for
;;       fetching the fields. This kind of `lazy' design will save much time on parsing
;;       unused fields each time, and eaiser for redirecting without any serialization.
;;       If users want to get certain field, Artanis provides APIs for fetching them. Users
;;       can decide how to parse the frames for efficiency.
(define (read-websocket-frame parser port)
  (define-syntax-rule (get-len payload-len port control-frame?)
    (cond
     ((< payload-len 126)
      ;; Yes, it's redundant, but I never trust the data from client
      payload-len)
     ((= payload-len 126)
      (bytevector-u16-ref (%read-bytevector port 2) 0 'big))
     ((= payload-len 127)
      (if (not control-frame?)
          (let ((real-len (bytevector-u64-ref (%read-bytevector port 8) 0 'big)))
            (if (<= real-len (get-conf '(websocket maxpayload)))
                real-len
                (throw 'artanis-err 1009 read-websocket-frame
                       "Too big message received! `~a' > `~a'"
                       payload-len (get-conf '(websocket maxpayload)))))
          (throw 'artanis-err 1007 read-websocket-frame
                 "Invalid websocket frame, the control frame can't be segmented!")))
     (else (throw 'artanis-err 500 read-websocket-frame
                  "Invalid payload-len `~a'!" payload-len))))
  (define (detect-payload-offset mask payload-len)
    (+ 1 ; head1
       1 ; head2
       (if (< payload-len 126) 0 (if (= payload-len 126) 2 8)) ; real-len
       (if mask 4 0))) ; mask-lenre
  (define (decode-with-mask! payload len mask)
    (let lp ((i 0))
      (DEBUG "payload: ~a~%len: ~a~%mask: ~a~%" payload len mask)
      (cond
       ((>= i len) payload)
       (else
        (let ((masked (logxor (u8vector-ref payload i)
                              (u8vector-ref mask (modulo i 4)))))
          (u8vector-set! payload i masked)
          (lp (1+ i)))))))
  (define (cook-payload mask payload real-len)
    (if mask
        (decode-with-mask! payload real-len mask)
        payload))
  (define* (read-and-verify-payload port size)
    (let ((payload (get-bytevector-n port size)))
      (cond
       ((eof-object? payload)
        (throw 'artanis-err 400 read-and-verify-payload "Websocket get EOF from peer!"))
       ((= (bytevector-length payload) size)
        (DEBUG "Websocket get payload (~a bytes) sucessfully!" size)
        payload)
       (else
        (throw 'artanis-err 400 read-and-verify-payload
               "Websocket payload in wrong size ~a bytes! Expect ~a bytes!"
               (bytevector-length payload) size)))))
  ;; NOTE: We have to read the header first since we need to check the payload length for
  ;;       security isssue.
  ;; FIXME: Maybe we have to drop the whole-body-redirecting method, since the socket
  ;;        demands a size to get all body. If we use get-bytevector-all, then it's stuck.
  ;;        Even if we are in non-blocking, so sad.
  (let* ((head (websocket-get-head port))
         (control-frame? (is-control-frame? (%get-opcode head)))
         (payload-len (logand #x7f head))
         (real-len (get-len payload-len port control-frame?))
         (mask (is-masked-frame? head))
         (mask-array (and mask (get-mask port)))
         (payload (read-and-verify-payload port real-len))
         (cooked-payload (cook-payload mask-array payload real-len)))
    (make-websocket-frame head parser real-len mask-array cooked-payload)))

(::define (generate-head1 final? type)
  (:anno: (boolean symbol) -> int)
  (logior (if final? #x80 #x00)
          (generate-opcode type)))

(define 16bit-size (ash 1 16))
(define 64bit-size (ash 1 64))

;; NOTE: According to RFC-6455, A server MUST NOT mask any frames that it sends to
;;       the client. (From 5.1 Overview).
;; NOTE: If the length is larger than 16bit, then just speicify it to 127 then deal with
;;       the actual length in later extended length field.
(::define (generate-head2 len)
  (:anno: (+int) -> +int)
  (cond
   ((< len 126) len) ; payload length less than 126 bytes
   ((< len 16bit-size) 126) ; extended 16bit payload length
   ((< len 64bit-size) 127) ; extended 64bit payload length
   (else (throw 'artanis-err 500 generate-head2
                "The payload size `~a' excceded 64bit!" len))))

(::define (write-websocket-frame/client port frame)
  (:anno: (port websocket-frame/client) -> ANY)
  (define (write-payload-size port len)
    (cond
     ((< len 16bit-size) (put-bytevector port (uint-list->bytevector (list len) 'big 2)))
     ((< len 64bit-size) (put-bytevector port (uint-list->bytevector (list len) 'big 8)))
     (else (throw 'artanis-err 1009 write-payload-size
                  "The payload size `~a' exceeded 64bit!" len))))
  (when (port-closed? port)
    (throw 'artanis-err 1001 write-websocket-frame/client
           "The client port `~a' was closed!" port))
  (let* ((final? (websocket-frame/client-final? frame))
         (type (websocket-frame/client-type frame))
         (len (websocket-frame/client-length frame))
         (payload (websocket-frame/client-payload frame))
         (head1 (generate-head1 final? type))
         (head2 (generate-head2 len)))
    (put-u8 port head1)
    (put-u8 port head2)
    (when (> len 125)
      (write-payload-size port len))
    (put-bytevector port (websocket-frame/client-payload frame))
    (force-output port)))

;; NOTE: A better design is not to split then store fields to record-type,
;;       we just need to parse the frame and store the offset.
;; NOTE: It's better to delay preprocessing to the time the payload is needed.
;;       And store the preprocessor to the frame (record-type).
;; NOTE: Return bytevector
(::define (new-websocket-frame/client type final? payload)
  (:anno: (symbol boolean bv) -> websocket-frame/client)
  (define-syntax-rule (detect-type t)
    (case t
      ((proxy binary) 'binary)
      ((ping pong text close) t)
      (else 'binary))) ; the unknown redirector type should always be `binary' type
  (let ((payload-len (bytevector-length payload))
        (real-type (detect-type type)))
    (make-websocket-frame/client
     final?
     real-type
     payload-len
     payload)))

(::define (print-websocket-frame frame)
  (:anno: (websocket-frame) -> ANY)
  (call-with-output-string
   (lambda (port)
     (let* ((head (websocket-frame-head frame))
            (opcode (%get-opcode head))
            (payload (websocket-frame-payload frame))
            (size (websocket-frame-payload-length frame))
            (mask (websocket-frame-mask frame)))
       (format port "<websocket-frame:~%")
       (format port "~10thead: ~a~%" head)
       (format port "~10tfinal?: ~a~%" (is-final-frame? head))
       (format port "~10ttype: ~a~%" (list-ref *opcode-list* opcode))
       (format port "~10tpayload-size: ~a~%" size)
       (format port "~10tpayload: ~a>" payload)))))
