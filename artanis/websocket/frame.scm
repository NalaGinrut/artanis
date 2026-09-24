;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2017,2018,2019,2026
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

;; RFC 6455 frame layer: frame parsing and validation, message assembly,
;; control frames, close frames, frame writing and outbound fragmentation.
;;
;; Conventions:
;; 1. Frame-level errors are thrown as
;;      (throw 'websocket-err close-code 'thrower-name fmt . args)
;;    where close-code is a WebSocket close code (1002, 1007, 1008, 1009...),
;;    never an HTTP status. 1006 means the peer went away; it's internal only
;;    and must never be sent on the wire. 1011 is used for misuse of this API.
;; 2. Payloads are bytevectors. The wire is binary, so port encoding never
;;    matters here: we only use get-u8/get-bytevector-n/put-u8/put-bytevector.
;;    Text is UTF-8 on the wire; strings are encoded by upper layers.
;; 3. Zero-copy: outgoing payloads are written as (bv start count) slices of
;;    the caller's bytevector, fragments are index ranges, nothing is copied
;;    in this layer. The caller must not modify the bytevector until the write
;;    returns (the synchronous write is the release point).
;; 4. All checks on a frame header are done before its payload is allocated.

(define-module (artanis websocket frame)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (ice-9 format)
  #:use-module ((rnrs) #:select (bytevector-u8-ref
                                 bytevector-u8-set!
                                 bytevector-u16-ref
                                 bytevector-u16-set!
                                 bytevector-length
                                 bytevector-copy!
                                 make-bytevector
                                 string->utf8
                                 put-u8
                                 get-u8
                                 put-bytevector
                                 get-bytevector-n
                                 define-record-type))
  #:export (make-websocket-frame
            websocket-frame?
            websocket-frame-final?
            websocket-frame-opcode
            websocket-frame-payload
            websocket-frame-type
            print-websocket-frame

            read-websocket-frame
            read-websocket-message
            default-control-handler

            write-websocket-frame
            make-fragment-writer
            write-websocket-message
            send-websocket-close

            websocket-output-closed?
            websocket-output-close!
            received-closing-frame?))

;; payload is always the unmasked bytevector.
(define-record-type websocket-frame
  (fields final? opcode payload))

;; ---------------------------------------------------------------------------
;; Opcodes

(define *type->opcode*
  '((continuation . #x0)
    (text . #x1)
    (binary . #x2)
    (close . #x8)
    (ping . #x9)
    (pong . #xa)))

(define (type->opcode type)
  (or (assq-ref *type->opcode* type)
      (throw 'websocket-err 1011 'type->opcode
             "Invalid frame type `~a'" type)))

(define (opcode->type opcode)
  (let lp ((lst *type->opcode*))
    (cond
     ((null? lst) 'reserved)
     ((= (cdar lst) opcode) (caar lst))
     (else (lp (cdr lst))))))

(define (websocket-frame-type frame)
  (opcode->type (websocket-frame-opcode frame)))

(define (reserved-opcode? op)
  (or (<= #x3 op #x7) (<= #xb op #xf)))

(define (control-opcode? op) (logtest #x8 op))

(define (close-opcode? op) (= op #x8))

;; ---------------------------------------------------------------------------
;; UTF-8 validation
;;
;; Byte-level DFA, no allocation, and the state survives across fragments so
;; that an invalid sequence fails on the fragment containing it.
;; States: 0 accept; 1-3 expect that many continuation bytes (#x80-#xbf);
;; 4 after #xe0 (#xa0-#xbf); 5 after #xed (#x80-#x9f, no surrogates);
;; 6 after #xf0 (#x90-#xbf); 7 after #xf4 (#x80-#x8f, <= U+10FFFF).
;; #f means invalid.

(define (utf8-step state b)
  (case state
    ((0) (cond
          ((< b #x80) 0)
          ((< b #xc2) #f)
          ((< b #xe0) 1)
          ((= b #xe0) 4)
          ((= b #xed) 5)
          ((< b #xf0) 2)
          ((= b #xf0) 6)
          ((< b #xf4) 3)
          ((= b #xf4) 7)
          (else #f)))
    ((1 2 3) (and (<= #x80 b #xbf) (1- state)))
    ((4) (and (<= #xa0 b #xbf) 1))
    ((5) (and (<= #x80 b #x9f) 1))
    ((6) (and (<= #x90 b #xbf) 2))
    ((7) (and (<= #x80 b #x8f) 2))
    (else #f)))

;; Scan bv[start, end) from state, return the new state or #f.
(define (utf8-scan bv start end state)
  (let lp ((i start) (s state))
    (cond
     ((not s) #f)
     ((= i end) s)
     (else (lp (1+ i) (utf8-step s (bytevector-u8-ref bv i)))))))

;; ---------------------------------------------------------------------------
;; Close codes

(define (valid-close-code? code)
  (or (<= 1000 code 1003)
      (<= 1007 code 1011)
      (<= 3000 code 4999)))

;; 1005/1006/1015 are reserved for local use and must never be sent;
;; valid-close-code? already excludes them.
(define sendable-close-code? valid-close-code?)

(define (make-close-payload code reason-bv)
  (let* ((n (bytevector-length reason-bv))
         (bv (make-bytevector (+ 2 n))))
    (bytevector-u16-set! bv 0 code 'big)
    (bytevector-copy! reason-bv 0 bv 2 n)
    bv))

(define *close-reasons*
  '((1000 . "Normal Closure")
    (1001 . "Going Away")
    (1002 . "Protocol Error")
    (1003 . "Unsupported Data")
    (1007 . "Invalid Frame Payload Data")
    (1008 . "Policy Violation")
    (1009 . "Message Too Big")
    (1010 . "Mandatory Extension")
    (1011 . "Internal Error")))

;; Built once at load time; they're only ever read.
(define *close-payloads*
  (map (lambda (p)
         (cons (car p) (make-close-payload (car p) (string->utf8 (cdr p)))))
       *close-reasons*))

;; Validate a received close payload.
(define (check-close-payload payload)
  (let ((len (bytevector-length payload)))
    (cond
     ((zero? len) #t)
     ((= len 1)
      (throw 'websocket-err 1002 'check-close-payload
             "Close frame payload of 1 byte"))
     (else
      (let ((code (bytevector-u16-ref payload 0 'big)))
        (unless (valid-close-code? code)
          (throw 'websocket-err 1002 'check-close-payload
                 "Invalid close code ~a" code))
        (unless (eqv? 0 (utf8-scan payload 2 len 0))
          (throw 'websocket-err 1007 'check-close-payload
                 "Close reason is not valid UTF-8")))))))

;; ---------------------------------------------------------------------------
;; Output side state
;;
;; Once a close frame was sent (or the peer is known to be gone), nothing may
;; be written on that connection anymore. Keyed by the port object itself: a
;; reused fd gets a new port, and the weak key lets a dead port be collected.
;; Only touched by the single-threaded server core, so no lock is needed.

(define *closed-outputs* (make-weak-key-hash-table))

(define (websocket-output-closed? port)
  (hashq-ref *closed-outputs* port #f))

(define (websocket-output-close! port)
  (hashq-set! *closed-outputs* port #t))

;; TODO: waiting for the peer's closing frame belongs to the connection layer.
(define (received-closing-frame? port)
  #t)

;; ---------------------------------------------------------------------------
;; Reading

(define (read-u8 port thrower)
  (let ((b (get-u8 port)))
    (if (eof-object? b)
        (throw 'websocket-err 1006 thrower
               "Peer closed the connection in the middle of a frame")
        b)))

(define (read-uint port n thrower)
  (let lp ((i 0) (v 0))
    (if (= i n)
        v
        (lp (1+ i) (logior (ash v 8) (read-u8 port thrower))))))

;; Short read only happens at EOF: in edge mode Ragnarok installs suspendable
;; ports with async-read-waiter (break-task), so straight-line reads are fine.
(define (read-payload port len)
  (if (zero? len)
      (make-bytevector 0)
      (let ((bv (get-bytevector-n port len)))
        (if (or (eof-object? bv) (< (bytevector-length bv) len))
            (throw 'websocket-err 1006 'read-payload
                   "Peer closed the connection in the middle of a frame")
            bv))))

;; TODO: xor 4 bytes at a time.
(define (unmask! bv m0 m1 m2 m3)
  (let ((len (bytevector-length bv)))
    (let lp ((i 0))
      (when (< i len)
        (bytevector-u8-set!
         bv i
         (logxor (bytevector-u8-ref bv i)
                 (case (logand i 3) ((0) m0) ((1) m1) ((2) m2) (else m3))))
        (lp (1+ i))))
    bv))

;; Read one client frame.
;; data-limit: max payload of a data frame (control frames are capped at 125
;;             bytes by the protocol and ignore it).
;; min-fragment: min payload of a non-final data frame, 0 disables the check.
(define* (read-websocket-frame port data-limit #:optional (min-fragment 0))
  (let ((b0 (let ((b (get-u8 port)))
              (if (eof-object? b)
                  (throw 'websocket-err 1006 'read-websocket-frame
                         "Peer closed the connection")
                  b))))
    (let* ((b1 (read-u8 port 'read-websocket-frame))
           (final? (logtest #x80 b0))
           (opcode (logand #x0f b0))
           (len7 (logand #x7f b1)))
      (unless (zero? (logand #x70 b0))
        (throw 'websocket-err 1002 'read-websocket-frame
               "RSV bits set without negotiated extension"))
      (when (reserved-opcode? opcode)
        (throw 'websocket-err 1002 'read-websocket-frame
               "Reserved opcode #x~x" opcode))
      (unless (logtest #x80 b1)
        (throw 'websocket-err 1002 'read-websocket-frame
               "Client frame is not masked"))
      (when (control-opcode? opcode)
        (unless final?
          (throw 'websocket-err 1002 'read-websocket-frame
                 "Fragmented control frame"))
        (when (> len7 125)
          (throw 'websocket-err 1002 'read-websocket-frame
                 "Control frame payload exceeds 125 bytes")))
      (let ((len (case len7
                   ((126) (read-uint port 2 'read-websocket-frame))
                   ((127) (let ((n (read-uint port 8 'read-websocket-frame)))
                            (when (logbit? 63 n)
                              (throw 'websocket-err 1002 'read-websocket-frame
                                     "64-bit payload length has MSB set"))
                            n))
                   (else len7))))
        (unless (control-opcode? opcode)
          (when (> len data-limit)
            ;; data-limit may be the frame limit or what is left of the
            ;; message limit, so the text names neither.
            (throw 'websocket-err 1009 'read-websocket-frame
                   "Payload too large: ~a > ~a" len data-limit))
          (when (and (not final?) (< len min-fragment))
            (throw 'websocket-err 1008 'read-websocket-frame
                   "Fragment too small: ~a < ~a" len min-fragment)))
        (let* ((m0 (read-u8 port 'read-websocket-frame))
               (m1 (read-u8 port 'read-websocket-frame))
               (m2 (read-u8 port 'read-websocket-frame))
               (m3 (read-u8 port 'read-websocket-frame))
               (payload (read-payload port len)))
          (make-websocket-frame final? opcode (unmask! payload m0 m1 m2 m3)))))))

;; Default reaction to control frames: pong with the ping payload, ignore
;; pongs, echo the status code of a close. The replies reuse the received
;; payload, nothing is copied.
;; NOTE: The connection layer should replace it with a handler that goes
;;       through the connection's single writer, so that a reply can never be
;;       interleaved inside an outgoing frame.
(define (default-control-handler port)
  (lambda (frame)
    (let ((payload (websocket-frame-payload frame)))
      (case (websocket-frame-opcode frame)
        ((#x9) (write-websocket-frame port #t 'pong payload))
        ((#xa) #t)
        ((#x8) (if (zero? (bytevector-length payload))
                   (write-websocket-frame port #t 'close payload)
                   (write-websocket-frame port #t 'close payload 0 2)))
        (else #t)))))

;; Read one complete message.
;; Returns a websocket-frame: opcode 1/2 with the whole message as payload,
;; or the received close frame (opcode 8) after `control' has handled it.
;; Control frames between fragments are passed to `control' and reading goes
;; on. An unfragmented message is delivered without copying; a fragmented one
;; is concatenated once when its final fragment arrives.
;; max-fragments: max data frames per message, 0 means no limit.
(define* (read-websocket-message port
                                 #:key
                                 (control (default-control-handler port))
                                 (max-frame (get-conf '(websocket maxpayload)))
                                 (max-message (get-conf '(websocket maxsize)))
                                 (min-fragment (get-conf '(websocket minpayload)))
                                 (max-fragments (get-conf '(websocket maxfragments))))
  (define (text? op) (= op #x1))
  (define (check-utf8 payload state)
    (or (utf8-scan payload 0 (bytevector-length payload) state)
        (throw 'websocket-err 1007 'read-websocket-message
               "Text message is not valid UTF-8")))
  (define (check-complete state)
    (unless (zero? state)
      (throw 'websocket-err 1007 'read-websocket-message
             "Text message ends in the middle of a UTF-8 sequence")))
  (define (assemble frags total)
    ;; frags is in reverse order
    (if (null? (cdr frags))
        (car frags)
        (let ((bv (make-bytevector total)))
          (let lp ((lst frags) (end total))
            (if (null? lst)
                bv
                (let* ((f (car lst))
                       (n (bytevector-length f))
                       (start (- end n)))
                  (bytevector-copy! f 0 bv start n)
                  (lp (cdr lst) start)))))))
  ;; opcode: #f when no fragmented message is in progress
  (let lp ((opcode #f) (frags '()) (total 0) (count 0) (state 0))
    (let* ((limit (min max-frame (- max-message total)))
           (frame (read-websocket-frame port limit min-fragment))
           (op (websocket-frame-opcode frame))
           (final? (websocket-frame-final? frame))
           (payload (websocket-frame-payload frame)))
      (cond
       ((control-opcode? op)
        (when (close-opcode? op)
          (check-close-payload payload))
        (control frame)
        (if (close-opcode? op)
            frame
            (lp opcode frags total count state)))
       ((zero? op)
        (unless opcode
          (throw 'websocket-err 1002 'read-websocket-message
                 "Continuation frame without a message in progress"))
        (let ((count (1+ count))
              (total (+ total (bytevector-length payload)))
              (state (if (text? opcode) (check-utf8 payload state) state)))
          (when (and (> max-fragments 0) (> count max-fragments))
            (throw 'websocket-err 1008 'read-websocket-message
                   "Too many fragments: > ~a" max-fragments))
          (cond
           (final?
            (when (text? opcode) (check-complete state))
            (make-websocket-frame #t opcode (assemble (cons payload frags) total)))
           (else
            (lp opcode (cons payload frags) total count state)))))
       (else
        (when opcode
          (throw 'websocket-err 1002 'read-websocket-message
                 "New data frame while a fragmented message is in progress"))
        (let ((state (if (text? op) (check-utf8 payload 0) 0)))
          (cond
           (final?
            (when (text? op) (check-complete state))
            frame)
           (else
            (lp op (list payload) (bytevector-length payload) 1 state)))))))))

;; ---------------------------------------------------------------------------
;; Writing
;;
;; NOTE: According to RFC 6455, a server MUST NOT mask any frames that it
;;       sends to the client.

(define (put-uint port v n)
  (let lp ((i (1- n)))
    (when (>= i 0)
      (put-u8 port (logand #xff (ash v (* -8 i))))
      (lp (1- i)))))

;; Write one frame whose payload is the slice payload[start, start+count).
;; Returns #t when written, #f when dropped because the output is closed.
(define* (write-websocket-frame port final? type payload
                                #:optional
                                (start 0)
                                (count (- (bytevector-length payload) start)))
  (let ((opcode (type->opcode type)))
    ;; Validate the slice before anything is written or marked, otherwise a
    ;; bad slice leaves a header on the wire and the stream out of sync.
    (unless (and (exact-integer? start) (exact-integer? count)
                 (<= 0 start) (<= 0 count)
                 (<= (+ start count) (bytevector-length payload)))
      (throw 'websocket-err 1011 'write-websocket-frame
             "Invalid slice: start ~a, count ~a, payload length ~a"
             start count (bytevector-length payload)))
    (cond
     ((websocket-output-closed? port)
      (DEBUG "Drop a ~a frame, the websocket output is closed on ~a~%" type port)
      #f)
     ((port-closed? port)
      (throw 'websocket-err 1006 'write-websocket-frame
             "The port `~a' was closed" port))
     (else
      (when (and (control-opcode? opcode)
                 (not (and final? (<= count 125))))
        (throw 'websocket-err 1011 'write-websocket-frame
               "Invalid control frame: final? ~a, length ~a" final? count))
      ;; Mark first: nothing may follow a close even if this write fails.
      (when (close-opcode? opcode)
        (websocket-output-close! port))
      (put-u8 port (logior (if final? #x80 #x00) opcode))
      (cond
       ((< count 126) (put-u8 port count))
       ((< count #x10000) (put-u8 port 126) (put-uint port count 2))
       (else (put-u8 port 127) (put-uint port count 8)))
      (put-bytevector port payload start count)
      (force-output port)
      #t))))

;; Return a writer for a data message: each call (writer port) writes the
;; next fragment and returns #t once the final one is written (or the output
;; is closed). Fragments are index ranges of payload, nothing is copied.
;; The writer only holds (payload, offset), so a connection writer can put
;; control frames between two calls.
;; fragment: max payload per fragment, 0 means no fragmentation.
(define* (make-fragment-writer type payload fragment
                               #:optional
                               (start 0)
                               (count (- (bytevector-length payload) start)))
  (unless (memq type '(text binary))
    (throw 'websocket-err 1011 'make-fragment-writer
           "Invalid data frame type `~a'" type))
  (let ((end (+ start count))
        (offset start)
        (first? #t)
        (done? #f))
    (lambda (port)
      ;; Once the final fragment is written (or the output is closed), later
      ;; calls write nothing, instead of an extra empty FIN continuation.
      (or done?
          (let* ((rest (- end offset))
                 (n (if (and (> fragment 0) (> rest fragment)) fragment rest))
                 (final? (= (+ offset n) end))
                 (written? (write-websocket-frame port final?
                                                  (if first? type 'continuation)
                                                  payload offset n)))
            (set! first? #f)
            (set! offset (+ offset n))
            (set! done? (or final? (not written?)))
            done?)))))

(define* (write-websocket-message port type payload
                                  #:key
                                  (fragment (get-conf '(websocket fragment)))
                                  (start 0)
                                  (count (- (bytevector-length payload) start)))
  (let ((next! (make-fragment-writer type payload fragment start count)))
    (let lp ()
      (unless (next! port) (lp)))))

;; code #f sends an empty close payload. reason is a string, at most 123
;; bytes once encoded; without it the standard reason text is used.
(define* (send-websocket-close port #:key (code 1000) (reason #f))
  (define (->payload)
    (cond
     ((not code) (make-bytevector 0))
     ((not (sendable-close-code? code))
      (throw 'websocket-err 1011 'send-websocket-close
             "Close code ~a can't be sent" code))
     (reason
      (let ((bv (string->utf8 reason)))
        (when (> (bytevector-length bv) 123)
          (throw 'websocket-err 1011 'send-websocket-close
                 "Close reason exceeds 123 bytes"))
        (make-close-payload code bv)))
     ((assv-ref *close-payloads* code))
     (else (make-close-payload code (make-bytevector 0)))))
  (write-websocket-frame port #t 'close (->payload)))

(define (print-websocket-frame frame)
  (call-with-output-string
   (lambda (port)
     (let ((payload (websocket-frame-payload frame)))
       (format port "<websocket-frame:~%")
       (format port "~10tfinal?: ~a~%" (websocket-frame-final? frame))
       (format port "~10ttype: ~a~%" (websocket-frame-type frame))
       (format port "~10tpayload-size: ~a>" (bytevector-length payload))))))
