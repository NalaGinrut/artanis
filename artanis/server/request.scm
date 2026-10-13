;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2026
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

;; =============================================================================
;; Reading an HTTP request from a client socket, with limits.
;;
;; Guile's read-request has no limit: a request line or a header line is read
;; until its end, and the headers until the empty line, however long they
;; are. So the head of a request (the request line and the headers) is read
;; here first, up to a limit, then parsed by Guile's read-request from the
;; bytes that have been read.
;;
;; The reads go through the suspendable port, so a slow peer suspends the
;; task, it never blocks the server.
;; =============================================================================

(define-module (artanis server request)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (web request)
  #:export (read-request-head))

;; Read the head of a request from port, at most limit bytes, and parse it.
;; Returns two values: the request, whose port is port, and the number of
;; bytes of the head. Returns the EOF object and 0 if port is at EOF, or
;; there are only empty lines before EOF.
;; Throws:
;;  - 'artanis-err 414 if the request line is longer than limit;
;;  - 'artanis-err 431 if the head is longer than limit;
;;  - 'artanis-err 400 if the peer closes in the middle of the head;
;;  - what Guile's read-request throws for a bad request.
;; Empty lines before the request line are ignored, as RFC 9112 2.2 suggests,
;; they count toward the limit.
(define (read-request-head port limit)
  (define-values (out get-bytes) (open-bytevector-output-port))
  (define (too-large! first-line?)
    (if first-line?
        (throw 'artanis-err 414 'read-request-head
               "The request line is longer than ~a bytes" limit)
        (throw 'artanis-err 431 'read-request-head
               "The request header fields are longer than ~a bytes" limit)))
  ;; count: bytes read; line: bytes of the current line before its LF;
  ;; prev: the previous byte; started?: the request line has begun;
  ;; first-line?: still in the request line.
  (let lp ((count 0) (line 0) (prev #f) (started? #f) (first-line? #t))
    (let ((b (get-u8 port)))
      (cond
       ((eof-object? b)
        (if (not started?)
            (values b 0)
            (throw 'artanis-err 400 'read-request-head
                   "The peer closed in the middle of the request head")))
       ((>= count limit) (too-large! first-line?))
       ((= b 10)                        ; LF, a CR before it is part of line
        (let ((empty? (or (zero? line) (and (= line 1) (eqv? prev 13)))))
          (cond
           ((not started?)
            ;; An empty line before the request line, drop it.
            (get-bytes)
            (lp (1+ count) 0 b #f #t))
           (empty?
            (put-u8 out b)
            (values (parse-head (get-bytes) port) (1+ count)))
           (else
            (put-u8 out b)
            (lp (1+ count) 0 b #t #f)))))
       (else
        (put-u8 out b)
        (lp (1+ count) (1+ line) b (or started? (not (= b 13))) first-line?))))))

;; Parse the head with Guile's read-request, and make it a request on port,
;; so the body is read from port.
(define (parse-head bv port)
  (let* ((hp (open-bytevector-input-port bv))
         (req (read-request hp)))
    ;; Like read-request, one character is one byte on port.
    (set-port-encoding! port "ISO-8859-1")
    (build-request (request-uri req)
                   #:method (request-method req)
                   #:version (request-version req)
                   #:headers (request-headers req)
                   #:meta (request-meta req)
                   #:port port
                   #:validate-headers? #f)))
