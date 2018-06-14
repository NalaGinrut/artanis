;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-

;; Copyright © 2009, 2010, 2012, 2013 Göran Weinholt <goran@weinholt.se>
;; Copyright 2018 Mu Lei known as NalaGinrut <nalaginrut@gmail.com>

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;; RFC 4648 Base-N Encodings

#!r6rs

(library (artanis crypto base64)
  (export base64-encode base64-decode)
  (import (guile) (rnrs))

  (define base64-alphabet
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

  (define base64url-alphabet
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")

  (define %base64-encode
    (case-lambda
      ;; Simple interface. Returns a string containing the canonical
      ;; base64 representation of the given bytevector.
      ((bv)
       (%base64-encode bv 0 (bytevector-length bv) #f #f base64-alphabet #f))
      ((bv start)
       (%base64-encode bv start (bytevector-length bv) #f #f base64-alphabet #f))
      ((bv start end)
       (%base64-encode bv start end #f #f base64-alphabet #f))
      ((bv start end line-length)
       (%base64-encode bv start end line-length #f base64-alphabet #f))
      ((bv start end line-length no-padding)
       (%base64-encode bv start end line-length no-padding base64-alphabet #f))
      ((bv start end line-length no-padding alphabet)
       (%base64-encode bv start end line-length no-padding alphabet #f))
      ;; Base64 encodes the bytes [start,end[ in the given bytevector.
      ;; Lines are limited to line-length characters (unless #f),
      ;; which must be a multiple of four. To omit the padding
      ;; characters (#\=) set no-padding to a true value. If port is
      ;; #f, returns a string.
      ((bv start end line-length no-padding alphabet port)
       (assert (or (not line-length) (zero? (mod line-length 4))))
       (let-values (((p extract) (if port
                                     (values port (lambda () (values)))
                                     (open-string-output-port))))
         (letrec ((put (if line-length
                           (let ((chars 0))
                             (lambda (p c)
                               (when (fx=? chars line-length)
                                 (set! chars 0)
                                 (put-char p #\linefeed))
                               (set! chars (fx+ chars 1))
                               (put-char p c)))
                           put-char)))
           (let lp ((i start))
             (cond ((= i end))
                   ((<= (+ i 3) end)
                    (let ((x (bytevector-uint-ref bv i (endianness big) 3)))
                      (put p (string-ref alphabet (fxbit-field x 18 24)))
                      (put p (string-ref alphabet (fxbit-field x 12 18)))
                      (put p (string-ref alphabet (fxbit-field x 6 12)))
                      (put p (string-ref alphabet (fxbit-field x 0 6)))
                      (lp (+ i 3))))
                   ((<= (+ i 2) end)
                    (let ((x (fxarithmetic-shift-left (bytevector-u16-ref bv i (endianness big)) 8)))
                      (put p (string-ref alphabet (fxbit-field x 18 24)))
                      (put p (string-ref alphabet (fxbit-field x 12 18)))
                      (put p (string-ref alphabet (fxbit-field x 6 12)))
                      (unless no-padding
                        (put p #\=))))
                   (else
                    (let ((x (fxarithmetic-shift-left (bytevector-u8-ref bv i) 16)))
                      (put p (string-ref alphabet (fxbit-field x 18 24)))
                      (put p (string-ref alphabet (fxbit-field x 12 18)))
                      (unless no-padding
                        (put p #\=)
                        (put p #\=)))))))
         (extract)))))

  ;; Create a lookup table for the alphabet and remember the latest table.
  (define get-decode-table
    (let ((ascii-table #f)
          (extra-table '())     ;in the unlikely case of unicode chars
          (table-alphabet #f))
      (lambda (alphabet)
        (unless (eq? alphabet table-alphabet)
          ;; Rebuild the table.
          (do ((ascii (make-vector 128 #f))
               (extra '())
               (i 0 (+ i 1)))
              ((= i (string-length alphabet))
               (set! ascii-table ascii)
               (set! extra-table extra))
            (let ((c (char->integer (string-ref alphabet i))))
              (if (fx<=? c 127)
                  (vector-set! ascii c i)
                  (set! extra (cons (cons c i) extra)))))
          (set! table-alphabet alphabet))
        (values ascii-table extra-table))))

  ;; Decodes a correctly padded base64 string, optionally ignoring
  ;; non-alphabet characters.
  (define %base64-decode
    (case-lambda
      ((str)
       (%base64-decode str base64-alphabet #f))
      ((str alphabet)
       (%base64-decode str alphabet #f))
      ((str alphabet port)
       (%base64-decode str alphabet port #t))
      ((str alphabet port strict?)
       (define (pad? c) (eqv? c (char->integer #\=)))
       (let-values (((p extract) (if port
                                     (values port (lambda () (values)))
                                     (open-bytevector-output-port)))
                    ((ascii extra) (get-decode-table alphabet)))
         (define-syntax lookup
           (syntax-rules ()
             ((_ c) (or (and (fx<=? c 127) (vector-ref ascii c))
                        (cond ((assv c extra) => cdr)
                              (else #f))))))
         (let* ((len (if strict?
                         (string-length str)
                         (let lp ((i (fx- (string-length str) 1)))
                           ;; Skip trailing invalid chars.
                           (cond ((fxzero? i) 0)
                                 ((let ((c (char->integer (string-ref str i))))
                                    (or (lookup c) (pad? c)))
                                  (fx+ i 1))
                                 (else (lp (fx- i 1))))))))
           (let lp ((i 0))
             (cond
              ((fx=? i len)
               (extract))
              ((fx<=? i (fx- len 4))
               (let lp* ((c1 (char->integer (string-ref str i)))
                         (c2 (char->integer (string-ref str (fx+ i 1))))
                         (c3 (char->integer (string-ref str (fx+ i 2))))
                         (c4 (char->integer (string-ref str (fx+ i 3))))
                         (i i))
                 (let ((i1 (lookup c1)) (i2 (lookup c2))
                       (i3 (lookup c3)) (i4 (lookup c4)))
                   (cond
                    ((and i1 i2 i3 i4)
                     ;; All characters present and accounted for.
                     ;; The most common case.
                     (let ((x (fxior (fxarithmetic-shift-left i1 18)
                                     (fxarithmetic-shift-left i2 12)
                                     (fxarithmetic-shift-left i3 6)
                                     i4)))
                       (put-u8 p (fxbit-field x 16 24))
                       (put-u8 p (fxbit-field x 8 16))
                       (put-u8 p (fxbit-field x 0 8))
                       (lp (fx+ i 4))))
                    ((and i1 i2 i3 (pad? c4) (= i (- len 4)))
                     ;; One padding character at the end of the input.
                     (let ((x (fxior (fxarithmetic-shift-left i1 18)
                                     (fxarithmetic-shift-left i2 12)
                                     (fxarithmetic-shift-left i3 6))))
                       (put-u8 p (fxbit-field x 16 24))
                       (put-u8 p (fxbit-field x 8 16))
                       (lp (fx+ i 4))))
                    ((and i1 i2 (pad? c3) (pad? c4) (= i (- len 4)))
                     ;; Two padding characters.
                     (let ((x (fxior (fxarithmetic-shift-left i1 18)
                                     (fxarithmetic-shift-left i2 12))))
                       (put-u8 p (fxbit-field x 16 24))
                       (lp (fx+ i 4))))
                    ((not strict?)
                     ;; Non-alphabet characters.
                     (let lp ((i i) (c* '()) (n 4))
                       (cond ((fxzero? n)
                              ;; Found four valid characters.
                              (lp* (cadddr c*) (caddr c*) (cadr c*) (car c*)
                                   (fx- i 4)))
                             ((fx=? i len)
                              (error '%base64-decode
                                     "Invalid input in non-strict mode."
                                     i c*))
                             (else
                              ;; Gather alphabetic (or valid
                              ;; padding) characters.
                              (let ((c (char->integer (string-ref str i))))
                                (cond ((or (lookup c)
                                           (and (pad? c)
                                                (fx<=? n 2)
                                                (fx=? i (fx- len n))))
                                       (lp (fx+ i 1) (cons c c*) (fx- n 1)))
                                      (else
                                       (lp (fx+ i 1) c* n))))))))
                    (else
                     (error '%base64-decode
                            "Invalid input in strict mode."
                            c1 c2 c3 c4))))))
              (else
               (error '%base64-decode
                      "The input is too short, it may be missing padding."
                      i)))))))))

  (define (get-line-comp f port)
    (if (port-eof? port)
        (eof-object)
        (f (get-line port))))

  (define (base64-encode str/bv)
    (%base64-encode (if (string? str/bv) (string->utf8 str/bv) str/bv)))

  (define* (base64-decode str/bv #:optional (bv? #t))
    (let ((ret (if bv? identity utf8->string)))
      (ret (%base64-decode (if (bytevector? str/bv) (utf8->string str/bv) str/bv))))))
