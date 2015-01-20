;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2010, 2012 Göran Weinholt <goran@weinholt.se>
;; Copyright (C) 2014,2015
;; Mu Lei known as NalaGinrut <mulei@gnu.org>

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

;; Byte-oriented SHA-1 from FIPS 180-3 and RFC 3174.

;; The data being hashed will never be modified here.

;; TODO: give an error if more than 2^64 bits are processed?
;; TODO: Optimize. Should be simple enough with the help of a profiler.

(library (artanis crypto sha-1)
  (export make-sha-1 sha-1-update! sha-1-finish! sha-1-clear!
          sha-1 sha-1-copy sha-1-finish
          sha-1-transform!              ;for interested parties only
          sha-1-length
          sha-1-copy-hash! sha-1-96-copy-hash!
          sha-1->bytevector sha-1->string
          sha-1-hash=? sha-1-96-hash=?
          hmac-sha-1)
  (import (except (rnrs) bitwise-rotate-bit-field))

  (define (sha-1-length) 20)

  (define (vector-copy x) (vector-map (lambda (i) i) x))

  (define (rol32 n count)
    (let ((field1 (bitwise-and #xffffffff (bitwise-arithmetic-shift-left n count)))
          (field2 (bitwise-arithmetic-shift-right n (- 32 count))))
      (bitwise-ior field1 field2)))

  (define-record-type sha1state
    (fields (immutable H)               ;Hash
            (immutable W)               ;temporary data
            (immutable m)               ;unprocessed data
            (mutable pending)           ;length of unprocessed data
            (mutable processed)))       ;length of processed data

  (define (make-sha-1)
    (let ((H (list->vector initial-hash))
          (W (make-bytevector (* 4 80)))
          (m (make-bytevector (* 4 16))))
      (make-sha1state H W m 0 0)))

  (define (sha-1-copy state)
    (let ((H (vector-copy (sha1state-H state)))
          (W (make-bytevector (* 4 80)))
          (m (bytevector-copy (sha1state-m state))))
      (make-sha1state H W m
                      (sha1state-pending state)
                      (sha1state-processed state))))

  (define (sha-1-clear! state)
    (for-each (lambda (i v)
                (vector-set! (sha1state-H state) i v))
              '(0 1 2 3 4)
              initial-hash)
    (bytevector-fill! (sha1state-W state) 0)
    (bytevector-fill! (sha1state-m state) 0)
    (sha1state-pending-set! state 0)
    (sha1state-processed-set! state 0))

  (define initial-hash '(#x67452301 #xefcdab89 #x98badcfe #x10325476 #xc3d2e1f0))

  (define (Ch x y z)
    (bitwise-xor (bitwise-and x y)
                 (bitwise-and (bitwise-not x) z)))

  (define Parity bitwise-xor)

  (define (Maj x y z)
    (bitwise-xor (bitwise-and x y)
                 (bitwise-and x z)
                 (bitwise-and y z)))

  (define k1 #x5a827999)
  (define k2 #x6ed9eba1)
  (define k3 #x8f1bbcdc)
  (define k4 #xca62c1d6)

  (define (f t B C D)
    ((cond ((<= 0 t 19) Ch)
           ((<= 20 t 39) Parity)
           ((<= 40 t 59) Maj)
           (else Parity))
     B C D))

  (define (K t)
    (cond ((<= 0 t 19) k1)
          ((<= 20 t 39) k2)
          ((<= 40 t 59) k3)
          (else k4)))

  ;; This function transforms a whole 512 bit block.
  (define (sha-1-transform! H W m offset)
    ;; Copy the message block
    (do ((t 0 (+ t 4)))
        ((= t (* 4 16)))
      (bytevector-u32-native-set! W t (bytevector-u32-ref m (+ t offset) (endianness big))))
    ;; Initialize W[16..79]
    (do ((t (* 4 16) (+ t 4)))
        ((= t (* 4 80)))
      (bytevector-u32-native-set! W t (rol32
                                       (bitwise-xor (bytevector-u32-native-ref W (- t (* 4 3)))
                                                    (bytevector-u32-native-ref W (- t (* 4 8)))
                                                    (bytevector-u32-native-ref W (- t (* 4 14)))
                                                    (bytevector-u32-native-ref W (- t (* 4 16))))
                                       1)))
    ;; Do the hokey pokey
    (let lp ((A (vector-ref H 0))
             (B (vector-ref H 1))
             (C (vector-ref H 2))
             (D (vector-ref H 3))
             (E (vector-ref H 4))
             (t 0))
      (cond ((= t 80)
             (vector-set! H 0 (bitwise-and #xffffffff (+ A (vector-ref H 0))))
             (vector-set! H 1 (bitwise-and #xffffffff (+ B (vector-ref H 1))))
             (vector-set! H 2 (bitwise-and #xffffffff (+ C (vector-ref H 2))))
             (vector-set! H 3 (bitwise-and #xffffffff (+ D (vector-ref H 3))))
             (vector-set! H 4 (bitwise-and #xffffffff (+ E (vector-ref H 4)))))
            (else
             (lp (bitwise-and #xffffffff
                              (+ (rol32 A 5)
                                 (f t B C D)
                                 E
                                 (bytevector-u32-native-ref W (* 4 t))
                                 (K t)))
                 A
                 (rol32 B 30)
                 C
                 D
                 (+ t 1))))))

  ;; Add a bytevector to the state. Align your data to whole blocks if
  ;; you want this to go a little faster.
  (define sha-1-update!
    (case-lambda
      ((state data start end)
       (let ((m (sha1state-m state))    ;unprocessed data
             (H (sha1state-H state))
             (W (sha1state-W state)))
         (let lp ((offset start))
           (cond ((= (sha1state-pending state) 64)
                  ;; A whole block is pending
                  (sha-1-transform! H W m 0)
                  (sha1state-pending-set! state 0)
                  (sha1state-processed-set! state (+ 64 (sha1state-processed state)))
                  (lp offset))
                 ((= offset end)
                  (values))
                 ((or (> (sha1state-pending state) 0)
                      (> (+ offset 64) end))
                  ;; Pending data exists or less than a block remains.
                  ;; Add more pending data.
                  (let ((added (min (- 64 (sha1state-pending state))
                                    (- end offset))))
                    (bytevector-copy! data offset
                                      m (sha1state-pending state)
                                      added)
                    (sha1state-pending-set! state (+ added (sha1state-pending state)))
                    (lp (+ offset added))))
                 (else
                  ;; Consume a whole block
                  (sha-1-transform! H W data offset)
                  (sha1state-processed-set! state (+ 64 (sha1state-processed state)))
                  (lp (+ offset 64)))))))
      ((state data)
       (sha-1-update! state data 0 (bytevector-length data)))))

  (define zero-block (make-bytevector 64 0))

  ;; Finish the state by adding a 1, zeros and the counter.
  (define (sha-1-finish! state)
    (let ((m (sha1state-m state))
          (pending (+ (sha1state-pending state) 1)))
      (bytevector-u8-set! m (sha1state-pending state) #x80)
      (cond ((> pending 56)
             (bytevector-copy! zero-block 0
                               m pending
                               (- 64 pending))
             (sha-1-transform! (sha1state-H state)
                               (sha1state-W state)
                               m
                               0)
             (bytevector-fill! m 0))
            (else
             (bytevector-copy! zero-block 0
                               m pending
                               (- 64 pending))))
      ;; Number of bits in the data
      (bytevector-u64-set! m 56
                           (* (+ (sha1state-processed state)
                                 (- pending 1))
                              8)
                           (endianness big))
      (sha-1-transform! (sha1state-H state)
                        (sha1state-W state)
                        m
                        0)))

  (define (sha-1-finish state)
    (let ((copy (sha-1-copy state)))
      (sha-1-finish! copy)
      copy))

  ;; Find the SHA-1 of the concatenation of the given bytevectors.
  (define (sha-1 . data)
    (let ((state (make-sha-1)))
      (for-each (lambda (d) (sha-1-update! state d))
                data)
      (sha-1-finish! state)
      state))

  (define (copy-hash! state bv off len)
    (do ((i 0 (+ i 1)))
        ((= i len))
      (bytevector-u32-set! bv (+ off (* 4 i))
                           (vector-ref (sha1state-H state) i)
                           (endianness big))))

  (define (sha-1-copy-hash! state bv off)
    (copy-hash! state bv off 5))

  (define (sha-1-96-copy-hash! state bv off)
    (copy-hash! state bv off 3))

  (define (sha-1->bytevector state)
    (let ((ret (make-bytevector (* 4 5))))
      (sha-1-copy-hash! state ret 0)
      ret))

  (define (sha-1->string state)
    (apply string-append
           (map (lambda (x)
                  (if (< x #x10)
                      (string-append "0" (number->string x 16))
                      (number->string x 16)))
                (bytevector->u8-list (sha-1->bytevector state)))))

  ;; Compare an SHA-1 state with a bytevector. It is supposed to not
  ;; terminate early in order to not leak timing information. Assumes
  ;; that the bytevector's length is ok.
  (define (cmp state bv len)
    (do ((i 0 (fx+ i 1))
         (diff 0 (+ diff
                    (bitwise-xor
                     (bytevector-u32-ref bv (* 4 i) (endianness big))
                     (vector-ref (sha1state-H state) i)))))
        ((fx=? i len)
         (zero? diff))))

  (define (sha-1-hash=? state bv) (cmp state bv 5))

  (define (sha-1-96-hash=? state bv) (cmp state bv 3))

;;; HMAC-SHA-1. RFC 2104.

  ;; TODO: an API with make, update!, finish!, finish, clear!, copy, etc

  (define (hmac-sha-1 secret . data)
    ;; RFC 2104.
    (if (> (bytevector-length secret) 64)
        (apply hmac-sha-1 (sha-1->bytevector (sha-1 secret)) data)
        (let ((k-ipad (make-bytevector 64 0))
              (k-opad (make-bytevector 64 0)))
          (bytevector-copy! secret 0 k-ipad 0 (bytevector-length secret))
          (bytevector-copy! secret 0 k-opad 0 (bytevector-length secret))
          (do ((i 0 (fx+ i 1)))
              ((fx=? i 64))
            (bytevector-u8-set! k-ipad i (fxxor #x36 (bytevector-u8-ref k-ipad i)))
            (bytevector-u8-set! k-opad i (fxxor #x5c (bytevector-u8-ref k-opad i))))
          (let ((state (make-sha-1)))
            (sha-1-update! state k-ipad)
            (for-each (lambda (d) (sha-1-update! state d)) data)
            (sha-1-finish! state)
            (let ((digest (sha-1->bytevector state)))
              (sha-1-clear! state)
              (sha-1-update! state k-opad)
              (sha-1-update! state digest)
              (sha-1-finish! state)
              state))))))
