;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;; Copyright (C) 2008, 2010, 2012 Andy Wingo <wingo at pobox dot com>
;; Copyright (C) 2009 Andreas Rottmann <a dot rottmann at gmx dot at>
;; Copyright (C) 2013,2014,2015
;; Mu Lei known as NalaGinrut <mulei@gnu.org>

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

;;; Commentary:
;;
;; base 64 y'all
;;
;;; Code:

(define-module (artanis crypto base64)
  #:use-module (rnrs bytevectors)
  #:export (base64-encode base64-decode))

(define b64-bytes
  (string->utf8
   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwzyz0123456789+/"))

(define (int->b64-byte i)
  (bytevector-u8-ref b64-bytes (logand i 63)))

(define b64-byte-ranges
  (map cons
       (map char->integer '(#\A #\a #\0 #\+ #\/))
       (map char->integer '(#\Z #\z #\9 #\+ #\/))))

(define (b64-byte->int i)
  (let lp ((ranges b64-byte-ranges) (out 0))
    (cond ((null? ranges)
           (error "bad base64 byte" i))
          ((and (>= i (caar ranges)) (<= i (cdar ranges)))
           (+ out (- i (caar ranges))))
          (else
           (lp (cdr ranges) (+ out (+ 1 (- (cdar ranges)
                                           (caar ranges)))))))))

(define (bytevector-pad bv n fill)
  (let ((result (make-bytevector n fill)))
    (bytevector-copy! bv 0 result 0 (bytevector-length bv))
    result))

(define-syntax bytevector-map-n-to-m
  (lambda (stx)
    (syntax-case stx ()
      ((_ n m)
       (with-syntax (((byte-in ...)
                      (map (lambda (x)
                             #`(bytevector-u8-ref s (+ i #,x)))
                           (iota (syntax->datum #'n))))
                     ((byte-out ...)
                      (generate-temporaries (iota (syntax->datum #'m))))
                     ((byte-out-idx ...)
                      (iota (syntax->datum #'m))))
         #'(lambda (proc s)
             (let* ((len (bytevector-length s))
                    (out (make-bytevector (* len (/ m n)))))
               (let lp ((i 0) (j 0))
                 (cond
                  ((< i len)
                   (call-with-values (lambda () (proc byte-in ...))
                     (lambda (byte-out ...)
                       (bytevector-u8-set! out (+ j byte-out-idx) byte-out)
                       ...))
                   (lp (+ i n) (+ j m)))
                  (else out))))))))))

(define (bytevector-fill-range! bv start end u8)
  (do ((i (- end 1) (- i 1)))
      ((< i start))
      (bytevector-u8-set! bv i u8)))

(define (bytevector-copy/padding bv npad pad-byte)
  (let ((result (bytevector-copy bv))
        (len (bytevector-length bv)))
    (bytevector-fill-range! result (- len npad) len pad-byte)
    result))

(define (base64-encode str/bv)
  (let* ((bv (if (string? str/bv) (string->utf8 str/bv) str/bv))
         (npad (remainder (- 3 (remainder (bytevector-length bv) 3)) 3))
         (out ((bytevector-map-n-to-m 3 4)
               (lambda (x y z)
                 (let ((n (logior (ash x 16) (ash y 8) z)))
                   (values (int->b64-byte (ash n -18))
                           (int->b64-byte (ash n -12))
                           (int->b64-byte (ash n -6))
                           (int->b64-byte n))))
               (bytevector-pad bv (+ (bytevector-length bv) npad) 0))))
    (bytevector-fill-range! out
                            (- (bytevector-length out) npad)
                            (bytevector-length out)
                            (char->integer #\=))
    (utf8->string out)))

(define eql-byte (char->integer #\=))

(define (b64-bv-npad bv)
  (let ((len (bytevector-length bv)))
    (if (> len 0)
        (if (= (bytevector-u8-ref bv (- len 1)) eql-byte)
            (if (> len 1)
                (if (= (bytevector-u8-ref bv (- len 2)) eql-byte)
                    2
                    1)
                1)
            0)
        0)))

(define* (base64-decode str #:optional (outstr #t))
  (let* ((bv (string->utf8 str))
         (npad (b64-bv-npad bv))
         (out ((bytevector-map-n-to-m 4 3)
               (lambda (w x y z)
                 (let ((n (logior (ash (b64-byte->int w) 18)
                                  (ash (b64-byte->int x) 12)
                                  (ash (b64-byte->int y) 6)
                                  (b64-byte->int z))))
                   (values (ash n -16)
                           (logand (ash n -8) 255)
                           (logand n 255))))
               (bytevector-copy/padding bv npad (char->integer #\A))))
         (result (make-bytevector (- (bytevector-length out) npad))))
    (bytevector-copy! out 0 result 0 (bytevector-length result))
    (if outstr (utf8->string result) result)))
