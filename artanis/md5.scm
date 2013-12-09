;; (md5) -- md5 hashing in scheme
;; Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.
;; Copyright (C) 2004 Moritz Schulte <moritz@gnu.org>.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

#!
;;; Commentary:
This code is heavily based on the MD5 implementation contained in
Libgcrypt.  To a certain degree this code is a literal translation from
referenced C implementation into Scheme.
;;; Code:
!#

(define-module (artanis md5)
  #:use-module (ice-9 rw)
  #:export (md5))

;; General helper functions.

(define (buffer->hexstring string)
  (define (buffer->hexstring-do string-rest string-new)
    (if (string-null? string-rest)
	string-new
	(let ((byte (char->integer (string-ref string-rest 0))))
	  (buffer->hexstring-do
	   (substring string-rest 1)
	   (string-append string-new
			  (number->string (logand (ash byte -4) #xF) 16)
			  (number->string (logand (ash byte -0) #xF) 16))))))
  (buffer->hexstring-do string ""))

(define (buffer->word buffer)
  (logior (ash (char->integer (string-ref buffer 0))  0)
	  (ash (char->integer (string-ref buffer 1))  8)
	  (ash (char->integer (string-ref buffer 2)) 16)
	  (ash (char->integer (string-ref buffer 3)) 24)))

(define (buffer->words buffer n)
  (define (buffer->words-do buffer i words)
    (if (= i n)
	words
	(buffer->words-do (substring buffer 4)
			  (+ i 1)
			  (append words
				  `(,(buffer->word (substring buffer 0 4)))))))
  (buffer->words-do buffer 0 `()))

(define (word->buffer word)
  (let ((buffer (make-string 4 #\nul)))
    (string-set! buffer 0 (integer->char (logand (ash word  -0) #xFF)))
    (string-set! buffer 1 (integer->char (logand (ash word  -8) #xFF)))
    (string-set! buffer 2 (integer->char (logand (ash word -16) #xFF)))
    (string-set! buffer 3 (integer->char (logand (ash word -24) #xFF)))
    buffer))

;; Some math basics.

(define f-add +)
(define f-ash ash)

(define (+ . args)
  (modulo (apply f-add args) #x100000000))

(define (ash x n)
  (modulo (f-ash x n) #x100000000))

(define (rol x n)
  (logior (ash x n)
	  (ash x (- (- 32 n)))))

;; Return a new, initialized MD5 context.
(define (md5-init)
  (let ((buffer-space (make-string 64 #\nul)))
    ;; Since this is a mutable state, cons it up
    (list
     (cons 'values (list (cons 'a #x67452301)
                         (cons 'b #xEFCDAB89)
                         (cons 'c #x98BADCFE)
                         (cons 'd #x10325476)))
     (cons 'buffer (list (cons 'space buffer-space)
                         (cons 'data-size 0)))
     (cons 'stats (list (cons 'blocks-processed 0))))))

(define (md5-func-f b c d)
  (logior (logand b c) (logand (lognot b) d)))

(define (md5-func-g b c d)
  (logior (logand d b) (logand (lognot d) c)))

(define (md5-func-h b c d)
  (logxor b c d))

(define (md5-func-i b c d)
  (logxor c (logior b (lognot d))))

(define-macro (md5-transform-op-round1 a b c d s T)
  `(begin
     (set! ,a (+ ,a (md5-func-f ,b ,c ,d) (list-ref words word-idx) ,T))
     (set! word-idx (+ word-idx 1))
     (set! ,a (rol ,a ,s))
     (set! ,a (+ ,a ,b))))

(define-macro (md5-transform-op-round2/3/4 f a b c d k s T)
  `(begin
     (set! ,a (+ ,a (,f ,b ,c ,d) (list-ref words ,k) ,T))
     (set! ,a (rol ,a ,s))
     (set! ,a (+ ,a ,b))))

(define (md5-transform-block context data)
  (let ((words    (buffer->words data 16))
	(word-idx 0)
	(a        (assq-ref (assq-ref context 'values) 'a))
	(b        (assq-ref (assq-ref context 'values) 'b))
	(c        (assq-ref (assq-ref context 'values) 'c))
	(d        (assq-ref (assq-ref context 'values) 'd)))

    ;; Round 1.

    (md5-transform-op-round1 a b c d  7 #xD76AA478)
    (md5-transform-op-round1 d a b c 12 #xE8C7B756)
    (md5-transform-op-round1 c d a b 17 #x242070DB)
    (md5-transform-op-round1 b c d a 22 #xC1BDCEEE)
    (md5-transform-op-round1 a b c d  7 #xF57C0FAF)
    (md5-transform-op-round1 d a b c 12 #x4787C62A)
    (md5-transform-op-round1 c d a b 17 #xA8304613)
    (md5-transform-op-round1 b c d a 22 #xFD469501)
    (md5-transform-op-round1 a b c d  7 #x698098D8)
    (md5-transform-op-round1 d a b c 12 #x8B44F7AF)
    (md5-transform-op-round1 c d a b 17 #xFFFF5BB1)
    (md5-transform-op-round1 b c d a 22 #x895CD7BE)
    (md5-transform-op-round1 a b c d  7 #x6B901122)
    (md5-transform-op-round1 d a b c 12 #xFD987193)
    (md5-transform-op-round1 c d a b 17 #xA679438E)
    (md5-transform-op-round1 b c d a 22 #x49B40821)

    ;; Round 2.

    (md5-transform-op-round2/3/4 md5-func-g a b c d  1  5 #xF61E2562)
    (md5-transform-op-round2/3/4 md5-func-g d a b c  6  9 #xC040B340)
    (md5-transform-op-round2/3/4 md5-func-g c d a b 11 14 #x265E5A51)
    (md5-transform-op-round2/3/4 md5-func-g b c d a  0 20 #xE9B6C7AA)
    (md5-transform-op-round2/3/4 md5-func-g a b c d  5  5 #xD62F105D)
    (md5-transform-op-round2/3/4 md5-func-g d a b c 10  9 #x02441453)
    (md5-transform-op-round2/3/4 md5-func-g c d a b 15 14 #xD8A1E681)
    (md5-transform-op-round2/3/4 md5-func-g b c d a  4 20 #xE7D3FBC8)
    (md5-transform-op-round2/3/4 md5-func-g a b c d  9  5 #x21E1CDE6)
    (md5-transform-op-round2/3/4 md5-func-g d a b c 14  9 #xC33707D6)
    (md5-transform-op-round2/3/4 md5-func-g c d a b  3 14 #xF4D50D87)
    (md5-transform-op-round2/3/4 md5-func-g b c d a  8 20 #x455A14ED)
    (md5-transform-op-round2/3/4 md5-func-g a b c d 13  5 #xA9E3E905)
    (md5-transform-op-round2/3/4 md5-func-g d a b c  2  9 #xFCEFA3F8)
    (md5-transform-op-round2/3/4 md5-func-g c d a b  7 14 #x676F02D9)
    (md5-transform-op-round2/3/4 md5-func-g b c d a 12 20 #x8D2A4C8A)

    ;; Round 3.

    (md5-transform-op-round2/3/4 md5-func-h a b c d  5  4 #xFFFA3942)
    (md5-transform-op-round2/3/4 md5-func-h d a b c  8 11 #x8771F681)
    (md5-transform-op-round2/3/4 md5-func-h c d a b 11 16 #x6D9D6122)
    (md5-transform-op-round2/3/4 md5-func-h b c d a 14 23 #xFDE5380C)
    (md5-transform-op-round2/3/4 md5-func-h a b c d  1  4 #xA4BEEA44)
    (md5-transform-op-round2/3/4 md5-func-h d a b c  4 11 #x4BDECFA9)
    (md5-transform-op-round2/3/4 md5-func-h c d a b  7 16 #xF6BB4B60)
    (md5-transform-op-round2/3/4 md5-func-h b c d a 10 23 #xBEBFBC70)
    (md5-transform-op-round2/3/4 md5-func-h a b c d 13  4 #x289B7EC6)
    (md5-transform-op-round2/3/4 md5-func-h d a b c  0 11 #xEAA127FA)
    (md5-transform-op-round2/3/4 md5-func-h c d a b  3 16 #xD4EF3085)
    (md5-transform-op-round2/3/4 md5-func-h b c d a  6 23 #x04881D05)
    (md5-transform-op-round2/3/4 md5-func-h a b c d  9  4 #xD9D4D039)
    (md5-transform-op-round2/3/4 md5-func-h d a b c 12 11 #xE6DB99E5)
    (md5-transform-op-round2/3/4 md5-func-h c d a b 15 16 #x1FA27CF8)
    (md5-transform-op-round2/3/4 md5-func-h b c d a  2 23 #xC4AC5665)

    ;; Round 4.
    
    (md5-transform-op-round2/3/4 md5-func-i a b c d  0  6 #xF4292244)
    (md5-transform-op-round2/3/4 md5-func-i d a b c  7 10 #x432AFF97)
    (md5-transform-op-round2/3/4 md5-func-i c d a b 14 15 #xAB9423A7)
    (md5-transform-op-round2/3/4 md5-func-i b c d a  5 21 #xFC93A039)
    (md5-transform-op-round2/3/4 md5-func-i a b c d 12  6 #x655B59C3)
    (md5-transform-op-round2/3/4 md5-func-i d a b c  3 10 #x8F0CCC92)
    (md5-transform-op-round2/3/4 md5-func-i c d a b 10 15 #xFFEFF47D)
    (md5-transform-op-round2/3/4 md5-func-i b c d a  1 21 #x85845DD1)
    (md5-transform-op-round2/3/4 md5-func-i a b c d  8  6 #x6FA87E4F)
    (md5-transform-op-round2/3/4 md5-func-i d a b c 15 10 #xFE2CE6E0)
    (md5-transform-op-round2/3/4 md5-func-i c d a b  6 15 #xA3014314)
    (md5-transform-op-round2/3/4 md5-func-i b c d a 13 21 #x4E0811A1)
    (md5-transform-op-round2/3/4 md5-func-i a b c d  4  6 #xF7537E82)
    (md5-transform-op-round2/3/4 md5-func-i d a b c 11 10 #xBD3AF235)
    (md5-transform-op-round2/3/4 md5-func-i c d a b  2 15 #x2AD7D2BB)
    (md5-transform-op-round2/3/4 md5-func-i b c d a  9 21 #xEB86D391)

    (assq-set! (assq-ref context 'values)
	       'a
	       (+ (assq-ref (assq-ref context 'values) 'a)
		  a))
    (assq-set! (assq-ref context 'values)
	       'b
	       (+ (assq-ref (assq-ref context 'values) 'b)
		  b))
    (assq-set! (assq-ref context 'values)
	       'c
	       (+ (assq-ref (assq-ref context 'values) 'c)
		  c))
    (assq-set! (assq-ref context 'values)
	       'd
	       (+ (assq-ref (assq-ref context 'values) 'd)
		  d))))

(define (md5-write-do context data data-size)

  (if (= (assq-ref (assq-ref context 'buffer) 'data-size) 64)
      ;; Flush the buffer.
      (begin
	(md5-transform-block context (assq-ref (assq-ref context 'buffer)
					       'space))
	(assq-set! (assq-ref context 'buffer) 'data-size 0)
	(assq-set! (assq-ref context 'stats)
		   'blocks-processed
		   (+ (assq-ref (assq-ref context 'stats) 'blocks-processed)
		      1))))

  (if (> data-size 0)
      (begin

	(if (> (assq-ref (assq-ref context 'buffer)
			 'data-size)
	       0)
	    ;; Fill buffer.
	    (while (and (> data-size
			   0)
			(< (assq-ref (assq-ref context 'buffer)
				     'data-size)
			   64))
		   (begin
		     (string-set! (assq-ref (assq-ref context 'buffer)
					    'space)
				  (assq-ref (assq-ref context 'buffer)
					    'data-size)
				  (string-ref data 0))
		     (assq-set! (assq-ref context 'buffer)
				'data-size
				(+ (assq-ref (assq-ref context 'buffer)
					     'data-size)
				   1))
		     (set! data (substring data 1))
		     (set! data-size (- data-size 1)))))

	;; Transform whole blocks.
	(while (>= data-size 64)
	       (begin
		 (md5-transform-block context data)
		 (assq-set! (assq-ref context 'stats)
			    'blocks-processed
			    (+ (assq-ref (assq-ref context 'stats)
					 'blocks-processed)
			       1))
		 (set! data-size (- data-size 64))
		 (set! data (substring data 64))))

	;; Fill buffer.
	(while (and (> data-size
		       0)
		    (< (assq-ref (assq-ref context 'buffer)
				 'data-size)
		       64))
	       (begin
		 (string-set! (assq-ref (assq-ref context 'buffer)
					'space)
			      (assq-ref (assq-ref context 'buffer)
					'data-size)
			      (string-ref data 0))
		 (assq-set! (assq-ref context 'buffer)
			    'data-size
			    (+ (assq-ref (assq-ref context 'buffer)
					 'data-size)
			       1))
		 (set! data-size (- data-size 1))
		 (set! data (substring data 1)))))))

;; Write data to context.
(define (md5-write context data data-size)
  (md5-write-do context data data-size))

;; Finalize context, return hash.
(define (md5-finalize context)
  (let ((t   0)
	(msb 0)
	(lsb 0))

    (md5-write-do context "" 0)

    (set! t (assq-ref (assq-ref context 'stats)
		      'blocks-processed))
    (set! lsb (ash t   6))
    (set! msb (ash t -26))

    (set! t lsb)
    (set! lsb (+ lsb (assq-ref (assq-ref context 'buffer)
			       'data-size)))
    (if (< lsb t)
	(set! msb (+ msb 1)))

    (set! t lsb)
    (set! lsb (ash lsb 3))
    (set! msb (ash msb 3))
    (set! msb (logior msb (ash t -29)))

    (if (< (assq-ref (assq-ref context 'buffer) 'data-size) 56)
	(begin
	  (string-set! (assq-ref (assq-ref context 'buffer)
				 'space)
		       (assq-ref (assq-ref context 'buffer)
				 'data-size)
		       (integer->char #x80))
	  (assq-set! (assq-ref context 'buffer)
		     'data-size
		     (+ (assq-ref (assq-ref context 'buffer)
				  'data-size)
			1))

	  (while (< (assq-ref (assq-ref context 'buffer)
			      'data-size)
		    56)
		 (begin
		   (string-set! (assq-ref (assq-ref context 'buffer)
					  'space)
				(assq-ref (assq-ref context 'buffer)
					  'data-size)
				#\nul)
		   (assq-set! (assq-ref context 'buffer)
			      'data-size
			      (+ (assq-ref (assq-ref context 'buffer)
					   'data-size)
				 1)))))
	(begin
	  (string-set! (assq-ref (assq-ref 'context 'buffer)
				 'space)
		       (assq-ref (assq-ref 'context 'buffer)
				 'data-size)
		       (integer->char #x80))
	  (while (< (assq-ref (assq-ref context 'buffer)
			      'data-size)
		    64)
		 (begin
		   (string-set! (assq-ref (assq-ref context 'buffer)
					  'space)
				(assq-ref (assq-ref context 'buffer)
					  'data-size)
				0)
		   (assq-set! (assq-ref context 'buffer)
			      'data-size
			      (+ (assq-ref (assq-ref context 'buffer)
					   'data-size)
				 1))))
	  (md5-write-do context "" 0)
	  (substring-fill! (assq-ref (assq-ref context 'buffer)
				     'space)
			   0
			   56
			   #\nul)))

    (let ((final-string (map (lambda (x)
			       (integer->char (logand x #xFF)))
			     `(,lsb
			       ,(ash lsb  -8)
			       ,(ash lsb -16)
			       ,(ash lsb -24)
			       ,msb
			       ,(ash msb  -8)
			       ,(ash msb -16)
			       ,(ash msb -24))))
	  (buffer (assq-ref (assq-ref context 'buffer) 'space)))
      (string-set! buffer 56 (list-ref final-string 0))
      (string-set! buffer 57 (list-ref final-string 1))
      (string-set! buffer 58 (list-ref final-string 2))
      (string-set! buffer 59 (list-ref final-string 3))
      (string-set! buffer 60 (list-ref final-string 4))
      (string-set! buffer 61 (list-ref final-string 5))
      (string-set! buffer 62 (list-ref final-string 6))
      (string-set! buffer 63 (list-ref final-string 7)))

    (md5-transform-block context (assq-ref (assq-ref context 'buffer)
					   'space))

    (buffer->hexstring
     (string-append (word->buffer (assq-ref (assq-ref context 'values) 'a))
		    (word->buffer (assq-ref (assq-ref context 'values) 'b))
		    (word->buffer (assq-ref (assq-ref context 'values) 'c))
		    (word->buffer (assq-ref (assq-ref context 'values) 'd))))))

(define (general-read-string!/partial buffer port)
  (if (file-port? port)
      (read-string!/partial buffer port)
      (let ((max-index (- (string-length buffer) 1)))
        (let loop ((ch (read-char port))
                   (read 0))
          (if (eof-object? ch)
              (if (= read 0)
                  #f
                  read)
              (begin 
                (string-set! buffer read ch)
                (if (< read max-index)
                    (loop (read-char port) (+ read 1))
                    (+ read 1))))))))
              
(define (md5 . port)
  "Reads data from @var{port}, and returns a string containing the calculated
md5 hash of the data.  If @var{port} is not given, then the default input
port is used."  
  (define (process-data buffer port callback arg)
    (define (process-data-do)
      (let ((bytes-read (general-read-string!/partial buffer port)))
	(if (not bytes-read)
	    #t
	    (begin
	      (callback arg buffer bytes-read)
	      (process-data-do)))))
    (process-data-do))

  (define (process-data-callback arg data data-size)
    (md5-write arg data data-size))

  (if (null? port)
      (set! port (current-input-port))
      (set! port (car port)))

  (let* ((context     (md5-init))
	 (buffer-size 4096)
	 (buffer      (make-string buffer-size #\nul)))
    (process-data buffer port process-data-callback context)
    (md5-finalize context)))

(define (string->md5 str)
  (call-with-input-string str md5))

(define (bytevector->md5 bv)
  (call-with-input-string ((@ (rnrs) utf8->string) bv) md5))
;; arch-tag: 03A57FCF-F9E7-11D8-A6BC-000A95CD5044
