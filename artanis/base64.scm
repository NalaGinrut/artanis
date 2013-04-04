;;  Copyright (C) 2013
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  Artanis is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  Artanis is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; TODO:
;; This base64 implementation is not so good.
;; But it's one of memo in HFG-alpha time when it's the best code of mine.
;; I'll fix it later. 
;; God bless hacking.

(define-module (artanis base64)
  :export (base64-encode base64-decode))

;; FIXME: 
;; it can't accept any punctuation like "!>."
;; (base64-encode "happy hacking") is OK.
;; (base64-encode "happy hacking!") throws error.

(define encode-str "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
(define bad-char-str "\x00\x01\x02\x03\x04\x05\x06\a\b\t\v\f\r\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f !\"#$%&'()*,-.:;<~\x7f")

(define get-div car)
(define get-mod cdr)
(define div-and-mod (@ (rnrs base) div-and-mod))
(define div (@ (rnrs base) div))

(define (mk-div-mod x n)
  (call-with-values
      (lambda () (div-and-mod x n))
    cons))

(define get-qindex
  (lambda (q n)
    (char->integer (string-ref q n))))

(define encode-char
  (lambda (index)
    (string-ref encode-str index)))

(define encode-quantum
  (lambda (quantum)
    (let* ((i1 (get-qindex quantum 0))
	   (i2 (get-qindex quantum 1))
	   (i3 (get-qindex quantum 2))
	   (c1 (encode-char
		(logand (ash i1 -2) #b111111)))
	   (c2 (encode-char
		(logior (ash (logand i1 #b11) 4)
		       (ash (logand i2 #b11110000) -4))))
	   (c3 (encode-char
		(logior (ash (logand i2 #b1111) 2)
		       (ash (logand i3 #b11000000) -6))))
	   (c4 (encode-char
		(logand i3 #b111111))))
      (list c1 c2 c3 c4))))

(define encode-m1
  (lambda (rest)
    (let* ((i1 (get-qindex rest 0))
	   (c1 (encode-char
		(ash (logand i1 #b11111100) -2)))
	   (c2 (encode-char
		(ash (logand i1 #b11) 4)))
	   )
      (list c1 c2 #\= #\=))))
		    
(define encode-m2
  (lambda (rest)
    (let* ((i1 (get-qindex rest 0))
	   (i2 (get-qindex rest 1))
	   (c1 (encode-char
		(ash (logand i1 #b11111100) -2)))
	   (c2 (encode-char
		(logior (ash (logand i1 #b11) 4)
			(ash (logand i2 #b11110000) -4))))
	   (c3 (encode-char
		(ash (logand i2 #b1111) 2))))
      (list c1 c2 c3 #\=))))

;; FIXME: string-index is too slow. I need an efficient trans-table to
;;        decode.
(define decode-char 
  (lambda (char)
    (if (char=? char #\=)
	0
	(string-index encode-str char))))
	  
(define decode-quantum
  (lambda (quantum)
    (let* ((i1 (decode-char (string-ref quantum 0)))
	   (i2 (decode-char (string-ref quantum 1)))
	   (i3 (decode-char (string-ref quantum 2)))
	   (i4 (decode-char (string-ref quantum 3)))
	   (c1 (integer->char
		(logior 
		 (logand (ash i1 2) #xFF)
		 (ash i2 -4))))
	   (c2 (integer->char
		(logior 
		 (logand (ash i2 4) #xFF)
		 (ash i3 -2))))
	   (c3 (integer->char
		(logior 
		 (logand (ash i3 6) #xC0) i4))))
      (list c1 c2 c3))))

(define base64-encode
  (lambda (str)
    (let* ((str-len (string-length str))
	   (dm (mk-div-mod str-len 3))
	   (d (get-div dm))
	   (m (get-mod dm))
	   (i 0)
	   (result '()))
      (while (< i d)
	     (set! result
		   (append result
			   (encode-quantum
			    (string-copy str (* 3 i) (* 3 (1+ i))))))
	     (set! i (1+ i)))
      (cond
       ((= m 0) (list->string result))
       ((= m 1) (list->string 
		 (append result 
			 (encode-m1
			  (string-copy str
				       (* 3 i)
				       str-len)))))
       ((= m 2) (list->string 
		 (append result 
			 (encode-m2
			  (string-copy str
				       (* 3 i)
				       str-len)))))
       (else
	(error base64-encode "Impossible error!"))))))

(define base64-decode
  (lambda (str)
    (cond 
     ((not (string? str)) (error base64-decode
                                 "Sorry dude, but I need a string!"))
     ((not 
       (string-null? (string-filter
                      (lambda (c) 
                        (string-contains bad-char-str (string c)))
                      str)))
      (error base64-decode
             "Hey dude, I don't eat this bizarre string!")))
    (let* ((i 0)
	   (result '())
	   (d (div (string-length str) 4)))
      (while (< i d)
        (set! result
		   (append result
			   (decode-quantum
			    (string-copy str (* 4 i) (* 4 (1+ i))))))
	     (set! i (1+ i)))
      (set! result (list->string result))
      (string-copy result 0 (string-index result #\x0)))))
