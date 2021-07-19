;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2017,2018,2021
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

(define-module (artanis cookie)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-cookie
            cookie?
            cookie-set!
            cookie-ref
            cookie-delete!
            generate-cookies
            header-string->cookie
            cookie->header-string
            new-cookie
            request-cookies
            cookie-has-key?
            get-value-from-cookie-name
            cookies-maker
            cookie-modify))

;; NOTE: server side never check cookie expires, it's client's duty
;;       server only check sessions expires

;; inner cookie, you shouldn't use it directly, try new-cookie
(define-record-type cookie
  (make-cookie nvp expires domain path secure http-only)
  cookie?
  (nvp cookie-nvp cookie-nvp!)          ; Name-Value-Pairs of the cookie
  (expires cookie-expires cookie-expires!) ; The expiration in Greenwich Mean Time
  (domain cookie-domain cookie-domain!) ; The domain the cookie is good for
  (path cookie-path cookie-path!)       ; The path the cookie is good for
  ;; keep cookie communication limited to encrypted transmission
  (secure cookie-secure cookie-secure!) ; The secure need of cookie
  (http-only cookie-httponly cookie-httponly!)) ; http-only

;; NOTE: expires should be positive integer
(define* (cookie-modify ck #:key expires domain path secure http-only)
  (and expires (positive? expires) (cookie-expires! ck (make-expires expires)))
  (and domain (cookie-domain! ck domain))
  (and path (cookie-path! ck path))
  (and secure (cookie-secure! ck secure))
  (and http-only (cookie-httponly! ck http-only)))

(define (nvp name v-ref)
  (lambda (c)
    (list (list name (v-ref c)))))

(define (nvp->string nvp)
  (let ((v (cdr nvp)))
    (if (boolean? v)
        (if v (car nvp) (format #f "~a=\"\"" (car nvp)))
        (format #f "~a=~a" (car nvp) v))))

(define *cookie-keywords*
  '("Expires" "Path" "Domain" "Secure" "HttpOnly"))

;; NOTE: string comparing has to use 'equal?' which is 'member' used.
;;       according to R5Rs
(define (is-cookie-keywords? item)
  (or (not (list? item))
      (member (car item) *cookie-keywords* string=?)))

(define (get-from-cookie al key)
  (let ((val (assoc-ref al key)))
    (and val (car val))))

(define (header-string->cookie str)
  (let* ((ll (map (lambda (e)
                    (if (string-contains e "=")
                        (map string-trim-both (string-split e #\=))
                        (string-trim-both e)))
                  (string-split str #\;)))
         (nvp (let lp((rest ll) (result '()))
                (cond
                 ((or (null? rest) (is-cookie-keywords? (car rest)))
                  result) ; drop the pair after keyword-value-pair
                 (else (lp (cdr rest) (cons (car rest) result))))))
         (cookie (new-cookie #:expires (get-from-cookie ll "Expires")
                             #:path (get-from-cookie ll "Path")
                             #:domain (get-from-cookie ll "Domain")
                             #:secure (get-from-cookie ll "Secure")
                             #:http-only (get-from-cookie ll "HttpOnly"))))
    (cookie-nvp! cookie nvp) ; insert cookie key-value pair table
    cookie))

(define (cookie->header-string cookie)
  (let ((nvps (string-join (map nvp->string (cookie-nvp cookie)) "; "))
        (expires (cookie-expires cookie))
        (path (cookie-path cookie))
        (domain (cookie-domain cookie))
        (secure (cookie-secure cookie))
        (http-only (cookie-httponly cookie)))
    (call-with-output-string
      (lambda (port)
        (format port "~a" nvps)
        (and expires (format port "; Expires=~a" expires))
        (and domain (format port "; Domain=~a" domain))
        (and path (format port "; Path=~a" path))
        (and secure (display "; Secure" port))
        (and http-only (display "; HttpOnly" port))))))

(define (generate-cookies cookies)
  (map (lambda (c) `(set-cookie . ,(cookie->header-string c))) cookies))

(define *the-remove-expires* "Thu, 01 Jan 1970 00:00:00 GMT")

;; NOTE: expires should be integer
;; NOTE: When we set #:expires to #t for removing the cookie, #:path MUST be "/".
;;       Otherwise the client may not clear the cookie correctly.
(define* (new-cookie #:key (expires (get-conf '(cookie expire))) ; expires in seconds
                     (npv '())
                     (path #f) (domain #f)
                     (secure #f) (http-only #t))
  (let ((e (cond
            ((eq? expires #t) *the-remove-expires*)
            ((integer? expires) (make-expires expires))
            (else #f)))) ; else #f for no expires
    (make-cookie npv e domain path secure http-only)))

(define (cookie-set! cookie name value)
  (let ((nvp (cookie-nvp cookie)))
    (cookie-nvp! cookie (assoc-set! nvp name value))))

(define (cookie-ref cookie name)
  (when (not (cookie? cookie))
    (throw 'artanis-err 500 cookie-ref "BUG: Invalid cookie: ~a!" cookie))
  (let ((nvp (cookie-nvp cookie)))
    (assoc-ref nvp name)))

(define (cookie-delete! cookie name)
  (let ((nvp (cookie-nvp cookie)))
    (cookie-nvp! cookie (assoc-remove! nvp name))))

(define (header->cookies header)
  ;;(format #t "header==> ~a~%" header)
  (fold (lambda (x p)
          (if (eqv? 'cookie (car x))
              (cons (cdr x) p)
              p))
        '() header))

(define (request-cookies req)
  (let ((cookies-str (header->cookies (request-headers req))))
    ;;(format #t "cookies-str: ~a~%" cookies-str)
    (map header-string->cookie cookies-str)))

(define (cookie-has-key? ckl key)
  (any (lambda (x) (and (cookie-ref x key) x)) ckl))

(define (get-value-from-cookie-name ckl key)
  (any (lambda (x) (and=> (cookie-ref x key) car)) ckl))
