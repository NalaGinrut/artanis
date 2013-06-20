;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
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

(define-module (artanis cookie)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (web request)
  #:export (make-cookie cookie? cookie-set! cookie-ref generate-cookies
            cookie->header-string new-cookie request-cookies cookie-expired?
            has-cookie?))

;; inner cookie, you shouldn't use it directly, try new-cookie
(define-record-type cookie
  (make-cookie nvp expir path domain secure ho)
  cookie?
  (nvp cookie-nvp cookie-nvp!)          ; Name-Value-Pairs of the cookie
  (expir cookie-expir cookie-expir!)    ; The expiration in Greenwich Mean Time
  (path cookie-path cookie-path!)       ; The path the cookie is good for
  (domain cookie-domain cookie-domain!) ; The domain the cookie is good for
  ;; keep cookie communication limited to encrypted transmission
  (secure cookie-secure cookie-secure!) ; The secure need of cookie
  (ho cookie-httponly cookie-httponly!)); http-only

(define (cookie-expired? cookie)
  (time-expired? (cookie-expir cookie)))

(define (nvp name v-ref)
  (lambda (c)
  (list (list name (v-ref c)))))

(define (nvp->string nvp)
  (let ((v (cadr nvp)))
    (if (boolean? v)
         (and v (car nvp))
         (format #f "~a=~a" (car nvp) v))))

(define nvp-accessors
  (list cookie-nvp
        (nvp "Expires" cookie-expir)
        (nvp "Path" cookie-path)
        (nvp "Domain" cookie-domain)
        (nvp "Secure" cookie-secure)
        (nvp "HttpOnly" cookie-httponly)))

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

(define (head-string->cookie str)
  (let* ((ll (map (lambda (e) 
                    (if (string-contains e "=") 
                        (map string-trim-both (string-split e #\=)) 
                        (string-trim-both e))) 
                  (string-split str #\;)))
         (nvp (let lp((rest ll) (result '()))
                (cond ((or (null? rest) (is-cookie-keywords? (car rest)))
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
  (let ((nvps (append-map
               (lambda (nvp-ref)
                 (filter-map nvp->string (cookie-nvp cookie)))
               nvp-accessors)))
    (string-join nvps ";")))

(define (generate-cookies cookies)
  (map (lambda (c) `(set-cookie . ,(cookie->header-string c))) cookies))

(define* (new-cookie #:key (expires 3600) ; expires in seconds
                     (npv '())
                     (path #f) (domain #f)
                     (secure #f) (http-only #t))
  (let ((e (cond ((string? expires) expires) ; TODO: need validate
                 ((integer? expires) (make-expires expires))
                 (else #f)))); else #f for no expires
    (make-cookie npv e path domain secure http-only)))
    
(define (cookie-set! cookie name value)
  (let ((nvp (cookie-nvp cookie)))
    (cookie-nvp! cookie (assoc-set! nvp name value))))

(define (cookie-ref cookie name)
  (let* ((nvp (cookie-nvp cookie))
         (v (assoc-ref nvp name)))
    (and v (car v))))

(define (cookie-delete! cookie name)
  (let ((nvp (cookie-nvp cookie)))
    (cookie-nvp! cookie (assoc-remove! nvp name))))

(define (header->cookies header)
  (fold (lambda (x p)
          (if (eqv? 'set-cookie (car x))
              (cons (cdr x) p)
              p))
        '() header))

(define (request-cookies req)
  (let ((cookies-str (header->cookies (request-headers req))))
    (map head-string->cookie cookies-str)))

(define (has-cookie? ck key)
  (let ((c (any (lambda (x) (cookie-ref x key)) ck)))
    (and c (not (cookie-expired? c)) c)))
