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
  #:export (make-cookie cookie? cookie-set! cookie-ref
            ->HTTP-cookie new-cookie request-cookie get-cookie-file))

(define *global-cookies-table* (make-hash-table))
  
(define-record-type cookie
  (make-cookie nvpt ic)
  cookie?
  (nvpt cookie-nvpt cookie-nvpt!) ; name-value-pairs hashtable
  (ic cookie-inner cookie-inner!)); inner cookie

;; inner cookie, you shouldn't use it directly, try new-cookie
(define-record-type inner-cookie
  (make-inner-cookie nvp expir path domain secure ho)
  inner-cookie?
  (nvp cookie-nvp cookie-nvp!)          ; Name-Value-Pairs of the cookie
  (expir cookie-expir cookie-expir!)    ; The expiration in Greenwich Mean Time
  (path cookie-path cookie-path!)       ; The path the cookie is good for
  (domain cookie-domain cookie-domain!) ; The domain the cookie is good for
  ;; keep cookie communication limited to encrypted transmission
  (secure cookie-secure cookie-secure!) ; The secure need of cookie
  (ho cookie-httponly cookie-httponly!)); http-only

(define (cookie-dump cookie)
  (let ((nvpt (cookie-nvpt cookie))
        (ic (cookie-ic cookie)))
    (cookie-nvp! ic (hash-map->list list nvpt))
    ic))

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
(define (is-cookie-keywords? str)
  (member *cookie-keywords*))

(define (get-from-cookie al key)
  (let ((val (assoc-ref al key)))
    (and val (cadr val))))

(define (head-string->cookie str)
  (let* ((ll (map (lambda (e) 
                    (if (string-contains e "=") 
                        (string-split e #\=) 
                        e)) 
                  str))
         (ntp (let lp((rest '()) (result '()))
                (cond ((or (is-cookie-keywords? rest) (null? reset))
                       result) ; drop the pair after keyword-value-pair
                      (else (lp (cdr rest) (cons (car rest) result))))))
         (cookie (new-cookie #:expires (get-from-cookie ll "Expires")
                             #:path (get-from-cookie ll "Path")
                             #:domain (get-from-cookie ll "Domain")
                             #:secure (get-from-cookie ll "Secure")
                             #:http-only (get-from-cookie ll "HttpOnly"))))
    (cookie-nvpt! cookie (alist->hashtable ntp)) ; generate a table for easy ref
    cookie))       

(define (cookie->header-string inner-cookie)
  (let ((nvps (append-map
               (lambda (nvp-ref)
                 (filter-map nvp->string (nvp-ref inner-cookie)))
               nvp-accessors)))
    (string-join nvps ";")))

(define* (new-cookie #:key (expires 3600) ; expires in seconds
                     (path #f) (domain #f)
                     (secure #f) (http-only #t))
  (let* ((ht (make-hash-table))
         (e (cond ((string? expires) expires) ; TODO: need validate
                  ((integer? expires) (make-expires expires))
                  (else #f))); else #f for no expires
         (cookie (make-inner-cookie '() e path domain secure http-only)))
    (make-cookie ht cookie)))

(define (cookie-set! cookie name value)
  (let ((nvpt (cookie-nvpt cookie)))
    (hash-set! nvpt name value)))

(define (cookie-ref cookie name)
  (let ((nvpt (cookie-nvpt cookie)))
    (hash-ref nvpt name)))

(define (cookie-delete! cookie name)
  (let ((nvpt (cookie-nvpt cookie)))
    (hash-remove! nvpt name)))

(define (->HTTP-cookie cookie)
  (cookie->header-string (cookie-dump cookie)))

(define (request-cookie req)
  (let ((cookie-str (assoc-ref (request-headers req) 'cookie)))
    (and cookie-str (head-string->cookie cookie-str))))    

(define* (set-cookie #:key (expires #f) (path #f) (domain #f)
                    (secure #f) (http-only #t))
  (let ((cookie (current-session-cookie)))
    ;;(cookies-set! cookie  
    #t))

(define (get-cookie-file cid)
  (let ((f (format #f "~a/~a.cookie" *cookie-path* cid)))
    (and (file-exists? f) f)))
