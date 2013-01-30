;;  -*-  indent-tabs-mode:nil;  -*-
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

(define-module (artanis artanis)
  #:use-module (artanis utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:export (get post put patch delete params header run))

(define request-handlers (make-hash-table))

(define (define-handler method path handler)
  (let ((keys (path->keys path))
        (path-regexp (compile-path path)))
    (hash-set! request-handlers
               (string-append method " " path-regexp)
               (cons handler keys))))

(define (get path handler) (define-handler "GET" path handler))
(define (post path handler) (define-handler "POST" path handler))
(define (put path handler) (define-handler "PUT" path handler))
(define (patch path handler) (define-handler "PATCH" path handler))
(define (delete path handler) (define-handler "DELETE" path handler))

(define *path-keys-regexp* (make-regexp "/:([^\\/]+)"))

(define (path->keys path)
  (map (lambda (m) (string->symbol (match:substring m 1)))
       (list-matches *path-keys-regexp* path)))

;; compiled regexp for optimization
(define *path-regexp* (make-regexp ":[^\\/]+"))    

(define (compile-path path)
  (string-append "^" 
                 (regexp-substitute/global 
                  #f *path-regexp* path 'pre "([^/?]+)" 'post)
                 "([^/]?$)"))

(define *key-regexp* (make-regexp "([^ ]+) ([^ ]+)"))

(define (request->matching-key request)
  (define (key-matches-route? pattern)
    (let* ((m (regexp-exec *key-regexp* pattern))
           (method (match:substring m 1))
           (path-regexp (match:substring m 2)))
      (and (eq? (request-method request) (string->symbol method))
           (regexp-exec (make-regexp path-regexp)
                        (uri-path (request-uri request))))))
  (find key-matches-route? (hash-keys request-handlers)))

(define (request->handler-keys request)
  (let ((handler-key (request->matching-key request)))
    (and handler-key
         (hash-ref request-handlers handler-key))))

(define (request->key-bindings request keys)
  (let* ((path-regexp 
          (cadr (regexp-split " "
                              (request->matching-key request))))
         (uri (request-uri request))
         (m (string-match path-regexp (uri-path uri))))
    (map (lambda (k i) (cons k (match:substring m i))) 
         keys (iota (1- (match:count m)) 1))))

(define (request->key-value request keys)
  (let* ((path-regexp 
          (cadr (regexp-split " "
                              (request->matching-key request))))
         (uri (request-uri request))
         (m (string-match path-regexp (uri-path uri))))
    (map (lambda (i) (match:substring m i)) 
         (iota (1- (match:count m)) 1))))

(define (params request key)
  (let* ((method (request-method request))
         (qstr (case method
                 ((GET) (uri-query (request-uri request)))
                 ((POST) (read-request-body request))
                 (else (error "wrong method for params! method"))))
         (ql (map (lambda (x) (string-split x #\=))
                  (string-split qstr #\&)))
         (keys (request->key-bindings
                request (cdr (request->handler-keys request)))))
    (assoc-ref (list ql keys) key)))

(define (render-404)
  (values
   (build-response #:code 404
                   #:headers '((content-type . (text/html))
                               (charset . "utf-8")))
   (lambda (port)
     (display "not found!" port))))

(define (handler-render handler request)
  (let* ((kv (request->key-value request (cdr (request->handler-keys request))))
         (content (if (thunk? handler) 
                      (handler) 
                      (apply handler `(,request ,kv))))
         (status (if (list? content) (first content) 200))
         (headers (if (list? content) (second content) '()))
         (body (if (list? content) (third content) content)))
    (values
     (build-response #:code status
                     #:headers `((content-type . (text/html)) 
                                 (charset . "utf-8")
                                 ,@headers))
     (lambda (port)
       (display body port)))))

(define (request->handler request)
  (let ((handler-keys (request->handler-keys request)))
    (if handler-keys 
        (handler-render (car handler-keys) request)
        (render-404))))

(define (server-handler request request-body)
  (request->handler request))

(define* (run #:key (port 3000))
  (run-server server-handler 'http `(#:port ,port)))


