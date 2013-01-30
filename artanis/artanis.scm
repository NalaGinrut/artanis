-*-  indent-tabs-mode:nil;  -*-
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
  #:use-module (ice-9 regex)
  #:use-module (web uri)
  #:use-module (web request)
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

(define *path-regexp* (make-regexp ":[^\\/]+")) ; compiled regexp for optimization   

(define (compile-path path)
  (string-append "^" 
                 (regexp-substitute/global #f *path-regexp* path 'pre "([^/?]+)" 'post)
                 "([^/]?$)"))

(define *key-regexp* (make-regexp "([^ ]+) ([^ ]+)"))

(define (request->matching-key request)
  (define (key-matches-route? key)
    (let* ((m (regexp-exec *key-regexp* key))
           (method (match:substring m 1))
           (path-regexp (match:substring m 2)))
      (and (eq? (request-method request) (string->symbol method))
           (regexp-exec (make-regexp path-regexp)
                        (uri-path (request-uri request))))))
  (any (lambda (k) (and (key-matches-route? k)
                        (match:substring k 1)))
       (hash-keys request-handlers)))

(define (request->handler-keys request)
  (let ((handler-key (request->matching-key request)))
    (and handler-key
         (hash-ref request-handlers handler-key))))


  (define handler-key (request->matching-key request))
  (case handler-key
    [(#f) #f]
    [else (hash-ref request-handlers handler-key #f)]))

(define (request->key-bindings request keys)
  (define path-regexp
    (second (regexp-split #rx" " (request->matching-key request))))
  (define bindings (cdr (regexp-match path-regexp (url->string (request-uri request)))))
  (for/list ([key keys] [binding bindings])
            (cons key binding)))
(define run
  #t)

(define (params request key)
  (let ((qstr (uri-query (request-uri request)))
        (body (utf8->string (read-request-body request)))
        (keys ()
    
    

(define (params request key)
  (define query-pairs (url-query (request-uri request)))
  (define body-pairs
    (match (request-post-data/raw request)
      [#f empty]
      [body (url-query (string->url (string-append "?" (bytes->string/utf-8 body))))]))
  (define url-pairs
    (let ([keys (cdr (request->handler/keys request))])
      (request->key-bindings request keys)))
  (hash-ref (make-hash (append query-pairs body-pairs url-pairs)) key ""))




(define (render/handler handler request)
  (define content
    (case (procedure-arity handler)
      [(1) (handler request)]
      [else (handler)]))
  (define status
    (cond [(list? content) (first content)]
          [else 200]))
  (define headers
    (cond [(list? content) (second content)]
          [else '()]))
  (define body
    (cond [(list? content) (third content)]
          [else content]))

  (response/full status
                 (status->message status)
                 (current-seconds)
                 TEXT/HTML-MIME-TYPE
                 headers
                 (list (string->bytes/utf-8 body))))

(define (request->handler request)
  (let ((handler/keys (request->handler/keys request)))
    (cond
     (handler/keys (render/handler (car handler/keys) request))
     (else (render/404)))))


