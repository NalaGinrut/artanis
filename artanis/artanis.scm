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
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 regex)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:export (get post put patch delete params header run))

;; table structure:
;; '((rule-handler-key (handler . keys)) ...)
;; for example:
;; `(("GET \"/photo/:id/edit\"" (,(lambda (req ..) ...) . id)))  
(define *handlers-table* (make-hash-table))

(define (define-handler method rule handler)
  (let ((keys (rule->keys rule))
        (path-regexp (compile-rule rule)))
    (hash-set! *handlers-table*
               (string-append method " " path-regexp)
               (cons handler keys))))

(define (get rule handler) (define-handler "GET" rule handler))
(define (post rule handler) (define-handler "POST" rule handler))
(define (put rule handler) (define-handler "PUT" rule handler))
(define (patch rule handler) (define-handler "PATCH" rule handler))
(define (delete rule handler) (define-handler "DELETE" rule handler))

(define-record-type route-context
  (make-route-context handler keys regexp request path 
                      qt method rhk bt body)
  route-context?
  (handler rc-handler rc-handler!) ; reqeust handler
  (keys rc-keys rc-keys!) ; rule keys
  (regexp rc-re rc-re!) ; regexp to parse key-bindings
  (request rc-req rc-req!) ; client request
  (path rc-path rc-path!) ; path from uri
  (qt rc-qt rc-qt!) ; query table
  (method rc-method rc-method!) ; request method
  (rhk rc-rhk rc-rhk!) ; rule handler key in handlers-table
  (bt rc-bt rc-bt!) ; bindings table
  (body rc-body rc-body!)) ; request body

;; compiled regexp for optimization
(define *rule-regexp* (make-regexp ":[^\\/]+"))    
(define *path-keys-regexp* (make-regexp "/:([^\\/]+)"))
(define *key-regexp* (make-regexp "([^ ]+) (.+)"))

;; parse rule-string and generate the regexp to parse keys from path-string
(define (rule->keys rule)
  (map (lambda (m) (string->symbol (match:substring m 1)))
       (list-matches *path-keys-regexp* rule)))

(define (compile-rule rule)
  (string-append "^" 
                 (regexp-substitute/global 
                  #f *rule-regexp* rule 'pre "([^\\/\\?]+)" 'post)
                 "[^ ]?"))

;; find & set the key of rule-handler,
;; which is used to find the (handler . keys)
;; FIXME: each method should have a own table
(define (init-rule-handler-key! rc)
  (define rmtd (rc-method rc))
  (define path (rc-path rc))
  (define (key-matches-route? pattern)
    (let* ((ml (regexp-split *key-regexp* pattern))
           (method (cadr ml))
           (path-regexp (caddr ml)))
      (and (eq? rmtd (string->symbol method))
           (regexp-exec (make-regexp path-regexp) path))))
  (rc-rhk! rc (find key-matches-route? (hash-keys *handlers-table*))))

;; find&set! the rule handler to rc
(define (init-rule-handler-and-keys! rc)
  (let* ((handler-key (rc-rhk rc))
         (hkp (if handler-key  ; get handler-keys pair
                  (hash-ref *handlers-table* handler-key)
                  (error "invalid handler key" handler-key))))
    (rc-handler! rc (car hkp))
    (rc-keys! rc (cdr hkp))))

(define (init-rule-path-regexp! rc)
  (rc-re! rc (caddr (regexp-split *key-regexp* (rc-rhk rc)))))

;; init key-bindings table
(define (init-rule-key-bindings! rc)
  (let ((m (string-match (rc-re rc) (rc-path rc))))
    (rc-bt! rc
            (map (lambda (k i) (cons k (match:substring m i))) 
                 (rc-keys rc) (iota (1- (match:count m)) 1)))))

(define (init-query! rc)
  (let ((qstr (case (rc-method rc)
                ((GET) (uri-query (request-uri (rc-req rc))))
                ((POST) (rc-body rc))
                (else (error "wrong method for query!" (rc-method rc))))))
    (rc-qt! rc (map (lambda (x) (string-split x #\=))
                    (string-split qstr #\&)))))

;; parse query while needed
;; ENHANCE: do we need query hashtable?
(define (query rc key)
  (unless (rc-qt rc) (init-query! rc))
  (car (assoc-ref (rc-qt rc) key)))

;; parse params while needed
;; ENHANCE: do we need query hashtable?
(define (params rc key)
  (unless (rc-bt rc) (init-rule-key-bindings! rc))
  (assoc-ref (rc-bt rc) key))

(define sys-page-path (make-parameter "./"))
(define (sys-page-show file port)
  (bv-cat (string-append (sys-page-path) "/" file) port))

(define (render-sys-page status)
  (format #t "response: ~a ,~a ~% ~a~%" status '() status)
  (values
   (build-response #:code status
                   #:headers '((content-type . (text/html))
                               (charset . "utf-8")))
   (lambda (port)
     (sys-page-show (format #f "~a.html" status) port))))

;; obsoleted, use render-sys-page instead
(define (render-404)
  (values
   (build-response #:code 404
                   #:headers '((content-type . (text/html))
                               (charset . "utf-8")))
   (lambda (port)
     (display "not found!" port))))

(define (handler-render handler rc)
  (call-with-values
      (lambda ()
        (if (thunk? handler) 
            (handler) 
            (handler rc)))
    (lambda (status headers body)
      (format #t "response: ~a ,~a ~% ~a~%" status headers body)
      (values
       (build-response #:code (or status 200)
                       #:headers `((content-type . (text/html)) 
                                   (charset . "utf-8")
                                   ,@(or headers '())))
       (lambda (port)
         (display body port))))))

(define (new-route-context request body)
  (let* ((uri (request-uri request))
         (path (uri-path uri))
         (method (request-method request))
         (rc (make-route-context #f #f #f request path #f method #f #f body)))
    ;; FIXME: maybe we don't need rhk? Throw it after get handler & keys
    (init-rule-handler-key! rc) ; set rule handler key
    (init-rule-handler-and-keys! rc) ; set handler and keys
    (init-rule-path-regexp! rc) ; set regexp
    rc))

(define (work-with-request request body)
  (let* ((rc (new-route-context request body))
         (handler (rc-handler rc)))
    (if handler 
        (catch #t (lambda () (handler-render handler rc))
          (lambda (k . e) (format #t "~a: ~a~%" k e) (render-404)))
        (render-404)))) ; FIXME: render 500

(define (server-handler request request-body)
  (get "/favicon.ico" (lambda () (values 404 '() ""))) ; FIXME: fix it
  (work-with-request request request-body))

(define* (run #:key (port 3000))
  (run-server server-handler 'http `(#:port ,port)))


