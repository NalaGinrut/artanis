;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014,2015,2017
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

(define-module (artanis route)
  #:use-module (artanis utils)
  #:use-module (artanis cookie)
  #:use-module (artanis env)
  #:use-module (artanis irregex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (web uri)
  #:use-module (web request)
  #:export (make-handler-rc
            handler-rc?
            handler-rc-handler
            handler-rc-keys
            handler-rc-oht
            get-handler-rc

            rc-handler rc-handler!
            rc-keys rc-keys!
            rc-re rc-re!
            rc-req rc-req!
            rc-path rc-path!
            rc-qt rc-qt!
            rc-method rc-method!
            rc-rhk rc-rhk!
            rc-bt rc-bt!
            rc-body rc-body!
            rc-mtime rc-mtime!
            rc-cookie rc-cookie!
            rc-set-cookie rc-set-cookie!
            rc-conn rc-conn!
            new-route-context
            route-context?

            get-header
            init-rule-handler-key!
            init-rule-handler-and-keys!
            init-rule-path-regexp!
            init-rule-key-bindings!
            init-query!
            get-from-qstr))

(define-record-type handler-rc
  (make-handler-rc handler keys oht)
  handler-rc?
  (handler handler-rc-handler)
  (keys handler-rc-keys)
  (oht handler-rc-oht))

(define (get-handler-rc handler-key)
  (hash-ref *handlers-table* handler-key))

(define-record-type route-context
  (make-route-context handler keys regexp request path qt method rhk bt 
                      body date cookie set-cookie conn)
  route-context?
  (handler rc-handler rc-handler!) ; reqeust handler
  (keys rc-keys rc-keys!) ; rule keys
  (regexp rc-re rc-re!) ; regexp to parse key-bindings
  (request rc-req rc-req!) ; client request
  ;; FIXME: actually we don't need this redundant path,
  ;;        it's better to get from request. 
  (path rc-path rc-path!) ; path from uri
  (qt rc-qt rc-qt!) ; query table
  ;; FIXME: the current Guile inner server treat HEAD as GET, so we
  ;;        need rc-method, but it's trivial when we have new server core.
  (method rc-method rc-method!) ; request method
  (rhk rc-rhk rc-rhk!) ; rule handler key in handlers-table
  (bt rc-bt rc-bt!) ; bindings table
  (body rc-body rc-body!) ; request body
  (date rc-mtime rc-mtime!) ; modified time, users need to set it in handler
  (cookie rc-cookie rc-cookie!) ; the cookie parsed from header string
  (set-cookie rc-set-cookie rc-set-cookie!) ; the cookies needed to be set as response
  ;; auto DB connection doesn't need users to close it, it's auto closed when request is over.
  (conn rc-conn rc-conn!)) ; auto DB connection from pool

(define (get-header rc k)
  ;; (display (request-headers (rc-req rc)))(newline)
  (assq-ref (request-headers (rc-req rc)) k))

(define (new-route-context request body)
  (let* ((uri (request-uri request))
         (path (uri-path uri))
         (m (valid-method? (request-method request)))
         ;; NOTE: sanitize-response will handle 'HEAD method
         ;;       though rc-method is 'GET when request-method is 'HEAD,
         ;;       sanitize-response only checks method from request
         (method (if (eq? m 'HEAD) 'GET m))
         (cookies (request-cookies request))
         (rc (make-route-context #f #f #f request path #f method #f #f
                                 body #f cookies '() #f)))
    ;; FIXME: maybe we don't need rhk? Throw it after get handler & keys
    (init-rule-handler-key! rc) ; set rule handler key
    (init-rule-handler-and-keys! rc) ; set handler and keys
    (init-rule-path-regexp! rc) ; set regexp
    (init-rule-key-bindings! rc) ; key binding of path
    (init-query! rc) ; init query-string and post body
    rc))

;; find & set the key of rule-handler,
;; which is used to find the (handler . keys)
;; FIXME: each method should have a own table
;; FIXME: use better data structure other than hashtable to make it faster
(define (init-rule-handler-key! rc)
  (define rmtd (rc-method rc))
  (define path (rc-path rc))
  (define (key-matches-route? pattern)
    (let ((method (car pattern))
          (path-regexp (cdr pattern)))
      (and (eq? rmtd method)
           (irregex-match path-regexp path))))
  (rc-rhk! rc (find key-matches-route? (hash-keys *handlers-table*))))

;; find&set! the rule handler to rc
(define (init-rule-handler-and-keys! rc)
  (let* ((handler-key (rc-rhk rc))
         (hrc (if handler-key  ; get handler-keys pair
                  (get-handler-rc handler-key)
                  (throw 'artanis-err 404 init-rule-handler-and-keys!
                         "invalid handler key ~a" handler-key))))
    (rc-handler! rc (handler-rc-handler hrc))
    (rc-keys! rc (handler-rc-keys hrc))))

(define (init-rule-path-regexp! rc)
  (rc-re! rc (string->irregex (cdr (rc-rhk rc)))))

;; init key-bindings table
(define (init-rule-key-bindings! rc)
  (let ((m (irregex-search (rc-re rc) (rc-path rc))))
    (rc-bt! rc
            (map (lambda (k i) (cons k (irregex-match-substring m i))) 
                 (rc-keys rc) (iota (irregex-match-num-submatches m) 1)))))

(define (init-query! rc)
  ;; NOTE: All the prefix/postfix ":" in query/post keys are trimmed.
  ;;       Because only rule keys can use such naming.
  (define (-> x)
    (string-trim-both x (lambda (c) (member c '(#\sp #\: #\return)))))
  (let ((str (case (rc-method rc)
               ((GET) (uri-query (request-uri (rc-req rc))))
               ;; The accessor of GET and POST should be divided
               ((POST) #f) ; don't handle post here
               (else (throw 'artanis-err 405 
                            "wrong method for query!" (rc-method rc))))))
    (if str
        (rc-qt! rc (map (lambda (x) 
                          (map -> (string-split x #\=)))
                        (string-split str #\&)))
        '())))

;; ENHANCE: do we need query hashtable?
(define (get-from-qstr rc key)
  (and (rc-qt rc)
       (and=> (assoc-ref (rc-qt rc) key) car)))
