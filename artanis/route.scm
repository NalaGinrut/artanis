;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014,2015,2017,2018,2019
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
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
  #:export (make-handler-context
            handler-context?
            handler-context-handler
            handler-context-keys
            handler-context-uid
            handler-context-oht
            get-handler-context

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
            rc-oht rc-oht!
            rc-conn rc-conn!
            rc-lpc rc-lpc!
            new-route-context
            route-context?

            get-header
            init-rule-handler-key!
            init-rule-handler-and-keys!
            init-rule-path-regexp!
            init-rule-key-bindings!
            init-query!
            get-from-qstr
            get-referer
            rc-oht-ref
            params
            get-rule-uid
            get-sid-from-client-cookie))

(define-record-type handler-context
  (make-handler-context handler keys uid oht)
  handler-context?
  (handler handler-context-handler)
  (keys handler-context-keys)
  ;; NOTE: We record an uid of each raw-rule so that we don't have to match the rule
  ;;       redundantly.
  (uid handler-context-uid)
  (oht handler-context-oht))

(define (get-handler-context handler-key)
  (hash-ref *handlers-table* handler-key))

(define-record-type route-context
  (make-route-context handler keys regexp request path qt method rhk bt
                      body date cookie set-cookie oht conn lpc)
  route-context?
  (handler rc-handler rc-handler!) ; reqeust handler
  (keys rc-keys rc-keys!) ; rule keys
  (regexp rc-re rc-re!) ; regexp to parse key-bindings
  (request rc-req rc-req!) ; client request
  ;; FIXME: actually we don't need this redundant path,
  ;;        it's better to get from request.
  (path rc-path rc-path!) ; path from uri, all the trailing "/" were dropped.
  (qt rc-qt rc-qt!) ; query table
  ;; FIXME: the current Guile inner server treat HEAD as GET, so we
  ;;        need rc-method, but it's trivial when we have new server core.
  (method rc-method rc-method!) ; request method
  (rhk rc-rhk rc-rhk!) ; rule handler key in handlers-table
  (bt rc-bt rc-bt!) ; bindings table
  (body rc-body rc-body!) ; request body
  (date rc-mtime rc-mtime!) ; modified time, users may want to set it
  (cookie rc-cookie rc-cookie!) ; the cookie parsed from header string
  (set-cookie rc-set-cookie rc-set-cookie!) ; the cookies needed to be set as response
  ;; auto DB connection doesn't need users to close it, it's auto closed when request is over.
  (lpc rc-lpc rc-lpc!) ; store lpc object for later destruction
  (oht rc-oht rc-oht!) ; Option Handlers Table
  (conn rc-conn rc-conn!)) ; auto DB connection from pool

(define (get-header rc k)
  ;; (display (request-headers (rc-req rc)))(newline)
  (assq-ref (request-headers (rc-req rc)) k))

(define (new-route-context request body)
  (let* ((uri (request-uri request))
         (path (string-trim-right (uri-path uri) #\/))
         (m (valid-method? (request-method request)))
         ;; NOTE: sanitize-response will handle 'HEAD method
         ;;       though rc-method is 'GET when request-method is 'HEAD,
         ;;       sanitize-response only checks method from request
         (method (if (eq? m 'HEAD) 'GET m))
         (cookies (request-cookies request))
         (rc (make-route-context #f #f #f request path #f method #f #f
                                 body #f cookies '() #f #f #f)))
    ;; FIXME: maybe we don't need rhk? Throw it after get handler & keys
    (init-rule-handler-key! rc) ; set rule handler key
    (init-rule-handler-and-keys! rc) ; set handler and keys
    (init-rule-path-regexp! rc) ; set regexp
    (init-rule-key-bindings! rc) ; key binding of path
    (init-query! rc) ; init query-string and post body
    rc))

;; find & set the key of rule-handler,
;; which is used to find the (handler . keys)
;; FIXME: each method should have its own table
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
                  (get-handler-context handler-key)
                  (throw 'artanis-err 404 init-rule-handler-and-keys!
                         "Client ~a had visited an invalid path ~a"
                         (REASON-TEXT (remote-info (rc-req rc)))
                         (REASON-TEXT (rc-path rc))))))
    (rc-oht! rc (handler-context-oht hrc))
    (rc-handler! rc (handler-context-handler hrc))
    (rc-keys! rc (reverse (handler-context-keys hrc)))))

(define (init-rule-path-regexp! rc)
  (rc-re! rc (string->irregex (cdr (rc-rhk rc)))))

;; init key-bindings table
(define (init-rule-key-bindings! rc)
  (let* ((m (irregex-search (rc-re rc) (rc-path rc)))
         (num (irregex-match-num-submatches m)))
    (rc-bt! rc
            (map (lambda (k i) (cons k (irregex-match-substring m i)))
                 (rc-keys rc) (iota num 1)))))

(define (init-query! rc)
  ;; NOTE: All the prefix/postfix ":" in query/post keys are trimmed.
  ;;       Because only rule keys can use such naming.
  (define (-> x) (string-trim-both x))
  (let ((str (case (rc-method rc)
               ((GET) (uri-query (request-uri (rc-req rc))))
               ;; The accessor of GET and POST should be divided
               ((POST PUT DELETE HEAD OPTIONS PATCH) #f) ; don't handle post here
               (else (throw 'artanis-err 405 init-query!
                            "wrong method for query!" (rc-method rc))))))
    (if (and str (string-index str #\=))
        (rc-qt! rc (map (lambda (x)
                          (map -> (string-split x #\=)))
                        (string-split str #\&)))
        '())))

;; ENHANCE: do we need query hashtable?
(define (get-from-qstr rc key)
  (and (rc-qt rc)
       (and=> (assoc-ref (rc-qt rc) key) car)))

(define* (get-referer rc #:key (except #f))
  (let* ((headers (request-headers (rc-req rc)))
         (referer (assoc-ref headers 'referer)))
    (if referer
        (if (and except (irregex-search except (uri-path referer)))
            #f
            referer)
        #f)))

;; NOTE: oht will be #f in these situations:
;; 1. URL is not hit
;; 2. URL is static files
(define (rc-oht-ref rc key)
  (and=> (rc-oht rc) (lambda (oht) (hash-ref oht key))))

;; The params will be searched in binding-list first, then search from
;; qstr
;; TODO: qstr should be independent from rules binding.
(define (params rc key)
  (let ((val (or (assoc-ref (rc-bt rc) key)
                 (get-from-qstr rc key))))
    (and val ((current-encoder) val))))

(define (get-rule-uid rc)
  (let ((hc (get-handler-context (rc-rhk rc))))
    (when (not hc)
      (throw 'artanis-err 500 get-rule-uid
             "BUG: If handler-context is missing, then there shouldn't be a rc!"))
    (handler-context-uid hc)))

(define* (get-sid-from-client-cookie rc #:optional (idname "sid"))
  (any (lambda (c) (and=> (cookie-ref c idname) car)) (rc-cookie rc)))
