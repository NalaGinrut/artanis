;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014
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

(define-module (artanis cache)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module ((artanis artanis) #:select (response-emit
                                            rc-req
                                            emit-response-with-file))
  #:use-module (ice-9 match)
  #:use-module (web request)
  #:use-module (web response)
  #:export (cache-maker))

;; Cache-Control
;; 1. The Cache-Control header is the most important header to set as it
;;    effectively 'switches on' caching in the browser.
;; 2. With this header in place, and set with a value that enables caching,
;;    the browser will cache the file for as long as specified.
;; 3. Without this header the browser will re-request the file on each
;;    subsequent request.
;; 4. `public' resources can be cached not only by the end-user’s browser
;;    but also by any intermediate proxies that may be serving many other
;;    users as well.
;; 5. `private' resources are bypassed by intermediate proxies and can only
;;    be cached by the end-client.
;; 6. `max-age' sets a timespan for how long to cache the resource (in seconds).
;;    e.g: Cache-Control:public, max-age=31536000

;; -----------------------------------------------------------------------------
;; Expires:
;; 1. When accompanying the Cache-Control header, Expires simply sets a date
;;    from which the cached resource should no longer be considered valid.
;;    From this date forward the browser will request a fresh copy of the
;;    resource. Until then, the browsers local cached copy will be used.
;;    e.g: Cache-Control:public
;;         Expires: Mon, 25 Jun 2012 21:31:12 GMT
;; 2. If both Expires and max-age are set max-age will take precedence.


;; Conditional Cache

;; A. Time based
;; 1. Server return Last-Modified to enable conditional cache on client side;
;;    e.g: Cache-Control:public, max-age=31536000
;;         Last-Modified: Mon, 03 Jan 2011 17:45:57 GMT
;; 2. Client should send If-Modified-Since, then server can decide how to cache.
;;    e.g: If-Modified-Since: Mon, 03 Jan 2011 17:45:57 GMT
;; 3. In Artanis, 

;; B. Content based
;; 1. ETag works in a similar way that its value is a digest of the resources
;;    contents (Artanis use MD5 to compute hash for ETag).
;;    e.g: Cache-Control:public, max-age=31536000
;;         ETag: "15f0fff99ed5aae4edffdd6496d7131f"
;; 2. ETag is useful when for when the last modified date is difficult to determine.
;; 3. On subsequent browser requests the If-None-Match request header is sent
;;    with the ETag value of the last requested version of the resource.
;;    e.g: If-None-Match: "15f0fff99ed5aae4edffdd6496d7131f"
;; 4. As with the If-Modified-Since header, if the current version has the same
;;    ETag value, indicating its value is the same as the browser’s cached copy,
;;    then an HTTP status of 304 is returned.
;; 5. In Artanis, ETag for static file is Time based, for dynamic content, content
;;    based.

;; Use Cases
;; A. Static page
;; Nothing to say. 

;; B. Dynamic page
;; 1. The developer must assess how heavily it can be cached and what the
;;    implications might be of serving stale content to the user.
;; 2. Some contents are slowly changed, e.g: RSS
;; 3. Some contents are frequently changed, e.g: Json of twitter timeline.
;; 4. In Artanis, if you enable cache for a dynamic page, the benifits are
;;    saving the bandwidth, and accelerate the response of loading page.
;;    The server will render the page anyway, but won't send it if the
;;    cache hits.

;; C. Cache prevention
;; 1. Cache-Control header can specify no-cache and no-store which informs the
;;    browser to not cache the resources under any circumstances.
;; 2. Both values are required as IE uses no-cache, and Firefox uses no-store.
;;    e.g: Cache-Control:no-cache, no-store

;; D. Private content
;; 1. The content can be considered sensitive and subject to security measures.
;; 2. Also need to consider the impact of having intermediary caches, such as
;;    web proxies. If in doubt, a safe option is not cache these items at all.
;; 3. Ask for resources to only be cached privately.
;;    (i.e only within the end-user’s browser cache).
;;    e.g: Cache-Control:private, max-age=31536000

(define (emit-HTTP-304)
  (response-emit "" #:status 304))

(define (cacheable-request? request)
  (and (memq (request-method request) '(GET HEAD))
       (not (request-authorization request))
       ;; We don't cache these conditional requests; just
       ;; if-modified-since and if-none-match.
       ;; TODO: provide all the cache features
       (not (request-if-match request))
       (not (request-if-range request))
       (not (request-if-unmodified-since request))))

(define (cacheable-response? response)
  (and (not (memq 'no-cache (response-pragma response)))
       (not (member '(no-cache . #t) (response-cache-control response)))
       (memq (response-code response) '(200 301 304 404 410))
       (null? (response-vary response))))

(define (try-to-cache-dynamic-content rc body etag opts)
  (define (->cc o)
    (match o
      ((#t)
       ;; public cache with default max-age
       (format #t "public,max-age=~a" (get-conf '(cache maxage))))
      (('public . maxage)
       (let ((m (if (null? maxage) (get-conf '(cache maxage)) (car maxage))))
         (format #f "public,max-age=~a" m)))
      (('private . maxage)
       (let ((m (if (null? maxage) (get-conf '(cache maxage)) (car maxage))))
           (foramt #f "private,max-age=~a" m)))
      (else (throw 'artanis-err "->cc: Invalid opts!" o))))
  (response-emit body #:headers `((ETag . ,etag)
                                  (Cache-Control . (->cc opts)))))

;; TODO:
;; Add a memory cache for quicker hash comparing, rather than get file stat
;; each time from disk.
;; say, (path . hash) table

(define (generate-ETag filename)
  (cond
   ((file-exists? filename)
    (let ((st (stat filename)))
      (format #f "\"~X-~X-~X\"" 
              (stat:ino st) (stat:mtime st) (stat:size st))))
   (else '())))

(define-syntax-rule (->headers rc)
  (request-headers (rc-req rc)))

(define-syntax-rule (->ETag rc)
  (assoc-ref (->headers rc) 'ETag))

(define-syntax-rule (ETag-hit? rc etag)
  (and=> (->ETag rc)
         (lambda (e) (string=? e etag))))

(define-syntax-rule (content-hit? rc etag)
  (and=> (->ETag rc)
         (lambda (e) (string=? e etag))))

;; ETag for dynamic content is content based
(define (try-to-cache-body rc body . opts)
  (cond
   ((cacheable-response? (rc-req rc))
    (let ((etag (string->md5 body)))
      (if (content-hit? rc etag)
          (emit-HTTP-304)
          (try-to-cache-dynamic-content rc body etag opts))))
   (else body)))

(define-syntax-rule (emit-static-file-with-cache file etag status max-age)
  (let ((cc (format #f "~a,max-age=~a" status max-age)))
    (emit-response-with-file
     file
     `((Cache-Control . ,cc)
       ,@(if (null? etag) '() `((ETag . ,etag)))))))

(define-syntax-rule (emit-static-file-without-cache file)
  (let ((headers `((Cache-Control . "no-cache,no-store"))))
    (emit-response-with-file file headers)))

;; NOTE: the ETag of static file is time based, not content based
(define (try-to-cache-static-file rc file status max-age)
  (cond
   ((cacheable-request? (rc-req rc))
    (let ((etag (generate-ETag file)))
      (or (and (ETag-hit? rc etag)
               (emit-HTTP-304)) ; cache hit
          (emit-static-file-with-cache file etag status max-age))))
   (else (emit-static-file-without-cache file))))

;; NOTE: maxage1 will overwrite maxage0
(define-syntax-rule (->maxage maxage0 maxage1)
  (begin
    (when (or (not (list? maxage0)) (not (list? maxage1)))
      (throw 'artanis-err "->maxage: Invalid maxage!" maxage0 maxage1))
    (if (null? maxage1) ; maxage1 wasn't specified
        (if (null? maxage0) ; use maxage0
            (get-conf '(cache maxage)) ; get default maxage
            (car maxage0)) ; set as maxage
        (if (null? maxage1) ; use maxage1
            (get-conf '(cache maxage))
            (car maxage1)))))
      
(define (non-cache rc body) body)

(define (cache-maker pattern rule keys)
  (match pattern
    ((#f) non-cache)
    (((? string=? file) . maxage0)
     (lambda (rc . maxage1)
       (try-to-cache-static-file rc file "public" (->maxage maxage0 maxage1))))
    (('public (? string=? file) . maxage0)
     (lambda (rc . maxage1)
       (try-to-cache-static-file rc file "public" (->maxage maxage0 maxage1))))
    (('private (? string=? file) . maxage0)
     (lambda (rc . maxage1)
       (try-to-cache-static-file rc file "private" (->maxage maxage0 maxage1))))
    ((or (? boolean? opts) (? list? opts))
     (lambda (rc body) (try-to-cache-body rc body opts)))
    (else (throw 'artanis-err "cache-maker: invalid pattern!" pattern))))
