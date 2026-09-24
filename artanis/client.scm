;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2022-2026
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

(define-module (artanis client)
  #:use-module (artanis utils)
  #:use-module (artanis server server-context)
  #:use-module (artanis runner)
  #:use-module (artanis config)
  #:use-module (web response)
  #:use-module (web http)
  #:use-module (web uri)
  #:use-module (srfi srfi-11)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 iconv)
  #:use-module (curl)
  #:export (artanis:http-head
            artanis:http-get
            artanis:http-post
            artanis:http-patch
            artanis:http-delete
            artanis:http-put))

;; It's recommended to use (artanis client) rather than (web client)

;; The client is designed to be safe to call anywhere:
;;  - In a handler: the request runs in a runner thread, the task is suspended
;;    and the server core is never blocked.
;;  - In a runner thunk, or outside any task (scripts, init): the request just
;;    runs in the current thread (see call-with-runner).
;; To make that safe, the whole life of a curl handle (init, setopt, perform,
;; cleanup) stays inside the runner thunk, so the handle is never touched by
;; two threads, and it's never freed while it's still in use, even if the
;; task gives up waiting for the runner.
;;
;; Defaults for the safety limits, they can be overridden per call:
(define *default-timeout* 300)                ; seconds for the whole request
(define *default-connect-timeout* 10)         ; seconds to connect
(define *default-max-size* (* 64 1024 1024))  ; bytes of the response body
(define *max-redirects* 5)
;; Only HTTP(S) is allowed, including redirects. Otherwise a malicious
;; redirect could lead the request to file://, gopher://, etc. (SSRF)
;; The values are CURLPROTO_HTTP and CURLPROTO_HTTPS in curl.h.
(define *allowed-protocols* (logior 1 2))
;; curl error code of CURLE_OPERATION_TIMEDOUT in curl.h.
(define CURLE_OPERATION_TIMEDOUT 28)

;; NOTE: Guile's (web http) enforces structured values for *declared*
;; headers -- e.g. 'authorization and 'content-type expect a parsed
;; credentials/media-type object, not a raw string, and write-header will
;; throw a match-error if you hand it "Bearer sk_xxx" directly (verified).
;; Undeclared/custom header symbols (e.g. 'stripe-version) go through a
;; generic fallback and accept plain strings fine.
;; To let callers set declared headers with a raw value (as every real
;; third-party API expects), a header entry may now be EITHER:
;;   - a (name . value) pair, handled via write-header as before, or
;;   - an already-formatted "Name: value" string, used verbatim.
;; NOTE: CR or LF in a header would inject extra headers (the values often
;;       come from user input, e.g. when calling an LLM API), so it's refused.
(define (gen-headers-list headers)
  (map (lambda (e)
         (let ((line (if (string? e)
                         e
                         (string-trim-both
                          (call-with-output-string
                           (lambda (port)
                             (write-header (car e) (cdr e) port)))))))
           (when (string-any (lambda (c) (memv c '(#\return #\newline))) line)
             (throw 'artanis-err 400 gen-headers-list
                    "Invalid header, CR or LF is not allowed: ~s" line))
           line))
       headers))

;; The body can be a bytevector or a string. A string is encoded in UTF-8,
;; since curl only takes 8-bit data (e.g. a prompt in Chinese would fail).
(define (->body-bytevector body)
  (cond
   ((bytevector? body) body)
   ((string? body) (string->utf8 body))
   (else (throw 'artanis-err 500 ->body-bytevector
                "The body must be a bytevector or a string: ~a" body))))

;; NOTE: `cert' and `verify-peer?' are two independent concerns and must
;; not be conflated:
;;   - `verify-peer?' controls whether curl validates the *server's*
;;     certificate (CURLOPT_SSL_VERIFYPEER / CURLOPT_SSL_VERIFYHOST).
;;     This should default to #t for any real HTTPS endpoint (e.g. a
;;     third-party payment API). Only flip it to #f for local/dev
;;     endpoints with self-signed certs.
;;   - `cert' is an optional *client* certificate (CURLOPT_SSLCERT) used
;;     for mutual TLS. Most third-party APIs (Stripe included) never need
;;     this; it has nothing to do with whether the server cert is checked.
;; Previously these were conflated: not passing `cert' silently disabled
;; peer verification entirely, which is unsafe for any endpoint handling
;; secrets or financial data.
(define (setup-handle! handle url headers cert verify-peer?
                       timeout connect-timeout max-size)
  (curl-easy-setopt handle 'url url)
  ;; NOTE: 2 is CURL_HTTP_VERSION_1_1, NOT HTTP/2. It must stay 1.1, since
  ;;       Guile's read-response can't parse the "HTTP/2 200" status line.
  (curl-easy-setopt handle 'http-version 2)
  ;; NOTE: Required for multi-threaded programs, otherwise libcurl may use
  ;;       signals for DNS timeouts, which isn't thread-safe.
  (curl-easy-setopt handle 'nosignal #t)
  (curl-easy-setopt handle 'protocols *allowed-protocols*)
  (curl-easy-setopt handle 'redir-protocols *allowed-protocols*)
  (curl-easy-setopt handle 'followlocation #t)
  (curl-easy-setopt handle 'maxredirs *max-redirects*)
  (when (and timeout (positive? timeout))
    (curl-easy-setopt handle 'timeout timeout))
  (when (and connect-timeout (positive? connect-timeout))
    (curl-easy-setopt handle 'connecttimeout connect-timeout))
  (when (and max-size (positive? max-size))
    (curl-easy-setopt handle 'maxfilesize max-size))
  (if verify-peer?
      (begin
        (curl-easy-setopt handle 'ssl-verifypeer #t)
        (curl-easy-setopt handle 'ssl-verifyhost #t))
      (begin
        (curl-easy-setopt handle 'ssl-verifypeer #f)
        (curl-easy-setopt handle 'ssl-verifyhost #f)))
  (when cert
    (curl-easy-setopt handle 'sslcert cert))
  (curl-easy-setopt handle 'httpheader headers))

;; Runs in the runner thread (or in place). Returns one of:
;;   (ok header-string body)
;;   (failed curl-code curl-errstr)
;;   (error key . args)       ; any exception while setting up the request
;; NOTE: guile-curl keeps the last error in process-global variables, and a
;;       successful request doesn't reset them. So we only read them right
;;       after a failure, in the same thread, and never judge success by them.
(define (perform-request url method-setup headers cert verify-peer? bv?
                         timeout connect-timeout max-size)
  (let ((handle (curl-easy-init)))
    (let ((result
           (catch #t
             (lambda ()
               (method-setup handle)
               (setup-handle! handle url headers cert verify-peer?
                              timeout connect-timeout max-size)
               (DEBUG "artanis-client: do the request...~%")
               (let ((ret (curl-easy-perform handle bv? #t)))
                 (DEBUG "artanis-client: done the request.~%")
                 (if ret
                     (list 'ok (car ret) (cadr ret))
                     (list 'failed (curl-error-code) (curl-error-string)))))
             (lambda args
               (cons 'error args)))))
      (curl-easy-cleanup handle)
      result)))

;; guile-curl builds a string body byte by byte as Latin-1, which garbles any
;; non-ASCII text (e.g. an LLM reply in Chinese). So we always fetch the body
;; as a bytevector, and decode it here by the charset of the response, which
;; defaults to UTF-8. Invalid sequences are substituted rather than throwing.
(define (decode-body res bv)
  (let* ((ct (false-if-exception (response-content-type res)))
         (charset (or (and (pair? ct) (assq-ref (cdr ct) 'charset)) "utf-8")))
    (catch #t
      (lambda () (bytevector->string bv charset 'substitute))
      (lambda _ (bytevector->string bv "utf-8" 'substitute)))))

;; Parse the response header string. There may be several responses in it,
;; e.g. with follow location (301/302) or "100 Continue", so we take the last.
(define (parse-final-response header-string)
  (call-with-input-string
   header-string
   (lambda (port)
     (let lp ((ret (read-response port)))
       (cond
        ((eof-object? (peek-char port)) ret)
        (else (lp (read-response port))))))))

(define (do-request url method method-setup headers cert verify-peer? bv?
                    timeout connect-timeout max-size)
  (let* ((header-lines (gen-headers-list headers))
         (result (call-with-runner
                  (lambda ()
                    ;; Always fetch the body as a bytevector, see decode-body.
                    (perform-request url method-setup header-lines cert
                                     verify-peer? #t timeout
                                     connect-timeout max-size))
                  ;; curl enforces the timeout and aborts the request, so the
                  ;; runner returns in time. The runner timeout is only a last
                  ;; resort in case curl gets stuck.
                  #:timeout (if (and timeout (positive? timeout))
                                (+ timeout 10)
                                0))))
    (case (car result)
      ((ok)
       (let ((res (catch #t
                    (lambda () (parse-final-response (cadr result)))
                    (lambda e
                      (throw 'artanis-err 502 do-request
                             "client error: method `~a', invalid response: ~a"
                             method e)))))
         (values res (if bv? (caddr result) (decode-body res (caddr result))))))
      ((failed)
       (let ((code (cadr result))
             (errstr (caddr result)))
         (throw 'artanis-err
                (if (eqv? code CURLE_OPERATION_TIMEDOUT) 504 502)
                do-request
                "client error: method `~a', code `~a', errstr `~a'!"
                method code errstr)))
      (else
       (let ((key (cadr result))
             (args (cddr result)))
         (apply throw key args))))))

(define* (artanis:http-head url #:key (headers '()) (cert #f) (verify-peer? #t)
                            (timeout *default-timeout*)
                            (connect-timeout *default-connect-timeout*)
                            (max-size *default-max-size*))
  (do-request url 'HEAD
              (lambda (handle) (curl-easy-setopt handle 'nobody #t))
              headers cert verify-peer? #f timeout connect-timeout max-size))

(define* (artanis:http-get url #:key (headers '()) (cert #f) (verify-peer? #t)
                           (bytevector? #f)
                           (timeout *default-timeout*)
                           (connect-timeout *default-connect-timeout*)
                           (max-size *default-max-size*))
  (do-request url 'GET
              (lambda (handle) (curl-easy-setopt handle 'httpget #t))
              headers cert verify-peer? bytevector? timeout connect-timeout
              max-size))

;; NOTE: The body defaults to empty. It used to be #u8(0), which sent a 1-byte
;;       NUL body, and many APIs reject it (e.g. as invalid JSON).
(define* (artanis:http-post url #:key (headers '()) (cert #f) (verify-peer? #t)
                            (body #vu8()) (bytevector? #f) (customrequest #f)
                            (timeout *default-timeout*)
                            (connect-timeout *default-connect-timeout*)
                            (max-size *default-max-size*))
  (let ((bv (->body-bytevector body)))
    (do-request url (or customrequest 'POST)
                (lambda (handle)
                  (curl-easy-setopt handle 'httpget #f)
                  (curl-easy-setopt handle 'post #t)
                  (when customrequest
                    (curl-easy-setopt handle 'customrequest customrequest))
                  (curl-easy-setopt handle 'postfields bv))
                headers cert verify-peer? bytevector? timeout connect-timeout
                max-size)))

(define* (artanis:http-patch url #:key (headers '()) (cert #f) (verify-peer? #t)
                             (body #vu8()) (bytevector? #f)
                             (timeout *default-timeout*)
                             (connect-timeout *default-connect-timeout*)
                             (max-size *default-max-size*))
  (artanis:http-post url #:headers headers #:cert cert #:verify-peer? verify-peer?
                     #:body body #:bytevector? bytevector? #:customrequest "PATCH"
                     #:timeout timeout #:connect-timeout connect-timeout
                     #:max-size max-size))

(define* (artanis:http-delete url #:key (headers '()) (cert #f) (verify-peer? #t)
                              (body #vu8()) (bytevector? #f)
                              (timeout *default-timeout*)
                              (connect-timeout *default-connect-timeout*)
                              (max-size *default-max-size*))
  (artanis:http-post url #:headers headers #:cert cert #:verify-peer? verify-peer?
                     #:body body #:bytevector? bytevector? #:customrequest "DELETE"
                     #:timeout timeout #:connect-timeout connect-timeout
                     #:max-size max-size))

(define* (artanis:http-put url #:key (headers '()) (cert #f) (verify-peer? #t)
                           (body #vu8()) (bytevector? #f)
                           (timeout *default-timeout*)
                           (connect-timeout *default-connect-timeout*)
                           (max-size *default-max-size*))
  (artanis:http-post url #:headers headers #:cert cert #:verify-peer? verify-peer?
                     #:body body #:bytevector? bytevector? #:customrequest "PUT"
                     #:timeout timeout #:connect-timeout connect-timeout
                     #:max-size max-size))
