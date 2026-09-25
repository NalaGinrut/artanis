;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2017-2026
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

;; RFC 6455 opening handshake, and the table of WebSocket routes.
;;
;; The handshake is done in http-read, before any route handler is called:
;; 1. websocket-request-error checks the upgrade request. A bad one is
;;    answered with an HTTP error (400, or 426 with the headers the client
;;    needs to retry) by reject-websocket-request, then the connection is
;;    closed.
;; 2. do-websocket-handshake writes the 101 response. From then on the
;;    connection speaks WebSocket only, any error must be a close frame.

(define-module (artanis websocket handshake)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis config)
  #:use-module (artanis irregex)
  #:use-module (artanis security nss)
  #:use-module (ice-9 format)
  #:use-module ((web request) #:select (request-version))
  #:use-module (web uri)
  #:use-module (rnrs bytevectors)
  #:use-module ((rnrs) #:select (define-record-type))
  #:use-module ((srfi srfi-1) #:select (find any))
  #:export (gen-accept-key
            websocket-origins-init!
            websocket-request-path
            websocket-request-error
            reject-websocket-request
            do-websocket-handshake
            closing-websocket-handshake

            websocket-rule-add!
            websocket-rule-timeout-set!
            websocket-rules-defined?
            find-websocket-rule
            websocket-rule-rule
            websocket-rule-protocol
            websocket-rule-inexclusive?
            websocket-rule-timeout
            url-need-websocket?
            url-need-inexclusive-websocket?))

(define *ws-magic* "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

;; ---------------------------------------------------------------------------
;; WebSocket routes
;;
;; One entry per route with #:websocket. `rule' is the route rule as written
;; by the user (trailing "/" trimmed), `irx' matches the request path the same
;; way the route does, so a rule with keys (e.g. "/chat/:room") works.
;; `protocol' is the handler protocol of the route (#:websocket), it's not
;; the Sec-WebSocket-Protocol subprotocol.

(define-record-type websocket-rule
  (fields irx rule protocol inexclusive?))

(define *websocket-rules* '())

;; rule -> seconds, from the #:timeout option of the route.
(define *websocket-timeouts* (make-hash-table))

;; regexp is the compiled rule, see compile-rule in (artanis oht).
(define* (websocket-rule-add! rule regexp protocol #:key (inexclusive? #f))
  (DEBUG "websocket-rule-add! ~a ~a~%" rule protocol)
  (set! *websocket-rules*
        (cons (make-websocket-rule (string->irregex regexp) rule protocol
                                   inexclusive?)
              *websocket-rules*)))

(define (websocket-rule-timeout-set! rule seconds)
  (hash-set! *websocket-timeouts* rule seconds))

(define (websocket-rules-defined?)
  (pair? *websocket-rules*))

;; The path as the route sees it, see new-route-context.
(define (websocket-request-path req)
  (string-trim-right (request-path req) #\/))

(define (find-websocket-rule path)
  (find (lambda (r) (irregex-match (websocket-rule-irx r) path))
        *websocket-rules*))

;; The idle timeout of connections of this route, in seconds. 0 means none.
(define (websocket-rule-timeout r)
  (or (hash-ref *websocket-timeouts* (websocket-rule-rule r))
      (get-conf '(websocket timeout))))

(define (url-need-websocket? path)
  (and (find-websocket-rule path) #t))

(define (url-need-inexclusive-websocket? path)
  (and=> (find-websocket-rule path) websocket-rule-inexclusive?))

;; ---------------------------------------------------------------------------
;; Opening handshake

(define (gen-accept-key key)
  (let* ((realkey (string-append key *ws-magic*))
         (keyhash (string->sha-1 realkey))
         (keybv (list->u8vector (string->byteslist keyhash 2 16))))
    (nss:base64-encode keybv)))

;; A valid key is the Base64 of 16 bytes, which is always 22 characters plus
;; "==". NSS's decoder is lenient, so we check the form directly.
(define *key-re* (string->irregex "[A-Za-z0-9+/]{21}[AQgw]=="))

(define (valid-key? key)
  (and (string? key) (irregex-match *key-re* key) #t))

(define (header-ref headers name)
  (assq-ref headers name))

;; Is it a request to upgrade to WebSocket at all?
(define (upgrade-request? headers)
  (let ((upgrade (header-ref headers 'upgrade))
        (connection (header-ref headers 'connection)))
    (and (list? upgrade)
         (any (lambda (p) (and (string? p) (string-ci=? p "websocket")))
              upgrade)
         (list? connection)
         ;; Guile parses Connection into downcased symbols.
         (memq 'upgrade connection)
         #t)))

;; ---------------------------------------------------------------------------
;; Origin
;;
;; Browsers don't apply CORS to WebSocket, and they send the cookies of the
;; target site with the handshake. So the Origin must be checked, otherwise
;; any page could open an authenticated connection on behalf of the user
;; (Cross-Site WebSocket Hijacking).
;; Trusted: the same origin as the Host of the request, and server.origins.
;; A request without Origin is not from a browser, so there's no hijacking
;; to prevent, it's accepted (a non-browser client can forge Origin anyway).

(define (default-port scheme)
  (case scheme
    ((http ws) 80)
    ((https wss) 443)
    (else #f)))

;; Normalize an origin string to (scheme host port), or #f if it's invalid
;; (including the opaque origin "null").
(define (parse-origin str)
  (let ((u (and (string? str) (string->uri (string-trim-both str)))))
    (and u (uri-scheme u) (uri-host u)
         (let ((port (or (uri-port u) (default-port (uri-scheme u)))))
           (and port
                (list (uri-scheme u) (string-downcase (uri-host u)) port))))))

;; host is the parsed Host header: (host . port), port may be #f.
(define (same-origin? origin host)
  (and (pair? host)
       (string-ci=? (cadr origin) (car host))
       ;; Without a port, Host means the default port of the scheme the
       ;; client used, which is the scheme of the origin (it may differ from
       ;; ours behind a TLS terminating proxy).
       (= (caddr origin)
          (or (cdr host) (default-port (car origin))))))

;; server.origins, parsed once at boot by websocket-origins-init!.
(define *trusted-origins* '())

;; Parse server.origins. Called at boot (see `run'), an invalid origin in the
;; config is an error.
(define (websocket-origins-init!)
  (set! *trusted-origins*
        (map (lambda (str)
               (or (parse-origin str)
                   (error "Invalid origin in server.origins:" str)))
             (get-conf '(server origins)))))

(define (trusted-origin? origin)
  (member origin *trusted-origins*))

(define (acceptable-origin? headers)
  (let ((str (header-ref headers 'origin)))
    (or (not str)
        (let ((origin (parse-origin str)))
          (and origin
               (or (same-origin? origin (header-ref headers 'host))
                   (trusted-origin? origin))
               #t)))))

(define *upgrade-headers*
  '((upgrade "websocket")
    (connection upgrade)
    (Sec-WebSocket-Version . "13")))

;; Check an opening handshake request on a WebSocket route.
;; Returns #f if it's acceptable, otherwise (status reason . extra-headers).
;; NOTE: Host is not checked here, Guile's read-request already rejects an
;;       HTTP/1.1 request without it.
(define (websocket-request-error req)
  (let* ((headers (request-headers req))
         (version (header-ref headers 'sec-websocket-version)))
    (cond
     ((not (upgrade-request? headers))
      ;; The route speaks WebSocket only.
      `(426 "Not a WebSocket upgrade request" ,@*upgrade-headers*))
     ((not (eq? 'GET (request-method req)))
      `(400 ,(format #f "Invalid method `~a' for WebSocket" (request-method req))))
     ((let ((v (request-version req)))
        (or (< (car v) 1) (and (= (car v) 1) (< (cdr v) 1))))
      `(400 ,(format #f "Invalid HTTP version ~a for WebSocket"
                     (request-version req))))
     ((not (valid-key? (header-ref headers 'sec-websocket-key)))
      '(400 "Missing or invalid Sec-WebSocket-Key"))
     ((not (string? version))
      '(400 "No Sec-WebSocket-Version"))
     ((not (string=? (string-trim-both version) "13"))
      `(426 ,(format #f "Unsupported WebSocket version `~a'" version)
            (Sec-WebSocket-Version . "13")))
     ((not (acceptable-origin? headers))
      `(403 ,(format #f "Untrusted origin `~a'" (header-ref headers 'origin))))
     (else #f))))

;; Answer a rejected handshake request. The caller closes the connection.
(define (reject-websocket-request req port err)
  (let ((status (car err))
        (reason (cadr err))
        (extra (cddr err)))
    (format (artanis-current-output)
            "[WebSocket] Rejected handshake of ~a: ~a ~a~%"
            (request-path req) status reason)
    (write-response (build-response #:code status
                                    #:headers `((content-length . 0) ,@extra))
                    port)
    (force-output port)))

;; The Sec-WebSocket-Protocol subprotocol is only negotiated when the client
;; asks for one: the protocol of the route is selected if the client listed
;; it. Otherwise the header is omitted, and it's up to the client whether to
;; go on (RFC 6455 4.2.2).
(define (select-subprotocol headers protocol)
  (let ((requested (header-ref headers 'sec-websocket-protocol)))
    (and (string? requested)
         (symbol? protocol)
         (let ((name (symbol->string protocol)))
           (and (any (lambda (p) (string=? (string-trim-both p) name))
                     (string-split requested #\,))
                name)))))

;; Write the 101 response. The request must have passed
;; websocket-request-error, and its path must be a WebSocket route.
(define (do-websocket-handshake req port)
  (let* ((headers (request-headers req))
         (rule (find-websocket-rule (websocket-request-path req)))
         (accept-key (gen-accept-key (header-ref headers 'sec-websocket-key)))
         (subprotocol (select-subprotocol headers (websocket-rule-protocol rule)))
         (res (build-response
               #:code 101
               #:headers `((upgrade "websocket")
                           (connection upgrade)
                           (Sec-WebSocket-Accept . ,accept-key)
                           ,@(if subprotocol
                                 `((Sec-WebSocket-Protocol . ,subprotocol))
                                 '())))))
    (write-response res port)
    (force-output port)
    (format (artanis-current-output)
            "[WebSocket] Handshake successfully from ~a~a~%"
            (or (header-ref headers 'origin) "unknown origin")
            (request-path req))))

;; NOTE: Only used by the redirector branch of http-close, which is dead code
;;       until the redirector is reworked (layer 5). The closing handshake of
;;       a WebSocket connection is done by ws-close in (artanis server websocket).
(define (closing-websocket-handshake server client peer-shutdown?)
  (DEBUG "[Websocket] closing-websocket-handshake ~a~%" peer-shutdown?))
