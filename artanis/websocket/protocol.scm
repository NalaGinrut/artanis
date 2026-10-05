;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2026
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

;; Application protocols of WebSocket.
;;
;; An application protocol is the codec of the messages of a route with
;; #:websocket '(proto X): on such a route, on-message gets the decoded
;; object, and ws-send encodes the object it's given, so the handler never
;; sees the bytes. It's not related to the protocols of Ragnarok (http,
;; websocket, ...) in *proto-table*, which are transports.
;;
;; Each protocol is one file in app/protocols, named after the protocol:
;;
;;   ;; app/protocols/chat.scm
;;   (define-ws-protocol chat
;;     #:type 'text
;;     #:decode (lambda (bv) (json-string->scm (utf8->string bv)))
;;     #:encode (lambda (obj) (string->utf8 (scm->json-string obj))))
;;
;; It's the module (app protocols chat), which exports ws-type, ws-decode
;; and ws-encode. (rnrs bytevectors) and (artanis third-party json) are
;; imported, import anything else after define-ws-protocol, e.g. a parser
;; in lib/. `art work' loads app/protocols before the controllers, loading
;; a file registers its protocol.
;; NOTE: The arguments are evaluated where define-ws-protocol is, so a name
;;       defined or imported after it can be used within a lambda, but not
;;       as the argument itself:
;;         #:decode (lambda (bv) (my-decode bv))   ; fine
;;         #:decode my-decode                      ; unbound
;;
;; #:type is the frame type of the messages:
;;  - 'text or 'binary: a message of the other type is closed with 1003.
;;    ws-decode is (bytevector) -> object, its error closes the connection
;;    with 1007. ws-encode is (object) -> bytevector, it's called by ws-send
;;    in the context of its caller, its error is thrown to the caller. The
;;    bytevector returned by ws-encode belongs to Artanis, it must not be
;;    modified afterwards, since it may be waiting in the outbound queue.
;;  - 'any: the frame type isn't checked. ws-decode gets the ws-message, and
;;    ws-encode returns anything ws-send takes on a 'raw route (a string, a
;;    ws-buffer or a ws-message).
;; The protocol is initialized once when it's loaded, it's shared by all the
;; connections of its routes, so keep per-connection state in the closure
;; of the route handler, not in the codec.
;;
;; Builtin protocols are registered here, a protocol in app/protocols can't
;; take their names:
;;  - echo: 'any, the identity codec.
;; raw, redirect and proxy are the other modes of #:websocket, they can't be
;; protocol names either.

(define-module (artanis websocket protocol)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (ice-9 ftw)
  #:use-module ((system syntax internal)
                #:select (syntax? make-syntax syntax-expression syntax-wrap
                          syntax-sourcev))
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (define-ws-protocol
            ws-protocol-register!
            load-app-protocols
            lookup-ws-protocol
            ws-protocol?
            ws-protocol-name
            ws-protocol-type
            ws-protocol-decode
            ws-protocol-encode
            ws-protocol-builtin?
            ws-protocol-route?))

(define-record-type ws-protocol
  (fields name type decode encode builtin?))

;; name -> ws-protocol, both builtin and app protocols.
(define *ws-protocols* (make-hash-table))

;; The modes of #:websocket other than '(proto X), see websocket-maker in
;; (artanis oht). The protocol of their route rules is the mode itself.
(define *websocket-modes* '(raw redirect proxy))

;; Is it the protocol of a '(proto X) route? See websocket-rule-protocol.
(define (ws-protocol-route? protocol)
  (and (symbol? protocol)
       (not (memq protocol *websocket-modes*))))

(define (lookup-ws-protocol name)
  (hashq-ref *ws-protocols* name))

;; The protocol the file being loaded by load-app-protocols must define.
(define loading-protocol (make-parameter #f))

;; NOTE: Protocols are registered at start-up, so the errors are reported like
;;       the other start-up checks (see check-websocket-config).
(define (check-protocol name type decode encode)
  (define (err fmt . args)
    (error (apply format #f fmt args)))
  (unless (symbol? name)
    (err "Invalid protocol name `~a'" name))
  (when (memq name *websocket-modes*)
    (err "`~a' is a mode of #:websocket, it can't be a protocol name" name))
  (let ((p (lookup-ws-protocol name)))
    (when p
      (if (ws-protocol-builtin? p)
          (err "`~a' is a builtin protocol, please choose another name" name)
          (err "Protocol `~a' is defined twice" name))))
  (let ((expected (loading-protocol)))
    (when (and expected (not (eq? expected name)))
      (err "app/protocols/~a.scm must define protocol `~a', not `~a'"
           expected expected name)))
  (unless (memq type '(text binary any))
    (err "Invalid #:type `~a' of protocol `~a', expect 'text, 'binary or 'any"
         type name))
  (unless (procedure? decode)
    (err "#:decode of protocol `~a' must be a procedure, but it's `~a'"
         name decode))
  (unless (procedure? encode)
    (err "#:encode of protocol `~a' must be a procedure, but it's `~a'"
         name encode)))

;; Register an app protocol, it's called by define-ws-protocol with its
;; keyword arguments. Returns (values type decode encode) for the exported
;; bindings of the protocol module.
(define (ws-protocol-register! name . args)
  (let lp ((args args) (type #f) (decode #f) (encode #f))
    (cond
     ((null? args)
      (check-protocol name type decode encode)
      (hashq-set! *ws-protocols* name
                  (make-ws-protocol name type decode encode #f))
      (values type decode encode))
     ((and (pair? (cdr args)) (memq (car args) '(#:type #:decode #:encode)))
      (let ((v (cadr args)))
        (case (car args)
          ((#:type) (lp (cddr args) v decode encode))
          ((#:decode) (lp (cddr args) type v encode))
          ((#:encode) (lp (cddr args) type decode v)))))
     (else
      (error (format #f "Invalid arguments `~a' of protocol `~a', expect #:type, #:decode and #:encode"
                     args name))))))

(eval-when (expand load eval)
  ;; Rebuild the syntax x as if it were written in module mod. The arguments
  ;; of define-ws-protocol come with the module where the form is expanded
  ;; (the module of the loader), since the module of the protocol is defined
  ;; by the same form. They're evaluated in the protocol module anyway, but
  ;; the compiler checks their names in the module of the loader, so each
  ;; name imported by define-ws-protocol (e.g. json-string->scm) would be
  ;; warned as a possibly unbound variable.
  (define (rewrap-in-module x mod)
    (cond
     ((syntax? x)
      (make-syntax (rewrap-in-module (syntax-expression x) mod)
                   (syntax-wrap x) mod (syntax-sourcev x)))
     ((pair? x)
      (cons (rewrap-in-module (car x) mod) (rewrap-in-module (cdr x) mod)))
     ((vector? x)
      (list->vector (map (lambda (e) (rewrap-in-module e mod))
                         (vector->list x))))
     (else x))))

(define-syntax define-ws-protocol
  (lambda (x)
    (syntax-case x ()
      ((_ name args ...) (identifier? #'name)
       (with-syntax ((ws-type (datum->syntax #'name 'ws-type))
                     (ws-decode (datum->syntax #'name 'ws-decode))
                     (ws-encode (datum->syntax #'name 'ws-encode))
                     ((args ...)
                      (rewrap-in-module
                       #'(args ...)
                       `(hygiene app protocols ,(syntax->datum #'name)))))
         #`(begin
             ;; NOTE: we have to encapsulate them to a module for protecting namespaces
             (define-module (app protocols name)
               #:use-module (rnrs bytevectors)
               #:use-module (artanis third-party json)
               #:export (ws-type ws-decode ws-encode))
             (define-values (ws-type ws-decode ws-encode)
               ((@ (artanis websocket protocol) ws-protocol-register!)
                'name args ...))))))))

(define (register-builtin! name type decode encode)
  (hashq-set! *ws-protocols* name
              (make-ws-protocol name type decode encode #t)))

(register-builtin! 'echo 'any identity identity)

;; Load app/protocols/*.scm, each one registers its protocol. It's done by
;; `art work' before the controllers, so a protocol is ready before any
;; connection. A file must define the protocol it's named after.
;; NOTE: There's no sub-directory in app/protocols, put a big parser in lib/
;;       and import it in the protocol file.
(define (load-app-protocols)
  (let* ((dir (format #f "~a/app/protocols" (current-toplevel)))
         (files (or (and (file-exists? dir)
                         (scandir dir (lambda (f)
                                        (and (string-suffix? ".scm" f)
                                             (not (string-prefix? "." f))))))
                    '())))
    (when (pair? files)
      (display "Loading protocols...\n" (artanis-current-output)))
    (use-modules (artanis websocket protocol)) ; black magic to make Guile happy
    (for-each
     (lambda (f)
       (let ((name (string->symbol (string-drop-right f 4))))
         (parameterize ((loading-protocol name))
           (load (string-append dir "/" f)))
         (unless (lookup-ws-protocol name)
           (error (format #f "app/protocols/~a doesn't define protocol `~a' with define-ws-protocol"
                          f name)))))
     files)))
