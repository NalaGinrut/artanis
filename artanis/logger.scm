;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2026
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
;;  Artanis is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  Artanis is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.
;;  If not, see <http://www.gnu.org/licenses/>.

(define-module (artanis logger)
  #:use-module (artanis env)
  #:use-module (artanis utils)
  #:use-module (ice-9 threads)
  #:export (artanis-log
            artanis-warn))

;; ENHANCE: use colored output
(define* (artanis-log blame-who? status mime #:key
                      (port (current-error-port))
                      (request #f) (msg #f)
                      ;; customizable meta data for logger plugin writers.
                      ;; For example:
                      ;; (artanis-log 'server 500 'html
                      ;;              #:msg "DB connection pool exhausted"
                      ;;              #:meta '((pool-size . 10)
                      ;;                       (waiting . 3)))
                      (meta #f))
  ;; NOTE: Optimized for mutex lock granularity.
  ;; 1. Each `monitor' calling will expand to create a new global mutex for the context.
  ;; 2. However, we only need one mutex for both client and server logging in case
  ;;    people want to get result from external web api with Artanis client.
  ;; 3. So we create a unified monitor context here.
  (define (atomic-output thunk)
    (monitor (thunk)))
  (case blame-who?
    ((warn)
     (atomic-output
      (lambda ()
        (when msg
          (display (WARN-TEXT msg) port)))))
    ((client)
     (when (not request)
       (error "artanis-log: Fatal bug! Request shouldn't be #f here!~%"))
     (let* ((uri (request-uri request))
            (path (uri-path uri))
            (qstr (uri-query uri))
            (method (request-method request)))
       (let ((s1 (format #f "[Remote] ~a @ ~a~%" (remote-info request) (local-time-stamp)))
             (s2 (format #f "[Request] method: ~a, path: ~a, query: ~a~%" method path qstr))
             (s3 (format #f "[Response] status: ~a, MIME: ~a~%~%" status mime)))
         (atomic-output
          (lambda ()
            (display s1 port)
            (display s2 port)
            (display s3 port)
            (and msg (display msg port)))))))
    ((server)
     (let ((s1 (format #f "[Server] ~a @ ~a~%" (get-conf '(host addr)) (local-time-stamp)))
           (s2 (format #f "[Response] status: ~a, MIME: ~a~%~%" status mime)))
       (atomic-output
        (lambda ()
          (display s1 port)
          (display s2 port)
          (and msg (display msg port))))))
    (else (error "artanis-log: Fatal BUG here!"))))

(define (artanis-warn fmt . args)
  (artanis-log 'warn #f #f #:msg (apply format #f fmt args)))
