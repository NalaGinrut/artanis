;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2016
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

(define-module (artanis server http)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis config)
  #:use-module (artanis server server-context)
  #:use-module (artanis server epoll)
  #:use-module (web request)
  #:use-module (web response)
  #:export (new-http-protocol))

;; NOTE: HTTP service is established by default, so it's unecessary to do any
;;       openning work.
(define (http-open server client)
  (throw 'artanis-err 500 http-open
         "This method shouldn't be called, it's likely a bug!"))

(define (http-read server client)
  (define (bad-request port)
    (write-response (build-response #:version '(1 . 1) #:code 400
                                    #:headers '((content-length . 0)))))
  (let ((port (client-sockport client)))
   (cond
    ((eof-object? (peek-char port))
     (DEBUG "Encountered EOF, closing ~a~%" (address->ip client))
     (close-port port)
     (close-task))
    (else
     (with-throw-handler
      #t
      ;; We use with-throw-handler here, it's different from catch which will
      ;; unwind the stack. That means, if you use with-throw-handler, and when
      ;; the exception occurs, the exception will throw to the innomost catch
      ;; context after the error handler of with-throw-handler returns.
      ;; This helps us to avoid unwind and re-throw exception.
      (lambda ()
        (let* ((req (read-request port))
               (body (if (detect-if-connecting-websocket req server client)
                         #f (read-request-body req))))
          (values port req body)))
      (lambda (k . e)
        (bad-request port)
        (close-port port)))))))

(define (http-write server client)
  #t)


(define (clean-current-conn-fd server client)
  (let ((conn-fd (client-sockport-decriptor client))
        (epfd (ragnarok-server-epfd server)))
    ;; NOTE:
    ;; In kernel versions before 2.6.9, the EPOLL_CTL_DEL operation required a
    ;; non-null pointer in event, even though this argument is ignored. Since
    ;; Linux 2.6.9, event can be specified as NULL when using EPOLL_CTL_DEL.
    ;; Applications that need to be portable to kernels before 2.6.9 should
    ;; specify a non-null pointer in event.
    ;; So, Artanis isn't compatible with Linux 2.6.9 and before.
    (epoll-ctl epfd EPOLL_CTL_DEL conn-fd #f) ; #f means %null-pointer here
    ;; Close the connection gracefully
    ;; FIXME: shutdown or close ?
    ;;(shutdown conn-fd 2) ; Stop both recv and trans
    (close conn-fd))) ; deallocate the File Descriptor

;; clean task from work-table
(define (clean-current-task server client)
  (define-syntax-rule (do-clean-task wt)
    (remove-from-work-table! wt client))
  ;; NOTE: current task is the head of work-table
  (let ((wt (current-work-table server))
        (workers (get-conf '(server workers))))
    (cond
     ((= 1 workers) (do-clean-task wt))
     ((> workers 1) (schedule-if-locked (work-table-mutex wt) (do-clean-task wt)))
     (else (throw 'artanis-err 500 "Invalid (server workers) !" workers)))))

;; Check if the client in the redirectors table:
;; 1. In the table, just scheduled for next time.
;; 2. Not in the table, just close the connection.
(define (http-close server client)
  (cond
   ((and (not (must-close-connection?))
         (get-the-redirector-of-protocol server client))
    ;; If the protocol has been registered by the client, then it means
    ;; the client enabled a special websocket-based protocol other than
    ;; HTTP. And we will not close this client, but treat it as a waiting
    ;; connection.
    => (lambda (proto)
         (let ((name (protocol-name prot))
               (ip (client-ip client)))
           (DEBUG "The client(~a) is ~a~%" name ip))))
   (else
    (clean-current-conn-fd server client)
    (clean-current-task server client))))

(define-protocol http http-open http-read http-write http-close)

(define (new-http-protocol)
  (make-protocol 'http http-open http-read http-write http-close (make-hash-table)))
