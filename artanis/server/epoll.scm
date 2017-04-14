;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2016,2017
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

(define-module (artanis server epoll)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module ((rnrs)
                #:select (bytevector-s32-native-ref
                          bytevector-s32-native-set!
                          bytevector-u32-native-ref
                          bytevector-u32-native-set!
                          make-bytevector))
  #:use-module (system foreign))

(define-public EPOLL_CLOEXEC 2000000)
(define-public EPOLL_NONBLOCK 4000)

(define-public EPOLLIN #x001) ; The associated file is available for read operations.
(define-public EPOLLRDNORM #x040) ; identical to EPOLLIN

;; EPOLLPRI is a modifier flag and always augments some other event (such as EPOLLERR).
;; It's use is subsystem dependent, as it may mean somewhat different things depending
;; on what purpose associated file descriptor serves.
(define-public EPOLLPRI #x002) ; There is urgent data available for read operations.

(define-public EPOLLOUT #x004) ; The associated file is available for write operations. 
(define-public EPOLLWRNORM #x100) ; identical to EPOLLOUT

;; on some sockets there will be the data send with MSG_OOB flag passed to socket.
(define-public EPOLLRDBAND #x080) ; out of band data on the descriptor for read operations.
(define-public EPOLLWRBAND #x200) ; out of band data on the descriptor for write operations.

(define-public EPOLLMSG #x400) ; unused by the kernel and appears to serve no purpose.

;; Error condition happened on the associated file descriptor.  epoll_wait(2) will always
;; wait for this event; it is not necessary to set it in events.
(define-public EPOLLERR #x008)

;; Hang up happened on the associated file descriptor.  epoll_wait(2) will always wait for
;; this event it is not necessary to set it in events.
(define-public EPOLLHUP #x010) ; signals an unexpected close of the socket, i.e. usually an internal error

(define-public EPOLLRDHUP #x2000) ; detect peer shutdown

;; Sets the one-shot behavior for the associated file descriptor.  This means that after
;; an event is pulled out with epoll_wait(2) the associated file descriptor is internally
;; disabled and no other events will be reported by the epoll interface.  The user must
;; call epoll_ctl() with EPOLL_CTL_MOD to rearm the file descriptor with a new event mask.
(define-public EPOLLONESHOT (ash 1 30))

;; Sets the Edge Triggered behavior for the associated file descriptor.  The default
;; behavior for epoll is Level Triggered.
(define-public EPOLLET (ash 1 31))

;; Valid opcodes ( "op" parameter ) to issue to epoll_ctl.
(define-public EPOLL_CTL_ADD 1) ; Add a file decriptor to the interface.
(define-public EPOLL_CTL_DEL 2) ; Remove a file decriptor from the interface.
(define-public EPOLL_CTL_MOD 3) ; Change file decriptor epoll_event structure.

(define-public epoll-data-meta (list '* int uint32 uint64))
(define-public epoll-data-size (sizeof epoll-data-meta))

(define-public (epoll-data-ptr ed) (car ed))
(define-public (epoll-data-ptr-set! ed ptr)
  (list-set! (cadr ed) 0 ptr))
(define-public (epoll-data-fd ed) (cadr ed))
(define-public (epoll-data-fd-set! ed fd)
  (list-set! (cadr ed) 1 fd))
(define-public (epoll-data-u32 ed) (caddr ed))
(define-public (epoll-data-u32-set! ed u32)
  (list-set! (cadr ed) 2 u32))
(define-public (epoll-data-u64 ed) (cadddr ed))
(define-public (epoll-data-u64-set! ed u64)
  (list-set! (cadr ed) 3 u64))

(define-public epoll-event-meta (list uint32 epoll-data-meta))
(define-public (make-epoll-event fd events)
  (let ((ees (make-bytevector %sizeof-struct-epoll-event)))
    (bytevector-s32-native-set! ees (fd-offset 0) fd)
    (bytevector-u32-native-set! ees (events-offset 0) events)
    ees))
(define (parse-epoll-event e)
  (parse-c-struct epoll-event-meta e))

(define-public (epoll-event-events ee) (car ee))
(define-public (epoll-event-events-set! ee e)
  (list-set! ee 0 e))
(define-public (epoll-event-data ee) (cadr ee))
(define-public (epoll-event-data-set! ee data)
  (list-set! ee 1 data))

;; FIXME: These sizes are fine on x64, but I'm not sure for i386
(define %sizeof-struct-epoll-event 12)
(define %offsetof-struct-epoll-event-fd 4)
(define epoll-event-size %sizeof-struct-epoll-event)

(define (fd-offset n)
  (+ (* n %sizeof-struct-epoll-event)
     %offsetof-struct-epoll-event-fd))

(define (events-offset n)
  (* n %sizeof-struct-epoll-event))

(define epoll-guardian (make-guardian))
(define (pump-epoll-guardian)
  (let ((ees (epoll-guardian)))
    (when ees
      (format (current-error-port)
              "[WARN] epoll-event-set was pumped, which I dislike!~%")
      (pump-epoll-guardian))))
(add-hook! after-gc-hook pump-epoll-guardian)

(define-public (make-epoll-event-set)
  (let* ((max (get-conf '(server wqlen)))
         (ees (make-bytevector (* max epoll-event-size))))
    (epoll-guardian ees)
    ees))

;; Creates an epoll instance.  Returns an fd for the new instance.
;; The "size" parameter is a hint specifying the number of file
;; descriptors to be associated with the new instance.  The fd
;; returned by `epoll-create' should be closed with `close'.
(define %epoll-create
  (pointer->procedure int
                      (dynamic-func "epoll_create" (dynamic-link))
                      (list int)))

(define-public (epoll-create size)
  (let* ((efd (%epoll-create size))
         (err (errno)))
    (cond
     ((>= efd 0) efd)
     (else
      (throw 'artanis-err 500 epoll-create "~S: ~A"
             (list size (strerror err))
             (list err))))))

;; Same as epoll_create but with an FLAGS parameter.  The unused SIZE
;; parameter has been dropped.
(define %epoll-create1
  (pointer->procedure int
                      (dynamic-func "epoll_create1" (dynamic-link))
                      (list int)))

(define-public (epoll-create1 flag)
  (let* ((efd (%epoll-create1 flag))
         (err (errno)))
    (cond
     ((>= efd 0) efd)
     (else
      (throw 'artanis-err 500 epoll-create1 "~S: ~A"
             (list flag (strerror err))
             (list err))))))

;; Manipulate an epoll instance "epfd". Returns 0 in case of success,
;; -1 in case of error ( the "errno" variable will contain the
;; specific error code ) The "op" parameter is one of the EPOLL_CTL_*
;; constants defined above. The "fd" parameter is the target of the
;; operation. The "event" parameter describes which events the caller
;; is interested in and any associated user data.
(define %epoll-ctl
  (pointer->procedure int
                      (dynamic-func "epoll_ctl" (dynamic-link))
                      (list int int int '*)))

(define* (epoll-ctl epfd op fd event #:key (keep-alive? #f))
  (let* ((ret (%epoll-ctl epfd op fd (if event
                                         (bytevector->pointer event)
                                         %null-pointer)))
         (err (errno)))
    (cond
     ((zero? ret) ret)
     ((and (= ret EEXIST) keep-alive?)
      (DEBUG "The event ~a exist and kept alive~%" fd)
      0)
     (else
      (throw 'artanis-err 500 epoll-ctl "~a: ~a"
             (list epfd op fd event (strerror err))
             (list err))))))
(export epoll-ctl)

;; NOTE: do NOT use this function outside this module!!!
(define (epoll-event-set->list ees nfds)
  (let lp((i 0) (ret '()))
    (if (< i nfds)
        (lp (1+ i)
            (acons (bytevector-s32-native-ref ees (fd-offset i))
                   (bytevector-u32-native-ref ees (events-offset i))
                   ret))
        ret)))

;; Wait for events on an epoll instance "epfd". Returns the number of
;; triggered events returned in "events" buffer. Or -1 in case of
;; error with the "errno" variable set to the specific error code. The
;; "events" parameter is a buffer that will contain triggered
;; events. The "maxevents" is the maximum number of events to be
;; returned ( usually size of "events" ). The "timeout" parameter
;; specifies the maximum wait time in milliseconds (-1 == infinite).
(define %epoll-wait
  (pointer->procedure int
                      (dynamic-func "epoll_wait" (dynamic-link))
                      (list int '* int int)))

(define-public (epoll-wait epfd events timeout)
  (let* ((maxevents (get-conf '(server wqlen)))
         (ret (%epoll-wait epfd (bytevector->pointer events) maxevents timeout))
         (err (errno)))
    (cond
     ((>= ret 0) (epoll-event-set->list events ret))
     (else
      (throw 'artanis-err 500 epoll-wait "~S: ~A"
             (list epfd events maxevents timeout (strerror err))
             (list err))))))

;; Same as epoll_wait, but the thread's signal mask is temporarily
;; and atomically replaced with the one provided as parameter.
(define %epoll-pwait
  (pointer->procedure int
                      (dynamic-func "epoll_pwait" (dynamic-link))
                      (list int '* int int '*)))

(define-public (epoll-pwait epfd events maxevents timeout sigmask)
  (let* ((maxevents (get-conf '(server wqlen)))
         (ret (%epoll-pwait epfd (bytevector->pointer events) maxevents timeout sigmask))
         (err (errno)))
    (cond
     ((>= ret 0) (epoll-event-set->list events ret))
     (else
      (throw 'artanis-err 500 epoll-pwait "~S: ~A"
             (list epfd events maxevents timeout sigmask (strerror err))
             (list err))))))

(define-public (is-peer-shutdown? e)
  (not (zero? (logand (cdr e) EPOLLRDHUP))))
