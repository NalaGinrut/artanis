;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015,2017
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

(define-module (artanis inotify)
  #:use-module (artanis utils)
  #:use-module ((rnrs)
                #:select (bytevector-s32-native-ref
                          bytevector-s32-native-set!
                          bytevector-u32-native-ref
                          bytevector-u32-native-set!
                          make-bytevector
                          define-record-type))
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:export (new-inotify-watcher
            inotify-watch
            inotify-clean-all-watch
            make-inotify-watching-loop

            make-inotify-event-iterator
            watch-event
            watch-event?
            watch-event-wd
            watch-event-mask
            watch-event-cookie
            watch-event-len
            watch-event-name))

(define-public IN_CLOEXEC #o2000000)
(define-public IN_NONBLOCK #o4000)

(define-public IN_ACCESS        #x00000001) ; File was accessed.
(define-public IN_MODIFY        #x00000002) ; File was modified.
(define-public IN_ATTRIB        #x00000004) ; Metadata changed.
(define-public IN_CLOSE_WRITE   #x00000008) ; Writtable file was closed.
(define-public IN_CLOSE_NOWRITE #x00000010) ; Unwrittable file closed.
(define-public IN_CLOSE (logior IN_CLOSE_WRITE IN_CLOSE_NOWRITE)) ; Close
(define-public IN_OPEN          #x00000020) ; File was opened.
(define-public IN_MOVED_FROM    #x00000040) ; File was moved from X.
(define-public IN_MOVED_TO      #x00000080) ; File was moved to Y.
(define-public IN_MOVE  (logior IN_MOVED_FROM IN_MOVED_TO)) ; Moves.
(define-public IN_CREATE        #x00000100) ; Subfile was created.
(define-public IN_DELETE        #x00000200) ; Subfile was deleted.
(define-public IN_DELETE_SELF   #x00000400) ; Self was deleted.
(define-public IN_MOVE_SELF     #x00000800) ; Self was moved.
 
;; Events sent by the kernel.
(define-public IN_UNMOUNT       #x00002000) ; Backing fs was unmounted.
(define-public IN_Q_OVERFLOW    #x00004000) ; Event queued overflowed.
(define-public IN_IGNORED       #x00008000) ; File was ignored.
 
;; Helper events.
(define-public IN_CLOSE (logior IN_CLOSE_WRITE IN_CLOSE_NOWRITE)) ; Close.
(define-public IN_MOVE  (logior IN_MOVED_FROM IN_MOVED_TO))       ; Moves.

;; Special flags.
(define-public IN_ONLYDIR       #x01000000) ; Only watch the path if it is a directory.
(define-public IN_DONT_FOLLOW   #x02000000) ; Do not follow a sym link.
(define-public IN_EXCL_UNLINK   #x04000000) ; Exclude events on unlinked objects.
(define-public IN_MASK_ADD      #x20000000) ; Add to the mask of an already

(define-public IN_ISDIR         #x40000000) ; Event occurred against dir.
(define-public IN_ONESHOT       #x80000000) ; Only send event once.

;; All events which a program can wait on.
(define-public IN_ALL_EVENTS
  (logior
   IN_ACCESS IN_MODIFY IN_ATTRIB IN_CLOSE_WRITE
   IN_CLOSE_NOWRITE IN_OPEN IN_MOVED_FROM
   IN_MOVED_TO IN_CREATE IN_DELETE
   IN_DELETE_SELF IN_MOVE_SELF))

(define-record-type inotify-watcher
  (fields
   (mutable fd)
   (mutable wd)
   (mutable paths)
   buf))

;;;; Structure describing an inotify event.
;; struct inotify_event
;; {
;;  int wd;               /* Watch descriptor.  */
;;  uint32_t mask;        /* Watch mask.  */
;;  uint32_t cookie;      /* Cookie to synchronize two events.  */
;;  uint32_t len;         /* Length (including NULs) of name.  */
;;  char name __flexarr;  /* Name.  */
;; };
(define inotify-event-meta (list int uint32 uint32 uint32))
(define inotify-event-size (sizeof inotify-event-meta))

(define *max-buf* (* 1024 (+ inotify-event-size 16)))

(define-record-type watch-event (fields wd mask cookie len name))

(define (make-inotify-event-iterator buf size)
  (let ((es buf) (cursor 0))
    (lambda ()
      (cond
       ((>= cursor size) 'inotify-event-itorator-end)
       (else
        (match (parse-c-struct es inotify-event-meta)
          ((wd mask cookie len)
           (let ((offset (+ len inotify-event-size)))
             (let ((we (make-watch-event
                        wd mask cookie len
                        (pointer->string
                         (bytevector->pointer
                          (pointer->bytevector es len inotify-event-size))))))
               (set! es (make-pointer (+ (pointer-address es) offset)))
               (set! cursor (+ cursor offset))
               we)))
          (else (throw 'artanis-err 500
                       "inotify - make-event-iterator: invalid struct!"
                       (parse-c-struct es inotify-event-meta)))))))))

(define inotify-guardian (make-guardian))
(define (pump-inotify-guardian)
  (let ((ies (inotify-guardian)))
    (when ies
          (format (current-error-port)
                  "[WARN] inotify-watcher-buf was pumped, which I dislike!~%")
          (pump-inotify-guardian))))
(add-hook! after-gc-hook pump-inotify-guardian)

(define %inotify-init
  (pointer->procedure int
                      (dynamic-func "inotify_init" (dynamic-link))
                      '() #:return-errno? #t))

(define %inotify-add-watch
  (pointer->procedure int
                      (dynamic-func "inotify_add_watch" (dynamic-link))
                     (list int '* uint32) #:return-errno? #t))

(define %inotify-rm-watch
  (pointer->procedure int
                      (dynamic-func "inotify_rm_watch" (dynamic-link))
                      (list int int) #:return-errno? #t))

(define (inotify-init)
  (call-with-values
      (lambda () (%inotify-init))
    (lambda (ifd  errno)
      (cond
       ((= ifd 0) ifd)
       (else
        (throw 'system-error "inotify-init" "~S: ~A"
               (list (strerror errno))
               (list errno)))))))

(define (inotify-add-watch fd pathname mask)
  (call-with-values
      (lambda ()
        (%inotify-add-watch fd (string->pointer pathname) mask))
    (lambda (ret errno)
      (cond
       ((= ret 0) ret)
       (else
        (throw 'system-error "inotify-add-watch" "~S: ~A"
               (list (strerror errno))
               (list errno)))))))

(define (inotify-rm-watch fd wd)
  (call-with-values
      (lambda () (%inotify-rm-watch fd wd))
    (lambda (ret errno)
      (cond
       ((= ret 0) ret)
       (else
        (throw 'system-error "inotify-rm-watch" "~S: ~A"
               (list (strerror errno))
               (list errno)))))))

(define (new-inotify-watcher)
  (let ((buf (make-bytevector *max-buf*)))
    (inotify-guardian buf)
    (make-inotify-watcher (inotify-init) #f '() (bytevector->pointer buf))))

(define* (inotify-watch paths #:optional (mask (logior IN_MODIFY IN_CREATE IN_DELETE)))
  (let* ((w (new-inotify-watcher))
         (fd (inotify-watcher-fd w)))
    (inotify-watcher-wd-set!
     w
     (map (lambda (p)
            (cons p (inotify-add-watch fd p mask)))
          paths)) 
    (inotify-watcher-paths-set! w paths)
    w))

(define (inotify-clean-all-watch w)
  (for-each
   (lambda (pp)
     (inotify-rm-watch (inotify-watcher-fd w) (car pp)))
   (inotify-watcher-paths-set! w '())))

(define (inotify-clean-watch w p)
  (let ((wd (assoc-ref (inotify-watcher-paths w) p)))
    (cond
     ((not wd) #f)
     (else
      (assoc-remove! (inotify-watcher-paths w) p)
      (inotify-rm-watch (inotify-watcher-fd w) wd)))))

(define posix-read
  (pointer->procedure int
                      (dynamic-func "read" (dynamic-link))
                      (list int '* size_t)))

(define (make-inotify-watching-loop . paths)
  (let* ((w (inotify-watch paths))
         (fd (inotify-watcher-fd w))
         (buf (inotify-watcher-buf w)))
    (lambda ()
      (match (select (list fd) '() '() 0 500)
        (((i) () ())
         (let ((size (posix-read i buf *max-buf*))) 
           (make-inotify-event-iterator buf size)))
        (else (lambda () 'inotify-event-itorator-end))))))
