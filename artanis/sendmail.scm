;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015,2019
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

(define-module (artanis sendmail)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis mime)
  #:use-module ((rnrs) #:select (define-record-type))
  #:use-module (ice-9 popen)
  #:export (make-simple-mail-sender
            send-the-mail))

(define-record-type sendmail
  (fields
   account
   sender
   from
   to
   (mutable subject)
   (mutable message)
   (mutable headers)
   (mutable attachements)))

(::define (print-sendmail-record sm)
  (:anno: (sendmail) -> ANY)
  (call-with-output-string
   (lambda (port)
     (display "\n#<sendmail\n" port)
     (format port "   sender: ~a~%" (sendmail-sender sm))
     (format port "   account: ~a~%" (sendmail-account sm))
     (format port "   from: ~a~%" (sendmail-from sm))
     (format port "   to: ~a~%" (sendmail-to sm))
     (format port "   subject: ~a~%" (sendmail-subject sm))
     (format port "   message: <the message>...~%")
     (format port "   headers: ~a~%" (sendmail-headers sm))
     (format port "   attachements: <the data>...~%")
     (display " >\n" port))))

(define-syntax-rule (add-header! sm new-header)
  (let ((header (sendmail-header sm)))
    (sendmail-header-set! sm (cons new-header header))))

(define-syntax-rule (add-attachment! sm file-with-path)
  (if (not (file-exists? file-with-path))
      (throw 'artanis-err 500 add-attachment!
             "Can't find attachment file `~a'" file-with-path)
      (let* ((file (basename file-with-path))
             (bv (bv-cat file #f))
             (al (sendmail-attachements sm)))
        (sendmail-attachements-set! sm (cons (cons file bv) al)))))

;; ENHANCE: do we need a customizable transfer encoding?
(define (dump-all-attachments port boundry sm)
  (define bdr (string-append "--" boundry "\n"))
  (define bdr-end (string-append "--" boundry "--\n"))
  (for-each
   (lambda (att)
     (let* ((filename (car att))
            (mime (guess-mime filename))
            (content (cdr att)))
       (display bdr port)
       (display (string-append "Content-Type: application; name=\""
                               filename "\"\n") port)
       (display "Content-Transfer-Encoding: base64\n" port)
       (display (string-append "Content-Disposition: attachmet; filename=\""
                               filename "\"\n") port)
       (display (nss:base64-encode content) port)
       (newline port)))
   (sendmail-attachements sm))
  (display bdr-end port))

;; TODO: don't dump the header which exists in customed headers list
(define (dump-headers port sm)
  (for-each (lambda (p)
              (format port "~a: ~a~%" (car p) (cdr p)))
            (sendmail-headers sm)))

(define (generate-boundary)
  (string-append "boundry-"(get-random-from-dev)))

(define (dump-as-attachments-mail sm)
  (define boundry (generate-boundary))
  (define bdr (string-append "--" boundry "\n"))
  (call-with-output-string
    (lambda (port)
      (format port "From: ~a~%To: ~a~%Subject: ~a~%"
              (sendmail-from sm) (sendmail-to sm) (sendmail-subject sm))
      (display "MIME-Version: 1.0\n" port)
      (dump-headers port sm)
      (display (string-append "Content-Type: multipart/mixed; boundary=\""
                              boundry "\"\n\n") port)
      (display bdr port)
      (display "Content-Type: text/html\n" port)
      (display "Content-Disposition: inline\n" port)
      (display (sendmail-message sm) port)
      (newline port)
      (dump-all-attachments port boundry sm))))

(define (dump-as-normal-mail sm)
  (call-with-output-string
   (lambda (port)
     (format port
             "From: ~a~%To: ~a~%Subject: ~a~%~%~a~%"
             (sendmail-from sm) (sendmail-to sm) (sendmail-subject sm)
             (sendmail-message sm)))))

(define (%send-the-mail sm t)
  (let* ((sender (sendmail-sender sm))
         (account (sendmail-account sm))
         (a-arg (if account
                    (format #f "-a ~a" account)
                    ""))
         (port (open-pipe* OPEN_WRITE sender a-arg "-i" "-t")))
    (display t port)
    (unless (zero? (status:exit-val (close-pipe port)))
      (throw 'artanis-err 500 %send-the-mail
             "Sendmail command failed: `~a'" sm))))

(define-syntax-rule (no-attachments? sm)
  (null? (sendmail-attachements sm)))

(define (send-the-mail sm)
  (when (not (sendmail? sm))
    (throw 'artanis-err 500 send-the-mail
           "Invalid sendmail object `~a'" sm))
  (let ((t (if (no-attachments? sm)
               (dump-as-normal-mail sm)
               (dump-as-attachments-mail sm))))
    (%send-the-mail sm t)))

;; TODO: maybe delay to send calling sender
(define* (make-simple-mail-sender from to
                                  #:key (sender (get-conf '(mail sender)))
                                  (account #f))
  (let ((sm (make-sendmail sender from to "no subject" #f '() '())))
    (lambda* (message #:key (attachements #f) (header #f) (subject #f))
      (if (string? message)
          (sendmail-message-set! sm message)
          (throw 'artanis-err 500 make-simple-mail-sender
                 "Invalid message format `~a', must be string!"
                 message))
      (and attachements (sendmail-attachements-set! sm attachements))
      (and subject (sendmail-subject-set! sm subject))
      (and header (sendmail-headers-set! sm header))
      sm)))
