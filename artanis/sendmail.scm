;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013
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

(define-module (artanis sendmail)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis mime)
  #:use-module (artanis base64)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 popen))

(module-export-all! (current-module))

;; TODO: customize the printer in case too much info printed when debugging 
(define-record-type <sendmail>
  (%make-sendmail sender from to subject message headers attachements)
  sendmail?
  (sender sendmail-sender sendmail-sender!) ; sendmail compatible MTA
  (from sendmail-from sendmail-from!)
  (to sendmail-to sendmail-to!)
  (subject sendmail-subject sendmail-subject!)
  (message sendmail-message sendmail-message!)
  (headers sendmail-headers sendmail-headers!)
  (attachements sendmail-attachements sendmail-attachements!))

(set-record-type-printer! <sendmail>
  (lambda (sm port)
    (display "\n#<sendmail\n" port)
    (format port "   sender: ~a~%" (sendmail-sender sm))
    (format port "   from: ~a~%" (sendmail-from sm))
    (format port "   to: ~a~%" (sendmail-to sm))
    (format port "   subject: ~a~%" (sendmail-subject sm))
    (format port "   message: <the message>...~%")
    (format port "   headers: ~a~%" (sendmail-headers sm))
    (format port "   attachements: <the data>...~%")
    (display " >\n" port)))

(define-syntax-rule (add-header sm new-header)
  (let ((header (sendmail-header sm)))
    (sendmail-header! sm (cons new-header header))))

(define-syntax-rule (add-attachment sm file-with-path)
  (if (not (file-exists? file-with-path)) 
      (error 'artanis-err 500 "can't find attachment file" file-with-path)
      (let* ((file (basename file-with-path))
             (bv (bv-cat file #f))
             (al (sendmail-attachements sm)))
        (sendmail-attachements! sm (cons (cons file bv) al)))))

;; ENHANCE: do we need a customizable transfer encoding?
(define (dump-all-attachments port boundry sm)
  (define bdr (string-append "--" boundry "\n"))
  (define bdr-end (string-append "--" boundry "--\n"))
  (for-each (lambda (att)
              (let* ((filename (car att))
                     (mime (guess-mime filename))
                     (content (cdr att)))
                (display bdr port)
                (display (string-append "Content-Type: application; name=\""
                                        filename "\"\n") port)
                (display "Content-Transfer-Encoding: base64\n" port)
                (display (string-append "Content-Disposition: attachmet; filename=\""
                                        filename "\"\n") port)
                (display (base64-encode content) port)(newline port)))
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
     (display (sendmail-message sm) port)(newline port)
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
         (port (open-pipe* OPEN_WRITE sender "-i" "-t")))
    (display t port)
    (unless (zero? (status:exit-val (close-pipe port)))
      (error 'artanis-err 500 "mail command failed" sm))))

(define-syntax-rule (no-attachments? sm)
  (null? (sendmail-attachements sm)))

(define (send-the-mail sm)
  (and (not (sendmail? sm)) (error 'artanis-err 500 "invalid <sendmail> object" sm))
  (let ((t (if (no-attachments? sm) 
               (dump-as-normal-mail sm)
               (dump-as-attachments-mail sm))))
    (%send-the-mail sm t)))

;; TODO: maybe delay to send calling sender
(define* (make-simple-mail-sender from to #:key (sender (current-mail-sender)))
  (let ((sm (%make-sendmail sender from to "no subject" #f '() '())))
    (lambda* (message #:key (attachements #f) (header #f) (subject #f))
      (if (string? message) 
          (sendmail-message! sm message)
          (error 'artanis-err 500 "invalid message format, must be string!" message)) 
      (and attachements (sendmail-attachements! sm attachements))
      (and subject (sendmail-subject! sm subject))
      (and header (sendmail-headers! sm header))
      sm)))
      ;;(send-the-mail sm))))
