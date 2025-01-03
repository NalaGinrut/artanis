;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2024
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

(define-module (artanis cli)
  #:use-module (ice-9 popen)
  #:use-module ((rnrs) #:select (get-string-all
                                 define-record-type))
  #:export (<cli>
            <cli>?
            <cli>-cmd
            <cli>-status
            <cli>-result

            cli-run
            cli-run*
            run-with-cli
            run-with-cli*))

(define-record-type <cli>
  (fields
   cmd
   status
   result))

(define (cli-run cmd)
  (let* ((port (open-input-output-pipe cmd))
         (result (get-string-all port))
         (status (status:exit-val (close-pipe port))))
    (make-<cli> cmd status result)))

;; open an input and output pipe, and write string into the input pipe
(define (run-with-cli cmd proc)
  (let ((port (open-input-output-pipe cmd)))
    (proc port)
    (display #\null port)
    (force-output port)
    (let* ((result (get-string-all port))
           (status (status:exit-val (close-pipe port))))
      (make-<cli> cmd status result))))

(define-syntax-rule (cli-run* cmd ...)
  (cli-run (format #f "~{~a ~}" (list `cmd ...))))

(define-syntax-rule (run-with-cli* cmd ... proc)
  (run-with-cli (format #f "~{~a ~}" `(cmd ...)) proc))
