;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2025
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

(define-module (artanis runner)
  #:use-module (artanis cli)
  #:use-module (artanis server)
  #:use-module (artanis server server-context)
  #:use-module (ice-9 futures)
  #:export (call-with-runner))

(define (create-runner thunk)
  (let ((client (current-client)))
    (make-future
     (lambda ()
       (let ((ret (thunk)))
         (oneshot-mention! client)
         ret)))))

(define (call-with-runner thunk)
  (let ((runner (create-runner thunk)))
    (while (not (eq? 'done ((@@ (ice-9 futures) future-state) runner)))
           (break-task))
    (touch runner)))
