;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014
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

;; ---------------------------------------------------------------------
;; This module is the brand new high performance concurrency server of
;; Artanis, which is based on Guile's powerful delimited-continuations.
;;
;; NOTE: I have no plan to finish this server module soon, because it's
;;       not in a high priority at present. But I have to provide some
;;       parameters for other modules in advance.

(define-module (artanis server)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:export (current-worker
            current-connection-pool))

(define current-worker (make-parameter #f))
(define current-connection-pool (make-parameter #f))
