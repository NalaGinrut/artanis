;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015
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

(define-module (artanis commands work)
  #:use-module (artanis commands)
  #:use-module (artanis artanis)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match))

;; `art work' command is used to run the server and make the site/app work
;; for clients/browsers.

(define %summary "Run the server and make the site/app work.")

(define option-spec
  '((help (value #f))
    (port (single-char #\p) (value #t))
    (usedb (single-char #\d) (value #f))
    (dbd (single-char #\b) (value #t))
    (name (single-char #\n) (value #t))
    (host (single-char #\h) (value #t))
    (user (single-char #\u) (value #t))
    (passwd (single-char #\w) (value #t))
    (debug (single-char #\g) (value #f))
    ;; NOTE: this is used for new server-core in the future,
    ;;       but now it's just useless.
    (server (single-char #\s) (value #t))))

(define (try-load-entry)
  (define entry (format #f "~a/ENTRY" (getcwd)))
  (when (not (file-exists? entry))
        (error try-load-entry "No ENTRY! Are you in toplevel dir?"))
  (load entry))
        
(define (work . args)
  (let ((options (if (null? args) '() (getopt-long args option-spec))))
    (define-syntax-rule (->opt k) (option-ref options k #f))
    (cond
     ((option-ref options 'help #f) (show-help))
     (else
      (try-load-entry)
      (run #:host (->opt 'host)
           #:port (and=> (->opt 'port) string->number)
           #:debug (->opt 'debug)
           #:use-db? (->opt 'usedb)
           #:dbd (and=> (->opt 'dbd) string->symbol)
           #:db-name (->opt 'name)
           #:db-username (->opt 'user)
           #:db-passwd (->opt 'passwd))))))

(define help-str
"
Usage:
  art work [options]

Options:
  -h, [--host=HOST]              # Specify the network host
                                   Default: 0.0.0.0
  -d, [--usedb]                  # Whether to use Database
                                   Default: false
  -b, [--dbd=DBD]                # Specify DBD, mysql/postgresql/sqlit3
                                   Default: mysql
  -n, [--name=DATABASE_NAME]     # Database name
                                   Default: artanis
  -w, [--passwd=PASSWD]          # Database password
                                   Default: none
  -u, [--user=USER]              # Database user name
                                   Default: root
  -p, [--port=PORT]              # Specify listenning port
                                   Default: 3000
  -g, [--debug]                  # Debug mode
                                   Default: disable
  -s, [--server=SERVER]          # Specify server core
                                   Default: inner (Guile built-in server)
  --help                         # Show this screen
")

(define (show-help)
  (display announce-head)
  (display help-str)
  (display announce-foot))

(define main work)
