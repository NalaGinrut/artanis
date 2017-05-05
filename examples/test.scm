#! /usr/bin/env guile
!#

;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013,2014,2015
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

(use-modules (artanis artanis) (artanis utils))
;; exclude *.html static file to be handled by default static-page-emitter
;; we need to handle foo.html in rule test-18, this can avoid the conflict.
(init-server #:exclude '(html))

(define mmr "123")

;; 1
(get "/mmr/:id1/to/:id2"
  (lambda (rc)
    (let ((id1 (params rc "id1"))
          (id2 (params rc "id2")))
      (response-emit (format #f "~a: Send from ~a to ~a~%" mmr id1 id2)))))

;; 2
(get "/hello.scm"
  (lambda (rc)
    (response-emit "hello world!")))

;; 3
(get "/test"
  (lambda (rc)
    (let ((a 123))
      (tpl->response "my.tpl" (the-environment)))))

;; 4
;; simple cache test (for dynamic content)
(get "/new" #:cache #t
  (lambda (rc)
    (:cache rc "hello world")))

;; 4.1
;; cache in auth should be private
(get "/pauth" #:auth `(basic ,(lambda (rc u p) #t)) #:cache '(public "./test.scm")
  (lambda (rc)
    (:cache rc)))

;; test database (here we use mysql/mariaDB for an example)
;; there's a table for testing:
;; CREATE TABLE Persons
;; (
;; PersonID int,
;; LastName varchar(255),
;; FirstName varchar(255),
;; Address varchar(255),
;; City varchar(255)
;; );

;; And insert some data:

;; insert into Persons
;; (PersonID,Lastname,Firstname,Address,City)
;; values (1,"lei","mu","adsf","sz");

;; Add this person for testing SQL-injection:
;; insert into Persons (PersonID,Lastname,Firstname,Address, City)
;; values (2,"ada","wang","secret","classified")

(define (result->html r)
  (if r
      (call-with-output-string
       (lambda (port)
         (for-each (lambda (e) (format port "<p>~a: ~a</p>" (car e) (cdr e))) r)))
      "no result"))

;; 5
(get "/raw-sql"
     #:raw-sql "select * from Persons where Lastname='lei'"
  (lambda (rc)
    (let ((r (:raw-sql rc 'top)))
      (result->html r))))

;; 6
;; curl localhost:3000/conn/lei
(get "/conn/:name"
     #:conn #t
  (lambda (rc)
    (let* ((name (params rc "name"))
           (r (:conn rc (->sql select * from 'Persons (where #:Lastname name)))))
      (result->html (DB-get-top-row r)))))

;; 7
;; curl "localhost:3000/conn/lei;select * from Persons;"
(get "/fucked/:name"
     #:conn #t
  (lambda (rc)
    (let* ((name (uri-decode (params rc "name")))
           (r (:conn rc (->sql select * from 'Persons (where #:Lastname name)))))
      (result->html (DB-get-top-row r)))))

;; 8
;; curl localhost:3000/conn+str/lei
(get "/conn[+]str/:name"
     #:conn #t #:str "select * from Persons where Lastname=${:name}"
  (lambda (rc)
    (let ((r (:conn rc (:str rc))))
      (result->html (DB-get-top-row r)))))

;; 9
;; various format tests
(get "/json" #:mime 'json
  (lambda (rc)
    (let ((j (json (object ("name" "nala") ("age" "15")))))
      (:mime rc j))))

;; 9.1
;; JSONP test
(get "/jsonp/:callback" #:mime 'jsonp
  (lambda (rc)
    (:mime rc (json (object ("name" "nala") ("age" "15"))) #:jsonp (params rc "callback"))))

;; 10
(get "/csv" #:mime 'csv
  (lambda (rc)
    (:mime rc '(("a" "1") ("b" "2")))))

;; 11
(get "/xml" #:mime 'xml
  (lambda (rc)
    (:mime rc '(*TOP* (WEIGHT (@ (unit "pound")) (NET (@ (certified "certified")) "67") (GROSS "95"))))))

;; 12
(get "/sxml" #:mime 'sxml
  (lambda (rc)
    (:mime rc '((a 1) (b 2)))))

;; 13
;; cookies test
(get "/cookie" #:cookies '(names cc)
  (lambda (rc)
    (:cookies-set! rc 'cc "sid" "123321")
    (:cookies-ref rc 'cc "sid")))

;; 14
(get "/cookie/:expires" #:cookies '(names cc)
  (lambda (rc)
    (:cookies-set! rc 'cc "sid" "123321")
    (:cookies-setattr! rc 'cc #:expir (string->number (params rc "expires")))
    "ok"))

;; 15
;; test for naive basic-auth
(get "/bauth" #:auth `(basic ,(lambda (rc u p) (and (string=? u "mmr") (string=? p "123"))))
  (lambda (rc) 
    (if (:auth rc)
        "auth ok"
        (throw-auth-needed))))

;; 16
;; test for more complicated auth
(post "/auth" #:auth '(table user "user" "passwd") #:session #t
  (lambda (rc)
    (cond
     ((:session rc 'check) "auth ok (session)")
     ((:auth rc) (:session rc 'spawn))
     (else (redirect-to rc "/login?login_failed=true")))))

;; 16.1
;; test for login successful
(get "/enter" #:session #t
  (lambda (rc)
    (cond
     ((:session rc 'check) "yes login!")
     (else (redirect-to rc "/login?login_failed=true")))))

;; 17
(get "/login"
  (lambda (rc)
    (let ((blog-title "test auth")
          (footer "<p>Powered by GNU Artanis</p>")
          (failed (params rc "login_failed")))
      (tpl->response "login.tpl" (the-environment)))))

;; 18
;; to support dot as the delimiter of key-bindings in rule
(get "/pkg/:name\\.:format"
  (lambda (rc)
   (format #f "~a.~a" (params rc "name") (params rc "format"))))

(run #:use-db? #t #:dbd 'mysql #:db-username "root" #:db-passwd "123" #:debug #t)

;;(run)
