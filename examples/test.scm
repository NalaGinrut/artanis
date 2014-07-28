#! /usr/local/bin/guile \
-L ../
!#


(use-modules (artanis artanis) (artanis utils))
(init-server)

(define mmr "123")

(get "/mmr/:id1/to/:id2"
  (lambda (rc)
    (let ((id1 (params rc "id1"))
          (id2 (params rc "id2")))
      (response-emit (format #f "~a: Send from ~a to ~a~%" mmr id1 id2)))))

(get "/hello.scm"
  (lambda (rc)
    (response-emit "hello world!")))

(get "/test"
  (lambda (rc)
    (let ((a 123))
      (tpl->response "my.tpl" (the-environment)))))

;; simple cache test (for dynamic content)
(get "/new" #:cache #t
  (lambda (rc)
    (:cache rc "hello world")))

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

(get "/raw-sql"
     #:raw-sql "select * from Persons where Lastname='lei'"
  (lambda (rc)
    (let ((r (:raw-sql rc 'top)))
      (result->html r))))

;; curl localhost:3000/conn/lei
(get "/conn/:name"
     #:conn #t
  (lambda (rc)
    (let* ((name (params rc "name"))
           (r (:conn rc (->sql select * from 'Persons (where #:Lastname name)))))
      (result->html (DB-get-top-row r)))))

(use-modules (web uri))
;; curl localhost:3000/conn/lei;select * from Persons;
(get "/fucked/:name"
     #:conn #t
  (lambda (rc)
    (let* ((name (uri-decode (params rc "name")))
           (r (:conn rc (->sql select * from 'Persons (where #:Lastname name)))))
      (result->html (DB-get-top-row r)))))

;; curl localhost:3000/conn+str/lei
(get "/conn[+]str/:name"
     #:conn #t #:str "select * from Persons where Lastname=${:name}"
  (lambda (rc)
    (let ((r (:conn rc (:str rc))))
      (result->html (DB-get-top-row r)))))

(run #:use-db? #t #:dbd 'mysql #:db-username "root" #:db-passwd "123" #:debug #t)

;;(run)
