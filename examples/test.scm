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
(get "/new$" #:cache #t
  (lambda (rc)
    (:cache rc "hello world")))

;; test database
(get "^/raw-sql/test"
     #:raw-sql "select * from Persons where Lastname='lei'"
  (lambda (rc)
    (let ((r (:raw-sql rc 'top)))
      (display r)(newline)
      (object->string r))))

;; BUG:
;; There's a bug returned #f as body when we enabled DB:
;; e.g:
(run #:use-db? #t #:dbd 'mysql #:db-username "root" #:db-passwd "123" #:debug #t)

;;(run)
