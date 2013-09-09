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

(get "/new"
  (lambda ()
    "hello world"))

(run)
