#! /usr/local/bin/guile \
-L ./
!#

;; This is a very simple blog example for artanis

(use-modules (artanis artanis)
             (artanis session)
             (artanis utils)
             (artanis db)
             (oop goops) (srfi srfi-1))

;; FOR USERS:
;;   Please modify to your own mysql configure
;; mysql info:
;;   database: mmr_blog
;;   table:    user
;;             (id int, user varchar, passwd varchar)      
(define blog-db (make <mysql> #:user "root" #:name "mmr_blog"))
(conn blog-db "user" "") ; user is table-name, "" is the passwd of database

(get "/admin"
  (lambda (rc)
    (cond
     ((has-auth? rc)
      ; TODO: show admin page, just title/author/content and a button
      )
     (else (redirect-to rc "/login")))))

(get "/login"
     (lambda ()
       (response-emit
        (tpl->html
         `(html (body
                 (p "Please login first!")
                 (form (@ (id "login") (action "/auth") (method "POST"))
                       "user name: " (input (@ (type "text") (name "user")))(br)
                       "password : " (input (@ (type "password") (name "pwd")))(br)
                       (input (@ (type "submit") (value "Submit"))))))))))

(post "/auth"
      (lambda (rc)
        (let ((id "3") ; just for test
              (user (params rc "user"))
              (pwd (params rc "pwd")))
          (cond
           ((and user pwd)
            (query blog-db (format #f "insert into ~a values (~a,~s,~s)" 
                                   "user" id user (string->md5 pwd)))
            (format #t "~a~%" (get-status blog-db))
            (redirect-to rc "/")
            (response-emit (format #f "Registered!~%id:~a~%user: ~a~%password:~a"
                                   id user pwd)))
           (else ; invalid auth request
            (redirect-to rc "/login"))))))

(define (get-all-articles)
  (query blog-db "select * from articles")
  (fold (lambda (x p)
          (let ((title (assoc-ref x "title"))
                (date (assoc-ref x "date"))
                (content (assoc-ref x "content")))
            (cons `(div (@ (class "post")) (h2 ,title) (h3 ,title) (p ,content)) p)))
        '() (get-all-rows blog-db)))

(get "/"
     (lambda (rc)
       (response-emit 
        (tpl->html
         `(html (body
                 (h1 "This is a blog example")
                 ,(get-all-articles)))))))
        
(run)
