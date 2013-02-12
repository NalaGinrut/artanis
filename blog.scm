#! /usr/local/bin/guile \
-L ./
!#

;; This is a very simple blog example for artanis

(use-modules (artanis artanis)
             (artanis session)
             (artanis utils)
             (artanis db)
             (oop goops))

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
    (let* ((sid (params rc "sid"))
           (session (session-restore sid)))
      (cond
       ((or (not session) (session-ref session "current_user_id"))
        ;;(throw-auth-needed)) ; need authority
        (response-emit
         (tpl->html
          `(html (body
                  (p "Please login first!")
                  (form (@ (id "login") (action "/login") (method "POST"))
                        "user name: " (input (@ (type "text") (name "user")))(br)
                        "password : " (input (@ (type "password") (name "pwd")))(br)
                        (input (@ (type "submit") (value "Submit")))))))))
       (else (response-emit ""))))))

(post "/login"
      (lambda (rc)
        (let ((id "3") ; just for test
              (user (params rc "user"))
              (pwd (params rc "pwd")))
          (query blog-db (format #f "insert into ~a values (~a,~s,~s)" 
                                 "user" id user (string->md5 pwd)))
          (format #t "~a~%" (get-status blog-db))
          (response-emit (format #f "Registered!~%id:~a~%user: ~a~%password:~a"
                                 id user pwd)))))

(get "/show/:the_user"
     (lambda (rc)
       (let ((user (params rc "the_user")))
         (query blog-db (format #f "select * from user where user = ~s" user))
         (let* ((row (get-one-row blog-db))
                (id (assoc-ref row "id"))
                (user (assoc-ref row "user"))
                (passwd (assoc-ref row "passwd")))
           (response-emit (format #f "result:</br>~%id:~a</br>user:~a</br>passwd:~a"
                                  id user passwd))))))
        
(run)
