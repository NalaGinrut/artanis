#! /usr/local/bin/guile \
-L ./
!#

;; This is a very simple blog example for artanis

(use-modules (artanis artanis) (artanis session) (artanis utils) (artanis db)
             (oop goops) (srfi srfi-1) (web uri))

(init-server) ;; make sure call init-server at beginning

(define blog-db (make <mysql> #:user "root" #:name "mmr_blog"))
(conn blog-db "user" "") ; user is table-name, "" is the passwd of database

(get "/admin"
  (lambda (rc)
    (cond
     ((has-auth? rc)
      (response-emit
       (tpl->html
        `(html (body
                (p "edit your article")
                (form (@ (id "post_article") (action "/new_post") (method "POST"))
                      "title: " (input (@ (type "text") (name "title")))(br)
                      "content:" 
                      (textarea (@ (name "content") (rows "25") (cols "38")) 
                                "write something")(br)
                      (input (@ (type "submit") (value "Submit")))))))))
     (else (redirect-to rc "/login")))))

(get "/login"
     (lambda (rc)
       (response-emit
        (tpl->html
         `(html (body
                 (p ,(if (params rc "login_failed") "Invalid user name or password!"
                         "Please login first!"))
                 (form (@ (id "login") (action "/auth") (method "POST"))
                       "user name: " (input (@ (type "text") (name "user")))(br)
                       "password : " (input (@ (type "password") (name "passwd")))(br)
                       (input (@ (type "submit") (value "Submit"))))))))))

(post "/auth"
      (lambda (rc)
        (let ((user (params rc "user"))
              (pwd (params rc "passwd")))
          (cond
           ((and user pwd)
            (query blog-db (format #f "select * from user where user=~s" user))
            (if (string=? pwd (assoc-ref (get-one-row blog-db) "passwd"))
                (call-with-values
                    (lambda () (session-spawn rc))
                  (lambda (sid session)
                    (redirect-to rc (format #f "/admin?sid=~a" sid)))) ; auth OK
                (redirect-to rc "/login?login_failed=true"))) ; auth failed, relogin!
           (else ; invalid auth request
            (redirect-to rc "/login"))))))

(define (get-all-articles)
  (query blog-db "select * from article")
  (fold (lambda (x p)
          (let ((title (uri-decode (assoc-ref x "title")))
                (content (uri-decode (assoc-ref x "content")))
                (date (assoc-ref x "date")))
            (cons `(div (@ (class "post")) (h2 ,title) (h3 ,date) (p ,content)) p)))
        '() (get-all-rows blog-db)))

(get "/$"
     (lambda (rc)
       (response-emit 
        (tpl->html
         `(html (body
                 (h1 "This is a blog example")
                 ,(get-all-articles)))))))

(post "/new_post"
      (lambda (rc)
        (let ((title (params rc "title"))
              (content (params rc "content"))
              (date (strftime "%D" (localtime (current-time)))))
          (query blog-db 
                 (format #f 
                         "insert into article (title,content,date) values (~s,~s,~s)"
                         title content date))
          (redirect-to rc "/"))))
                                         
(run)
