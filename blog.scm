#! /usr/local/bin/guile \
-L ./
!#

;; This is a very simple blog example for artanis

(use-modules (artanis artanis) (artanis session) (artanis utils) (artanis db)
             (oop goops) (srfi srfi-1))

(define blog-db (make <mysql> #:user "root" #:name "mmr_blog"))
(conn blog-db "user" "") ; user is table-name, "" is the passwd of database

(get "/admin"
  (lambda (rc)
    (cond
     ((not (has-auth? rc))
      ; TODO: show admin page, just title/author/content and a button
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
                 (p "Please login first!")
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
                (redirect-to rc "/login"))) ; auth failed, relogin!
           (else ; invalid auth request
            (redirect-to rc "/login"))))))

(define (get-all-articles)
  (query blog-db "select * from article")
  (fold (lambda (x p)
          (let ((title (assoc-ref x "title"))
                (content (assoc-ref x "content")))
            (cons `(div (@ (class "post")) (h2 ,title) (h3 ,title) (p ,content)) p)))
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
              (content (params rc "content")))
          (query blog-db 
                 (format #f 
                         "insert into article (title,content) values (~s,~s)"
                         title content))
          (redirect-to rc "/"))))
                                         
(run)
