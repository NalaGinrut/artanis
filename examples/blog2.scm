#! /usr/local/bin/guile \
-L ../
!#

;; This is a very simple blog example for artanis

(use-modules (artanis artanis) (artanis session) (artanis utils) (artanis db)
             (oop goops) (srfi srfi-1) (artanis cookie))

(init-server) ;; make sure call init-server at beginning

(define blog-db (make <mysql> #:user "root" #:name "mmr_blog"))
(conn blog-db "") ; "123" is the passwd of database

(define (make-footer)
  (tpl->html
   `(div (@ (id "footer"))
         (p "Colt blog-engine based on " 
            (a (@ (href "https://github.com/NalaGinrut/artanis")) "Artanis")
            "."))))

(define blog-title "Colt blog-engine")
(define footer (make-footer))

(define* (cookie-auth? rc #:key (key "sid"))
  (let* ((ck (request-cookies (rc-req rc)))
         (c (and ck (cookie-has-key? ck key))))
    (format #t "c: ~a~%" c)
    c))

(define* (login-auth? rc #:key (key "sid"))
  (let ((sid (params rc key)))
    (format #t "sid: ~a~%" sid)
    (and sid (get-session sid))))

(define (show-admin-page)
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

(get "/admin"
  (lambda (rc)
    (cond
     ((login-auth? rc)
      (display "login auth!\n")
      (show-admin-page))
     ((cookie-auth? rc)
      (display "cookie auth!\n")
      (show-admin-page))
     (else (redirect-to rc "/login")))))

(get "/login"
     (lambda (rc)
       (let ((failed (params rc "login_failed")))
         (tpl->response "login.tpl" (the-environment)))))

(post "/auth"
      (lambda (rc)
        (let ((user (params rc "user"))
              (pwd (params rc "passwd"))
              (keep (params rc "remember_me")))
          (cond
           ((and user pwd)
            (query blog-db (format #f "select * from user where user=~s" user))
            (let ((line (get-one-row blog-db)))
              (cond
               ((not line) (redirect-to rc "/login?login_failed=true"))
               ((string=? pwd (assoc-ref line "passwd"))
                (call-with-values
                    (lambda () (session-spawn rc))
                  (lambda (sid session)
                    (let ((cookie (and keep (new-cookie #:npv `(("sid" ,sid))))))
                      (and cookie (rc-set-cookie! rc (list cookie)))
                      (redirect-to rc (format #f "/admin?sid=~a" sid)))))) ; auth OK
               (else (redirect-to rc "/login?login_failed=true"))))) ; auth failed, relogin!
           (else ; invalid auth request
            (redirect-to rc "/login"))))))

(define (get-all-articles)
  (query blog-db "select * from article")
  (fold (lambda (x prev)
          (let ((title (uri-decode (assoc-ref x "title")))
                (content (uri-decode (assoc-ref x "content")))
                (date (assoc-ref x "date")))
            (cons `(div (@ (class "post")) (h2 ,title) 
                        (p (@ (class "post-date")) ,date) 
                        (p ,content))
                        ;;(div (@ (class "post-meta")) ,meta)
                  prev)))
        '() (get-all-rows blog-db)))

(get "/search$"
     (lambda (rc)
       (response-emit "waiting, it's underconstruction!")))

(get "/$"
     (lambda (rc)
       (let ((all-posts (tpl->html (get-all-articles))))
         (tpl->response "index.tpl" (the-environment)))))

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
