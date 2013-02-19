Artanis
=========

Artanis aims to be a very lightweight web framwork for Scheme which is written all with GNU Guile.

## Features:

* very lightweight: the core artanis.scm almost 300 lines, easy to hack
and learn for newbies.
* a relative complete web-server implementation, include error page
throw and all the HTTP method(you have to specify your own handler)
* 10K concurrent performance for the server, takes advantage of the
Guile inner server. It's enough for you own site/blog.
* sinatra like style route, that's why it names "artanis" ;-)
* Database support(now use guile-dbi), mysql/sqlite/postgresql. But it's
easy to port to other database binding. (but I like dbi)
* session support
* HTML template of SXML (very easy to use for Lisper)

## INSTALL:
You need guile-dbi to handle database:
http://download.gna.org/guile-dbi/

And you need specified dbd to control the database, you have three choices:
* guile-dbd-mysql
* guile-dbd-postgresql
* guile-dbd-sqlite3

All the database operations are in (artanis db).

NOTE: For our example/blog.scm, you need guile-dbd-mysql

All the packages above is easy to install:

1. untar the package
2. ./configure
3. make
4. sudo make install

Anyway, you don't have to install Artanis, you may just write your website under
the artanis' toplevel directory. But if you like to install it, just copy/link artanis
to somewhere like '/usr/local/share/guile/2.0' where guile was installed.

## example:

It's very easy to use:

```scheme
(init-server) ; make sure alway put it in the main script head.
(define my-var #f) ; a global var for later use

;; get means GET method in HTTP protocol, and you may use:
;; get/post/put/patch/delete
(get "/show-me" ; define a page which path named "show-me"
  (lambda (rc)  ; rc is route-context, you may do everything with it!
    (response-emit "OK, this is a hello world page!"))) 
;; any page must return with response-emit
   
(get "/show/:id" ; ":id" is a general key to indicate something you passed in
  (lambda (rc)
    (let ((id (params rc "id"))) ; use 'params' to get any key, include query-string
      (response-emit (format #f "the id is ~a" id)))))
;; now you may try "localhost:3000/show/123" in your fav browser, and it'll show
;; "the id is 123" in the page!

;; Why always 'get'? But 'get' is the most useful method of HTTP, anyway, you
;; may try others, like 'post':
(post "/edit/:name"
  (lambda (rc)
    (let ((name (params rc "name"))) ; ":name" means the key to specify is "name"
      (set! my-var name)
      (response-emit "edit name successfully!"))))

;; and just try to show the result you modified
(get "/show/name"
  (lambda (rc)
    (response-emit (format #f "the name is ~a" my-var))))

(run) ; the last line, you must run the inner server
```

Enjoy.

Happy hacking!

