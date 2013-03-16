Artanis
=========

Artanis aims to be a very lightweight web framework for Scheme which is written all with GNU Guile.

## Features:

* very lightweight: the core artanis.scm almost 300 lines, easy to hack
and learn for newbies.
* a relative complete web-server implementation, include error page
throw and all the HTTP method (you have to specify your own handler)
* 10K concurrent performance for the server, takes advantage of the
Guile inner server. It's enough for you own site/blog.
* sinatra like style route, that's why it names "artanis" ;-)
* Database support (now use guile-dbi), mysql/sqlite/postgresql. But it's
easy to port to other database binding. (but I like dbi)
* session support
* HTML template of SXML (very easy to use for Lisper)

## INSTALL:
First, you need Guile-2.x:
http://www.gnu.org/software/guile/
Or just install it with your apt-get/zypper/yum...
I suggest you install Guile-2.0.7

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

## Inner web server
The default port is 3000, so you have to fetch the URL like:
http://localhost:3000/...

But you may specify it like this:
``` scheme
(run #:port 1234)
```

## APIs docs
``` scheme

;; 0. the most useful tool, is 'param':


;; 1. HTTP method handler register
;;    Here are 6 methods: get post put patch delete params header
;;    All of them have same usage, depends on your need.
;; Usage:
(get rule     ; rule is a string indicates the path, support regexp
     handler) ; handler is a function accepts one argument
;; E.g:
(get "/hello$" ; it's a regexp means to parse the path "/hello", 
               ; if you're familiar with regexp, "$" means no other thing follows.
     (lambda (rc) ; argument 'rc' is means route-context
       (response-emit "hello world!")))

;; Now you registered a handler with rule "/hello$" for GET method,
;; which means, each time you visit "localhost:3000/hello" from you browser,
;; the request hits the rule "/hello$", then call the handler you registered.
;; As you guess, the handler just show "hello world!" on your browser.

;; 1.1 the most useful tool, is 'param'
;; Usage:
(param route-context key)
;; E.g:
(get "/send/:id1/to/:id2"
     (lambda (rc)
       (let ((name1 (param rc "id1"))
             (name2 (param rc "id2")))
         (response-emit (format #f "send ~s to ~s" name1 name2)))))
;; And you may visit "localhost:3000/send/AAA/to/BBB", there'll be 
;; "send AAA to BBB" in your browser.

;; 'param' could get the value of the key you speicified in the rules.
;; As you see, key just like this ":keyname", and you could use 'param' to get it.
;; Besides, even the query-string key, "/send?name1=AAA&name2=BBB", you may try param.     

;; 2. response-emit
;;    This function is the standard way to return your response to the client.
;; Usage:
(response-emit HTTP-body #:status #:header #:mtime)
;; E.g:
(response-emit "hello world" #:status 200) ; return just a string to client.
;; But you don't have to pass status if it's OK:
(response-emit "hello world")
;; If you just pass HTTP-body in, the HTTP-status will automatically assigned 200 which means OK.
(response-emit "" #:status 404) ; this is the standard way you response a missing page (404).
(response-emit "test" #:header '((etag . "asdf"))) ; you may costum the HTTP header as you wish.
;; Actually you don't have to specify mtime(last modify time) by your self in general.
;; But if you really need:
(response-emit "test" #:mtime (cons seconds nanoseconds))
;; Don't use the common "current-time", since it's only for seconds
;; You may find srfi-19 is useful for this.

;; 3. tpl->html
;;    The HTML template.
;; Usage:
(tpl->html s-expr)
;; E.g:
(let ((a 1) (b 2) (c 3))
  (tpl->html `((font (@ (color "red")) ,a)
               (font (@ (color "blue")) ,b)
               (font (@ (color "green")) ,c))))
;; You will get the HTML string:
"<font color=\"red\">1</font><font color=\"blue\">2</font><font color=\"green\">3</font>"
;; In general, you could use response-emit:
(response-emit (tpl->html ...))
;; It's cool, try it!

;; 4. redirect-to
;;    Redirect the URL
;; Usage:
(redirect-to rc URL)
;; E.g:
(get "/path1"
     (lambda (rc)
       (redirect-to rc "/path2")))

;; 5. emit-response-with-file
;;    Send static page or binary files to client
;; Usage:
(emit-response-with-file filename)
;; E.g:
(get "/$"
     (lambda (rc)
       (emit-response-with-file "index.html")))

;; 6. generate-response-with-file
;;    Generate response by the file you specified, and you need to specify the handler
;;    emit-response-with-file is implemented with generate-response-with-file
;;    NOTE: This function is low-level, emit-response-with-file is more common.
;; Usage:
(generate-response-with-file filename proc) ; proc need the file-port of filename as the argument
;; The proc will return four values, so you need call-with-values or receiver
;; E.g:
(define (emit-response-with-file filename)
  (call-with-values
      (lambda ()
        (generate-response-with-file filename (lambda (p) (bv-cat p #f))))
    (lambda (mtime status bv mime)
      (cond
       ((= status 200) 
        (response-emit bv #:status status #:headers `((content-type . (mime)))
                       #:mtime mtime))
       (else (response-emit bv #:status status))))))

;; 7. init-server
;;    Init web server
;; Usage:
(init-server)
;; NOTE: You must call (init-server) before any rules you registered.

;; 8. run
;;    Run web server
;; Usage:
(init-server #:port) ; #:port is optional, 3000 in default.
;; E.g:
(init-server #:port 1234) ; now you need to visit "localhost:1234/"
```

Enjoy.

Happy hacking!

