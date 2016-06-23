(get "/hello/:name"
  (lambda (rc)
    (format #f "hello ~a ~%"
            (params rc "name"))))

;; curl example.com/hello/mulei
;; ==> hello mulei
