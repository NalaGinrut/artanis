(use-modules (artanis artanis) (artanis tpl) (artanis upload))

;; the example of multi files upload

(init-server)

(define upload-form
  '(form (@ (method "POST") (enctype "multipart/form-data") (action "upload"))
         "File to upload: " (input (@ (type "file") (name "upfile"))) (br)
         "Notes about the file: " (input (@ (type "text") (name "note")))
         (br) (br)
         (input (@ (type "submit") (value "Press")) "to upload the file!")))

(get "/upload"
     (lambda (rc)
       (response-emit
        (tpl->html upload-form))))

(post "/upload"
      (lambda (rc)
        (cond
         ((content-type-is-mfd? rc)
          => (lambda (boundry)
               (let ((mfds (parse-mfd-body boundry (rc-body rc))))
                 (mfd-simple-dump-all mfds)
                 (response-emit "upload succeeded!"))))
         (else (response-emit "No uploaded files!")))))

(run)

