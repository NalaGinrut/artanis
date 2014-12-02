#!/usr/bin/env guile
!#

(use-modules (artanis artanis))

;; the example of multi files upload
(init-server)

(define upload-form
  '(form (@ (method "POST") (enctype "multipart/form-data") (action "upload"))
         "File to upload: " (input (@ (type "file") (name "upfile"))) (br)
         "Notes about the file: " (input (@ (type "text") (name "note")))
         (br) (br)
         (input (@ (type "submit") (value "Press")) "to upload the file!")))

(get "/upload" (lambda () (tpl->response upload-form)))

(post "/upload"
      (lambda (rc)
        (case (store-uploaded-files rc)
          ((success) (response-emit "upload succeeded!"))
          ((none) (response-emit "No uploaded files!"))
          (else (response-emit "Impossible! please report bug!")))))

(run)

