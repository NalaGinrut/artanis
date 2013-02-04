#! /usr/local/bin/guile \
-L ./
!#


(use-modules (artanis artanis))

(setlocale LC_ALL "")
(get "/mmr/:id1/to/:id2"
  (lambda (rc)
    (let ((id1 (params rc 'id1))
          (id2 (params rc 'id2)))
      (response-emit (format #f "Send from ~a to ~a~%" id1 id2)))))

(run)
