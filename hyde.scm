(use hyde lowdown colorize hyde-atom regex)
(output-dir "")
(clean-before-build #f)
(translators (cons (list "md" markdown->html) (translators)))
(require-library regex)
(import irregex)

(default-page-vars '(((: bos "" (+ any) ".wiki")
                      (layouts "default.sxml"))))

(link-shortcuts '((docs . "http://web-artanis.com/docs/~A")))

(define description-contents "The light-weight and fastest web-framework of Scheme language")
(define keyword-contents "guile tutorial,scheme tutorial,guile scheme,guile,web,web app,framework,scheme language,scheme")
(define $ (environment-ref (page-eval-env) '$))

(define (page-updated page)
  (or ($ 'updated page) ($ 'date page)))

(define (sort-by pages accessor)
  (sort pages (lambda (p1 p2) (> (accessor p1) (accessor p2)))))

(define (pages-matching regex)
  (map cdr (filter (lambda (p) (irregex-match regex (car p)))
		   ((environment-ref (page-eval-env) 'pages)))))

(define (format-seconds seconds)
  (time->string (seconds->utc-time seconds) "%Y-%m-%d %z"))

(define (authors->sxml authors)
  `(,(car authors)
    ,@(if (null? (cdr authors)) 
          '()
          (map (lambda (author)
                 `(,(car author) (span (@ (class "author")) ,(cdr author))))
               (append (map (cut cons ", " <>) (butlast (cdr authors)))
                       `((" and " . ,(last authors))))))))

(for-each (lambda (binding)
	    (apply environment-extend! (cons (page-eval-env) binding)))
	  `((page-updated ,page-updated)
	    (format-seconds ,format-seconds)
            (authors->sxml ,authors->sxml)
	    (all-issues ,(lambda () 
			   (sort-by (pages-matching '(: "" (+ any) ".wiki")) page-updated)))))
