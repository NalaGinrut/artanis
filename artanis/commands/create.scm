;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015-2025
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
;;  Artanis is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License and GNU
;;  Lesser General Public License published by the Free Software
;;  Foundation, either version 3 of the License, or (at your option)
;;  any later version.

;;  Artanis is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License and GNU Lesser General Public License
;;  for more details.

;;  You should have received a copy of the GNU General Public License
;;  and GNU Lesser General Public License along with this program.
;;  If not, see <http://www.gnu.org/licenses/>.

(define-module (artanis commands create)
  #:use-module (artanis version)
  #:use-module (artanis config)
  #:use-module (artanis utils)
  #:use-module (artanis env)
  #:use-module (artanis commands)
  #:use-module (artanis irregex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match))

(define %summary "Create a new Artanis project.")
(define *current-conf-file* (gen-local-conf-file))

(define (show-help)
  (display announce-head)
  (display "\nUsage:\n  art create proj-path\n")
  (display "\n[Special]
  art create --upgrade
  * Upgade webapp to current Artanis and keep existing configs.\n")
  (display announce-foot))

(define (conf-header)
  (format #f
          "##  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
##  Copyright (C) ~a
##      \"Mu Lei\" known as \"NalaGinrut\" <mulei@gnu.org>
##  Artanis is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.

##  Artanis is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.

##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <http://www.gnu.org/licenses/>.

## ---------------------------------------------------------------------
## The skeleton of config file, you may modify it on your own purpose.
## DON'T TYPE `;' AT THE END OF LINE!!!
## ---------------------------------------------------------------------

## Please read the manual or /etc/artanis/default.conf if you have problem
## to understand these items.

"
          (strftime "%Y" (localtime (current-time)))))

(define conf-footer "\n\n## End Of Artanis conf.\n")

(define *plugin-conf*
  ";; This is a plugin configuration file, don't remove it!

;; Uncomment following lines to enable plugins.
;; Make sure you import the plugin modules here.
(define-module (conf plugins)
  #:use-module (artanis oht))

;; (plugin-enable! name handler)
")

(define (touch f)
  (close (open-file f "w")))

;; ENHANCEME: make some color
(define (print-create-info pstr)
  (format #t "create~10t~a~%" pstr))

(define (create-default-readme readme)
  (print-create-info readme)
  (touch readme))

(define (benchmark-handler p)
  (define (-> f) (string-append p "/" f))
  (create-default-readme (-> "README"))
  ;; TODO: generate template
  )

(define (plugins-handler p)
  (define (-> f) (string-append p "/" f))
  (when (not (file-exists? (-> "plugins.scm")))
    (let ((fp (open-file (-> "plugins.scm") "w")))
      (display *plugin-conf* fp)
      (close fp)
      (print-create-info (-> "plugins.scm")))))

(define (conf-handler p)
  (define (-> f) (string-append p "/" f))
  (define (create-local-config)
    (define (->proper v)
      (match v
        ((or #t 'true 'on 'yes) 'true)
        ((or #f 'false 'off 'no) 'false)
        ((? list?) (format #f "~{~a~^,~}" v))
        (else v)))
    (define (read-config-val k val)
      (let ((usr-val (get-conf k)))
        (if (or (not (file-exists? *current-conf-file*))
                (not usr-val))
            val
            usr-val)))
    (define (->cstr ctb)
      (define (->comments str)
        (call-with-input-string
         str
         (lambda (port)
           (let lp ((rst-string "")
                    (line (read-line port)))
             (if (eof-object? line)
                 rst-string
                 (lp (string-append rst-string "## " line "\n") (read-line port)))))))
      (call-with-output-string
       (lambda (port)
         (for-each (lambda (c)
                     (match c
                       (('(server info) _ comments)
                        (format port "~%~aserver.info = ~a~%"
                                (->comments comments) artanis-version))
                       ((ns val comments)
                        (format port "~%~a~{~a~^.~} = ~a~%" (->comments comments) ns (->proper (read-config-val ns val))))
                       (else (error create-local-config "BUG: Invalid conf value!" c))))
                   ctb))))
    (let* ((ctb (default-conf-values))
           (cstr (->cstr ctb))
           (fp (open-file (-> "artanis.conf") "w")))
      (display (conf-header) fp)
      (display cstr fp)
      (display conf-footer fp)
      (close fp)
      (print-create-info (-> "artanis.conf"))))
  (when (not (file-exists? p))
    (create-default-readme (-> "README")))
  (create-local-config)
  (plugins-handler p))

(define (sm-handler p)
  (define (-> f) (string-append p "/" f))
  (create-default-readme (-> "README"))
  ;; TODO: generate template
  )

(define *files-handler*
  `(((sm) . ,sm-handler)
    ((conf) . ,conf-handler)
    ((test benchmark) . ,benchmark-handler)))

(define *dir-arch*
  '((app (models controllers views protocols)) ; MVC stuff
    (conf) ; config files
    (sys (pages (i18n (json po sxml)))) ; system stuff
    (db (migration sm)) ; DB (include SQL Mappings)
    (log) ; log files
    (lib) ; libs
    (pub ((img (upload)) css js)) ; public assets
    (prv) ; private stuff, say, private config or tokens
    (test (unit functional benchmark)))) ; tests stuffs

;; Simple recursive depth-first order traverser for generic tree (in list).
;; We use this function for making *dir-arch* directory tree, the performance here
;; is trivial.
(define (dfs t p l)
  (match t
    (() #t)
    (((r (children ...)) rest ...)
     (p r l)
     (for-each (lambda (x) (dfs (list x) p (cons r l))) children)
     (dfs rest p l))
    (((r) rest ...)
     (p r l)
     (dfs rest p l))
    ((children ...)
     (p (car children) l)
     (dfs (cdr children) p l))
    (else (error dfs "BUG: Impossible pattern! Please report it!" t))))

(define (create-framework)
  (define (->path p)
    (format #f "~{~a~^/~}" p))
  (define (generate-elements x l)
    (let* ((p (reverse (cons x l)))
           (pstr (->path p)))
      (when (not (file-exists? pstr))
        (mkdir pstr) ; generate path

        (touch (format #f "~a/.gitkeep" pstr))
        (print-create-info pstr)
        (and=> (assoc-ref *files-handler* p)
               (lambda (h) (h pstr))))))
  (dfs *dir-arch* generate-elements '()))

(define *entry-string*
  "
 (use-modules (artanis artanis)
              ;; Put modules you want to be imported here
              ;; only for this file, not controllers/views

              (artanis utils))
 ;; Put whatever you want to be called before server initilization here

 (init-server #:statics '(png gif jpg jpeg ico html js json csv xml css woff woff2 ttf))
 (add-to-load-path (string-append (current-toplevel) \"/lib\"))
 ;; Put whatever you want to be called before server running here
")

(define (create-entry name)
  (let ((fp (open-file "ENTRY" "w")))
    (format fp ";; This an Artanis ENTRY file, don't remove it!~%")
    (display *entry-string* fp)
    (close fp)))

(define (working-for-toplevel)
  (define (gen-readme)
    (touch "README")
    (print-create-info "README"))

  (gen-readme)
  ;; TODO
  )
;;Upgrade config from old version to the new one with comments
(define (upgrade-config)
  (current-conf-file *current-conf-file*)
  (current-conf-file)
  (init-config)
  (conf-handler "conf"))

(define (create-project name)
  (define (within-another-app?)
    (let ((entry (current-toplevel)))
      (and entry (verify-ENTRY entry))))
  (cond
   ((file-exists? name)
    (format #t
            "`~a' exists, please choose a new name or remove the existed one!~%"
            name))
   ((within-another-app?)
    (display "Can't create a new Artanis app within the directory of another, ")
    (display "please change to a non-Artanis directory first.\n")
    (exit 1))
   (else
    (print-create-info name)
    (mkdir name)
    (chdir name)
    (working-for-toplevel)
    (create-entry name)
    (create-framework)
    (format #t "OK~%"))))

(define (upgrade-project)
  (upgrade-config)
  (create-framework))

(define (create . args)
  (define (validname? x)
    (irregex-search "^-.*" x))
  (match args
    (("create" "--upgrade") (upgrade-project))
    (("create" "--options-list") (display "--upgrade --help\n"))
    (("create" (or () (? validname?) "help" "--help" "-help" "-h")) (show-help))
    (("create" name) (create-project name))
    (else (show-help))))

(define main create)
