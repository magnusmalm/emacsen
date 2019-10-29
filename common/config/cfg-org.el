;; Hack to make loading latest org mode work.
;; org-git-version, org-release
(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
		   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
	      "--match=release\*"
	      "--abbrev=6"
	      "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
		   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
	       "--match=release\*"
	       "--abbrev=0"
	       "HEAD")))))

(provide 'org-version)

(use-package org
  ;; :straight nil
  ;; :type built-in
  ;; :demand nil
  ;; :ensure nil
  :init
  (defun load-org-agenda-files-recursively (dir) "Find all directories in DIR."
	 (unless (file-directory-p dir)
	   (error "Not a directory `%s'" dir))
	 (unless (equal (directory-files dir nil org-agenda-file-regexp t) nil)
	   (add-to-list 'org-agenda-files dir))
	 (dolist (file (directory-files dir nil nil t))
	   (unless (member file '("." ".."))
	     (let ((file (concat dir file "/")))
	       (when (file-directory-p file)
		 (load-org-agenda-files-recursively file))))))
  ;; :config
  (setq org-agenda-files '("~/sync/org/work.org"
			   "~/sync/org/private.org"
			   "~/sync/org/solkatten.org"
			   ))

  (setf org-log-done (quote time))
  (setf org-log-redeadline (quote time))
  (setf org-log-reschedule (quote time))

  (setf org-pretty-entities t)
  (setf org-use-sub-superscripts '{})

  (setf org-log-into-drawer t)
  (setf org-use-speed-commands t
	org-hide-emphasis-markers t
	org-src-fontify-natively t ;; Pretty code blocks
	org-src-tab-acts-natively t
	org-confirm-babel-evaluate nil)
  (setf org-src-fontify-natively t)
  (setf org-src-tab-acts-natively t)
  (setf org-return-follows-link t)

  (setf org-ellipsis "⤵")
  (setf org-todo-keywords
	'((sequence "TODO(t)" "IN-PROGRESS(i)" "WAIT(w@/!)" "|"
		    "DONE(d!)" "CANCELED(c@)")))

  (setf org-todo-keyword-faces
	'(("TODO" . "LightSkyBlue")
	  ("IN-PROGRESS" . "yellow2")
	  ("WAIT" . "IndianRed")
	  ("DONE" . "gold")
	  ("CANCELED" . "red")))

  ;; (load-org-agenda-files-recursively "~/sync/org/") ; trailing slash required
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (emacs-lisp . t)
     (lisp . t)
     (python . t)
     (shell . t)
     (ditaa . t)
     (C . t)
     ))
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))

  ;;  set to nil to use symmetric encryption.
  (setq org-crypt-key nil)

  ;; Global Tags
  (setf org-tag-alist '((:startgroup . nil)
			("@work" . ?w)
			("@home" . ?h)
			("@computer" . ?l)
			("@mobile" . ?p)
			(:endgroup . nil)
			("ttdp" . ?t)
			("config" . ?c)
			("emacs" . ?E)
			("org" . ?o)
			("meeting" . ?M)
			("household" . ?H)
			("economy" . ?e)
			("crypt" . ?y)))
  (setf org-refile-targets '((nil :maxlevel . 2)
  			     (org-agenda-files :maxlevel . 2)))

  (setf org-tag-faces
	'(("@home"
	   :foreground "Green3"
	   :background nil
	   :weight bold)
	  ("@work"
	   :foreground "DeepSkyBlue"
	   :background nil
	   :weight bold)
	  ("@computer"
	   :foreground "LightSeaGreen"
	   :background nil
	   :weight bold)
	  ("@mobile"
	   :foreground "Orange"
	   :background nil
	   :weight bold)))

  )

(defun mmm/org-capture-mode-hook ()
  (bind-keys :map org-capture-mode-map
    ("C-d" . insert-current-time)
    ("M->" . org-priority-up)
    ("M-<" . org-priority-down)))
(add-hook 'org-capture-mode-hook 'mmm/org-capture-mode-hook)

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (setf org-bullets-bullet-list '("◉" "○")))

(setf org-capture-templates
      '(
	("m" "meeting" entry (file+headline "~/sync/org/inbox.org" "meetings")
	 "* TODO %? :meeting:\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n"
	 :prepend t)
	("T" "todo" entry (file+headline "~/sync/org/inbox.org" "todos")
	 "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"
	 :prepend t)
	("j" "Journal entry" plain
	 (file+olp+datetree "~/sync/org/journal/journal.org")
	 "%i\n\n**** %?\n" :empty-lines 1)
	("t" "TODO" entry
	 (file+headline "~/sync/org/inbox.org" "todos")
	 "* TODO %?\n%u" :prepend t)
	("n" "Note" entry
	 (file+headline "~/sync/org/inbox.org" "notes")
	 "* %?\n%u" :prepend t)
	)
      )


(use-package oauth2)

(use-package org-noter)


(use-package ox-publish
  :straight nil
  :init
  (setq my-blog-header-file "~/Projects/blog/org/partials/header.html"
        my-blog-footer-file "~/Projects/blog/org/partials/footer.html"
        org-html-validation-link nil)

  ;; Load partials on memory
  (defun my-blog-header (arg)
    (with-temp-buffer
      (insert-file-contents my-blog-header-file)
      (buffer-string)))

  (defun my-blog-footer (arg)
    (with-temp-buffer
      (insert-file-contents my-blog-footer-file)
      (buffer-string)))

  (defun filter-local-links (link backend info)
    "Filter that converts all the /index.html links to /"
    (if (org-export-derived-backend-p backend 'html)
        (replace-regexp-in-string "/index.html" "/" link)))

  :config
  (setq org-publish-project-alist
        '(;; Publish the posts
          ("blog-notes"
           :base-directory "~/Projects/blog/org"
           :base-extension "org"
           :publishing-directory "~/Projects/blog/public"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :section-numbers nil
           :html-head nil
           :html-head-include-default-style nil
           :html-head-include-scripts nil
           :html-preamble my-blog-header
           :html-postamble my-blog-footer
           )

          ;; For static files that should remain untouched
          ("blog-static"
           :base-directory "~/Projects/blog/org/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|eot\\|svg\\|woff\\|woff2\\|ttf"
           :publishing-directory "~/Projects/blog/public"
           :recursive t
           :publishing-function org-publish-attachment
           )

          ;; Combine the two previous components in a single one
          ("blog" :components ("blog-notes" "blog-static"))))

  (add-to-list 'org-export-filter-link-functions 'filter-local-links))




(use-package ox-html5slide
  :init
  (setf org-html-postamble nil)
  (setf org-export-with-section-numbers nil)
  (setf org-export-with-toc nil)
  (setf org-html-head-extra "
       <link href='http://fonts.googleapis.com/css?family=Source+Sans+Pro:400,700,400italic,700italic&subset=latin,latin-ext' rel='stylesheet' type='text/css'>
       <link href='http://fonts.googleapis.com/css?family=Source+Code+Pro:400,700' rel='stylesheet' type='text/css'>
       <style type='text/css'>
          body {
             font-family: 'Source Sans Pro', sans-serif;
          }
          pre, code {
             font-family: 'Source Code Pro', monospace;
          }
       </style>"))

(use-package ox-reveal
  :init
  (setf org-reveal-postamble "Magnus Malm")
  (setf org-reveal-root "file:///home/magnus/src/reveal.js"))

(use-package poporg)

(use-package org-habit
  :straight nil
  :ensure nil)

(setf org-modules '(org-habit))

(use-package org-super-agenda
  :init
  (setf org-super-agenda-groups
	'((:log t)  ; Automatically named "Log"
	  (:name "Schedule"
		 :time-grid t)
	  (:name "Today"
		 :scheduled today)
	  (:habit t)
	  (:name "Due today"
		 :deadline today)
	  (:name "Overdue"
		 :deadline past)
	  (:name "Due soon"
		 :deadline future)
	  (:name "Unimportant"
		 :todo ("SOMEDAY" "MAYBE" "CHECK" "TO-READ" "TO-WATCH")
		 :order 100)
	  (:name "Waiting..."
		 :todo "WAIT"
		 :order 98)
	  (:name "Scheduled earlier"
		 :scheduled past)))
  :config
  (org-super-agenda-mode 1))

(use-package org-sidebar)
