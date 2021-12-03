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
	     :ensure org-plus-contrib
	     :init
	     (defun my-org-mode-hook-fn ()
	       ;; (unbind-key "M-h")
	       ;; (unbind-key "M-H")
	       (bind-key "M-h" 'org-beginning-of-line 'org-mode-map)
	       (bind-key "M-H" 'org-end-of-line 'org-mode-map))

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
	     :hook (org-mode . my-org-mode-hook-fn)
	     :bind (("C-c M-l" . org-store-link)
		    ("C-c C-l" . org-insert-link)
		    (:map org-mode-map
			  ("M-<return>" . org-meta-return)
			  ;; ("M-<return>" . org-insert-heading-respect-content)
			  ("C-c a" . org-agenda)
			  ("C-c c" . org-capture)
			  ("<pause>" . org-capture)
			  ("C-c C-e" . org-export-dispatch)))
	     :config
	     (require 'org-tempo)
	     (setq org-agenda-files '("~/sync/org/work.org"
				      "~/sync/org/private.org"
				      "~/sync/org/solkatten.org"
				      ))

	     (setf org-log-done (quote time))
	     (setf org-log-redeadline (quote time))
	     (setf org-log-reschedule (quote time))

	     (setf org-pretty-entities t)
	     (setf org-use-sub-superscripts '{})

	     (setf org-startup-folded 'overview)
	     
	     (setf org-log-into-drawer t)
	     (setf org-special-ctrl-a/e t)
	     (setf org-use-speed-commands t
		   org-hide-emphasis-markers t
		   org-src-fontify-natively t ;; Pretty code blocks
		   org-src-tab-acts-natively t
		   org-confirm-babel-evaluate nil)
	     (setf org-src-fontify-natively t)
	     (setf org-src-tab-acts-natively t)
	     (setf org-return-follows-link t)
	     (setf org-reverse-note-order t)

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
		(restclient . t)
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

	     (setf org-loop-over-headlines-in-active-region t)
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
	     (setq org-bullets-bullet-list (quote ("◉" "✿"))))
;; (setf org-bullets-bullet-list '("◉" "○")))

(setq inhibit-compacting-font-caches t)

(setf org-capture-templates
      '(
	("w" "work")

	("wt" "work todo" entry
	 (file+headline "~/sync/org/work.org" "todos")
	 "* TODO %?\n"
	 :prepend t)


	("wj" "Journal entry" plain
	 (file+olp+datetree "~/sync/org/journal-work.org")
	 "%i\n**** %?\n"
	 :empty-lines 0)

	("wm" "meeting" entry (file+headline "~/sync/org/work.org" "meetings")
	 "* TODO %? :meeting:\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n"
	 :prepend t)

	("wn" "Note" entry
	 (file+headline "~/sync/org/work.org" "notes")
	 "* %?\n%u"
	 :prepend t)

	("p" "private")

	("pt" "work todo" entry
	 (file+headline "~/sync/org/private.org" "todos")
	 "* TODO %?\n"
	 :prepend t)


	("pj" "Journal entry" plain
	 (file+olp+datetree "~/sync/org/journal-private.org")
	 "%i\n\n**** %?\n"
	 :empty-lines 1)

	("pm" "meeting" entry (file+headline "~/sync/org/private.org" "meetings")
	 "* TODO %? :meeting:\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n"
	 :prepend t)

	("pn" "Note" entry
	 (file+headline "~/sync/org/private.org" "notes")
	 "* %?\n%u"
	 :prepend t)

	("s" "solkatten")

	("st" "work todo" entry
	 (file+headline "~/sync/org/solkatten.org" "todos")
	 "* TODO %?\n"
	 :prepend t)


	("sj" "Journal entry" plain
	 (file+olp+datetree "~/sync/org/journal-solkatten.org")
	 "%i\n\n**** %?\n"
	 :empty-lines 1)

	("sm" "meeting" entry (file+headline "~/sync/org/solkatten.org" "meetings")
	 "* TODO %? :meeting:\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n"
	 :prepend t)

	("sn" "Note" entry
	 (file+headline "~/sync/org/solkatten.org" "notes")
	 "* %?\n%u"
	 :prepend t)

	)
      )


(use-package oauth2)

(use-package org-noter)

;; (use-package org-static-blog
;;   :config
;;   (setq org-static-blog-publish-title "Magnus Malm's Mumblings")
;;   (setq org-static-blog-publish-url "https://blog.mware.se/")
;;   (setq org-static-blog-publish-directory "~/blog/public/")
;;   (setq org-static-blog-posts-directory "~/blog/posts/")
;;   (setq org-static-blog-drafts-directory "~/blog/drafts/")
;;   (setq org-static-blog-enable-tags t)
;;   (setq org-export-with-toc nil)
;;   (setq org-export-with-section-numbers nil)

;;   ;; This header is inserted into the <head> section of every page:
;;   ;;   (you will need to create the style sheet at
;;   ;;    ~/projects/blog/static/style.css
;;   ;;    and the favicon at
;;   ;;    ~/projects/blog/static/favicon.ico)
;;   (setq org-static-blog-page-header
;; 	"<meta name=\"author\" content=\"Magnus Malm\">
;; <meta name=\"referrer\" content=\"no-referrer\">
;; <link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
;; <link rel=\"icon\" href=\"static/favicon.ico\">")

;;   ;; This preamble is inserted at the beginning of the <body> of every page:
;;   ;;   This particular HTML creates a <div> with a simple linked headline
;;   (setq org-static-blog-page-preamble
;; 	"<div class=\"header\">
;;   <a href=\"https://blog.mware.se\">Magnus Malm's Mumblings</a>
;;   <div class=\"sitelinks\">
;;     <a href=\"https://twitter.com/malm_magnus\">Twitter</a> | <a href=\"https://github.com/magnusmalm\">Github</a>
;;   </div>
;; </div>")

;;   ;; This postamble is inserted at the end of the <body> of every page:
;;   ;;   (setq org-static-blog-page-postamble
;;   ;; 	"<div id=\"archive\">
;;   ;;   <a href=\"https://blog.mware.se/archive.html\">Other posts</a>
;;   ;; </div>"))

;;   (setq org-static-blog-page-postamble
;; 	""))

(use-package htmlize)

(use-package ox-publish
	     :straight nil
	     :init
	     (setq my-blog-header-file "~/blog/partials/header.html"
		   my-blog-footer-file "~/blog/partials/footer.html"
		   org-html-validation-link nil)

	     (setq my-site-extra-head "<link rel='stylesheet' href='/static/main.css' />")
	     
	     ;; Load partials on memory
	     (defun my-blog-header (arg)
	       (with-temp-buffer
		 (insert-file-contents my-blog-header-file)
		 (buffer-string)))

	     (defun my-blog-footer (arg)
	       (with-temp-buffer
		 ;; (insert-file-contents my-blog-footer-file)
		 (format "<p>%s</p>" (format-time-string "%Y-%m-%d"))
		 (buffer-string)))

	     (defun filter-local-links (link backend info)
	       "Filter that converts all the /index.html links to /"
	       (if (org-export-derived-backend-p backend 'html)
		   (replace-regexp-in-string "/index.html" "/" link)))

	     (defun add-html-file (arg)
	       (with-temp-buffer
		 (insert-file-contents arg)
		 (buffer-string)))

	     (defun my-site-format-entry (entry style project)
	       (format "[[file:%s][%s]] --- %s"
		       entry
		       (org-publish-find-title entry project)
		       (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))))

	     (defun my-website-sitemap-function (project &optional sitemap-filename)
	       "Custom sitemap generator that inserts additional options."
	       (let ((buffer (org-publish-sitemap project sitemap-filename)))
		 (with-current-buffer buffer
		   (insert "\n#+OPTIONS: html-preamble:nil")
		   (insert "\n#+SUBTITLE: Subtitle TEST")
		   (insert "\n\n#+BEGIN_EXAMPLE")
		   (insert "\nCopyright (c) 2020 Magnus Malm")
		   (insert "\n#+END_EXAMPLE")
		   (save-buffer))))

	     :config
	     (setq org-html-htmlize-output-type 'css)
	     (setq org-html-htmlize-font-prefix "org-")
	     (setq org-publish-project-alist
		   '(;; Publish the posts
		     ("blog-notes"
		      :base-directory "~/blog/posts"
		      :base-extension "org"
		      :publishing-directory "~/blog/public"
		      :recursive t
		      :publishing-function org-html-publish-to-html
		      :headline-levels 4
		      :section-numbers nil
		      :html-head nil
		      :html-head-include-default-style nil
		      :html-head-include-scripts nil
		      ;; :html-preamble my-blog-header
		      ;; :html-postamble my-blog-footer
		      :auto-sitemap t
		      :sitemap-title "Blog Index"
		      :sitemap-filename "index.org"
		      :sitemap-style list
		      :sitemap-format-entry my-site-format-entry
		      :sitemap-sort-files chronologically
		      ;; :sitemap-sort-files anti-chronologically
		      ;; :sitemap-function my-website-sitemap-function
		      :html-postamble (lambda (arg) (format "Last updated on %s"
							    (format-time-string "%Y-%m-%d")))
		      )

		     ;; For static files that should remain untouched
		     ("blog-static"
		      :base-directory "~/blog/static"
		      :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|eot\\|svg\\|woff\\|woff2\\|ttf"
		      :publishing-directory "~/blog/public"
		      :recursive t
		      :publishing-function org-publish-attachment
		      )

		     ;; Combine the two previous components in a single one
		     ("blog" :components ("blog-notes" "blog-static"))))

	     (add-to-list 'org-export-filter-link-functions 'filter-local-links))



;; (use-package ox-publish
;;   :straight nil
;;   :init

;;   (defun add-html-file (arg)
;;     (with-temp-buffer
;;       (insert-file-contents arg)
;;       (buffer-string)))

;;   (defun my-site-format-entry (entry style project)
;;     (format "[[file:%s][%s]] --- %s"
;;             entry
;;             (org-publish-find-title entry project)
;;             (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))))

;;   (setq my-site-project-path "/home/magnus/blog/")
;;   (setq my-site-publish-path (concat my-site-project-path "public/"))

;;   (setq my-site-extra-head "<link rel='stylesheet' href='/static/main.css' />")
;;   (setq my-site-header-file (concat my-site-project-path "templates/header.html"))
;;   (setq my-site-footer-file (concat my-site-project-path "templates/footer.html"))


;;   (setq org-publish-project-alist
;; 	`(("site"
;; 	   :components ("site-static", "site-pages", "site-images", "site-posts", "site-dl"))
;; 	  ("site-static"
;; 	   :base-directory ,(concat my-site-project-path "static/")
;; 	   :base-extension ".*"
;; 	   :publishing-directory ,(concat my-site-publish-path "static/")
;; 	   :publishing-function org-publish-attachment
;; 	   :recursive t)

;; 	  ("site-images"
;; 	   :base-directory ,(concat my-site-project-path "img")
;; 	   :base-extension ".*"
;; 	   :publishing-directory ,(concat my-site-publish-path "img/")
;; 	   :publishing-function org-publish-attachment
;; 	   :recursive t)

;; 	  ("site-dl"
;; 	   :base-directory ,(concat my-site-project-path "dl")
;; 	   :base-extension ".*"
;; 	   :publishing-directory ,(concat my-site-publish-path "dl/")
;; 	   :publishing-function org-publish-attachment
;; 	   :recursive t)

;; 	  ("site-pages"
;; 	   :base-directory ,(concat my-site-project-path "pages/")
;; 	   :base-extension "org"
;; 	   :publishing-directory ,my-site-publish-path

;; 	   :html-link-home "/"
;; 	   :html-head nil
;; 	   :html-head-extra ,my-site-extra-head
;; 	   :html-head-include-default-style nil
;; 	   :html-head-include-scripts nil
;; 	   :html-home/up-format ""

;; 	   :html-preamble ,(add-html-file my-site-header-file)
;; 	   :html-postamble ,(add-html-file my-site-footer-file)

;; 	   :makeindex nil
;; 	   :with-toc nil
;; 	   :section-numbers nil

;; 	   :publishing-function org-html-publish-to-html)

;; 	  ("site-posts"
;; 	   :base-directory ,(concat my-site-project-path "posts/")
;; 	   :base-extension "org"
;; 	   :publishing-directory ,(concat my-site-publish-path "posts/")

;; 	   :html-link-home "/"
;; 	   :html-head nil
;; 	   :html-head-extra ,my-site-extra-head
;; 	   :html-head-include-default-style nil
;; 	   :html-head-include-scripts nil
;; 	   :html-home/up-format ""

;; 	   :html-preamble ,(add-html-file my-site-header-file)
;; 	   :html-postamble ,(add-html-file my-site-footer-file)

;; 	   :makeindex nil
;; 	   :auto-sitemap t
;; 	   :sitemap-filename "index.org"
;; 	   :sitemap-title "Post"
;; 	   :sitemap-style list
;; 	   :sitemap-sort-files chronologically
;; 	   :sitemap-format-entry my-site-format-entry 
;; 	   :with-toc nil
;; 	   :section-numbers nil

;; 	   :publishing-function org-html-publish-to-html
;; 	   :recursive t)))

;;   )




(use-package ox-html5slide
	     :init
	     (setf org-html-postamble t)
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

(use-package ox-gfm)

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

;;(use-package org-sidebar)

(use-package org-timer
	     :ensure nil
	     :straight nil)

(use-package org-clock
	     :ensure nil
	     :straight nil)

;; Resume clocking task when emacs is restarted
;;; To save the clock history across Emacs sessions, use
(if (file-exists-p org-clock-persist-file)
    ;; (setq org-clock-persist 'history)
    (org-clock-persistence-insinuate)
  (shell-command (concat "touch " org-clock-persist-file)))
;; (org-clock-persistence-insinuate)

(setf org-clock-idle-time 15)

;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))

;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)

;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)

;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(add-hook 'org-clock-out-hook
          '(lambda ()
             (setq org-mode-line-string nil)
             (force-mode-line-update t)))

(setq org-clock-mode-line-total 'current)

(defun mmm/file-and-task ()
  (interactive)
  (when (eq major-mode 'org-mode)
    (format "%s/%s" (file-name-base buffer-file-name) (nth 4 (org-heading-components)))))

(setf org-clock-heading-function #'mmm/file-and-task)

;; export headlines to separate files
;; http://emacs.stackexchange.com/questions/2259/how-to-export-top-level-headings-of-org-mode-buffer-to-separate-files
(defun org-export-headlines-to-pdf ()
  "Export all subtrees that are *not* tagged with :noexport: to
separate files.

Subtrees that do not have the :EXPORT_FILE_NAME: property set
are exported to a filename derived from the headline text."
  (interactive)
  (save-buffer)
  (let ((modifiedp (buffer-modified-p)))
    (save-excursion
      (goto-char (point-min))
      (goto-char (re-search-forward "^*"))
      (set-mark (line-beginning-position))
      (goto-char (point-max))
      (org-map-entries
       (lambda ()
         (let ((export-file (org-entry-get (point) "EXPORT_FILE_NAME")))
           (unless export-file
             (org-set-property
	      "EXPORT_FILE_NAME"
	      (replace-regexp-in-string " " "_" (nth 4 (org-heading-components)))))
           (deactivate-mark)
           (org-latex-export-to-pdf nil t)
           (unless export-file (org-delete-property "EXPORT_FILE_NAME"))
           (set-buffer-modified-p modifiedp)))
       "-noexport" 'region-start-level))))

(defun org-export-current-subtree-to-pdf ()
  (interactive)
  (save-buffer)
  (let ((modifiedp (buffer-modified-p)))
    (save-excursion
      (org-map-entries
       (lambda ()
         (let ((export-file (org-entry-get (point) "EXPORT_FILE_NAME")))
           (unless export-file
             (org-set-property
	      "EXPORT_FILE_NAME"
	      (replace-regexp-in-string " " "_" (nth 4 (org-heading-components)))))
           (deactivate-mark)
           (org-latex-export-to-pdf nil t)
           (unless export-file (org-delete-property "EXPORT_FILE_NAME"))
           (set-buffer-modified-p modifiedp)))
       "-noexport" 'tree))))

;; https://emacs.stackexchange.com/questions/51389/changing-todo-state-of-multiple-headings-at-once
;; https://emacs.stackexchange.com/a/51442
(defun j-change-todo (start end state)
  "Change heading todo states in region defined by START and END to STATE.
Operate on whole buffer if no region is defined."
  (interactive (list
		(if (region-active-p) (region-beginning) (point-min))
		(if (region-active-p) (region-end) (point-max))
		(completing-read "State: " org-todo-keywords-1)))
  (save-excursion
    (goto-char start)
    (when (org-at-heading-p)
      (org-todo state))
    (while (re-search-forward org-heading-regexp end t)
      (org-todo state))))
