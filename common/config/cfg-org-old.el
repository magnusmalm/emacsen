;; Hack to make loading latest org mode work.
;; org-git-version, org-release
;; (require 'subr-x)
;; (straight-use-package 'git)

;; (defun org-git-version ()
;;   "The Git version of org-mode.
;; Inserted by installing org-mode or when a release is made."
;;   (require 'git)
;;   (let ((git-repo (expand-file-name
;; 		   "straight/repos/org/" user-emacs-directory)))
;;     (string-trim
;;      (git-run "describe"
;; 	      "--match=release\*"
;; 	      "--abbrev=6"
;; 	      "HEAD"))))

;; (defun org-release ()
;;   "The release version of org-mode.
;; Inserted by installing org-mode or when a release is made."
;;   (require 'git)
;;   (let ((git-repo (expand-file-name
;; 		   "straight/repos/org/" user-emacs-directory)))
;;     (string-trim
;;      (string-remove-prefix
;;       "release_"
;;       (git-run "describe"
;; 	       "--match=release\*"
;; 	       "--abbrev=0"
;; 	       "HEAD")))))

;; (provide 'org-version)

;; (add-hook 'text-mode-hook
;;           (lambda ()
;;             (variable-pitch-mode 1)))

(use-package org
  :straight nil
  :demand nil
  :ensure nil
  :bind
  (("C-c M-l" . org-store-link)
   ("C-c C-l" . org-insert-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("<pause>" . org-capture)
   ("C-M-|" . indent-rigidly)
   ("M-<left>" . org-do-promote)
   ("M-<right>" . org-do-demote)
   ("M-S-<left>" . org-promote-subtree)
   ("M-S-<right>" . org-demote-subtree)
   ("M-S-<up>" . org-move-subtree-up)
   ("M-S-<down>" . org-move-subtree-down)
   ("C-c C-e" . org-export-dispatch)
   ("C-c C-q" .   air-org-set-tags)

   ("C-c c" .   air-org-task-capture)
   ("C-C t a" . mmm-pop-to-org-agenda)
   ("C-c t n" . air-pop-to-org-notes)
   ("C-c t t" . air-pop-to-org-todo)
   ("C-c t c" . my-open-calendar)
   ("C-c t A" . org-agenda)

   ("C-c f k" . org-search-view)
   ("C-c f t" . org-tags-view)
   ("C-c f i" . air-org-goto-custom-id)
   :map org-mode-map
   ("C-c C-o" . my/org-open-at-point)
   ("<" . mmm-org-insert-template))

  ;; :ensure org-plus-contrib
  :init
  (defun my/org-open-at-point ()
    (interactive)
    (org-open-at-point t))
  (defun air-pop-to-org-todo (&optional split)
    "Visit my main TODO list, in the current window or a SPLIT."
    (interactive "P")
    (air--pop-to-file org-default-tasks-file split))

  (defun air-pop-to-org-notes (&optional split)
    "Visit my main TODO list, in the current window or a SPLIT."
    (interactive "P")
    (air--pop-to-file org-default-notes-file split))

  (defun air-org-task-capture (&optional vanilla)
    "Capture a task with my default template.
If VANILLA is non-nil, run the standard `org-capture'."
    (interactive "P")
    (if vanilla
	(org-capture)
      (org-capture nil "t")))

  (defun air--pop-to-org-agenda-view (key &optional split)
    "Visit the org agenda KEY, in the current window or a SPLIT."
    ;; I don't know why this works, but it works.
    (let ((current-prefix-arg nil))
      (org-agenda nil key))
    (when (not split)
      (delete-other-windows)))

  (defun mmm-org-insert-template ()
    (interactive)
    (if (looking-back "^")
	(hydra-org-template/body)
      (self-insert-command 1)))

  (defun air-org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
	  (pri-value (* 1000 (- org-lowest-priority priority)))
	  (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
	  subtree-end
	nil)))

  (defun air-org-skip-subtree-if-habit ()
    "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (string= (org-entry-get nil "STYLE") "habit")
	  subtree-end
	nil)))

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

  (defun mmm-pop-to-org-agenda (split)
    "Visit the org agenda, in the current window or a SPLIT."
    (interactive "P")
    (org-agenda-list)
    (when (not split)
      (delete-other-windows)))

  (defun hot-expand (str)
    "Expand org template."
    (insert str)
    (org-try-structure-completion))

  (defun air--org-display-tag (tag &optional focus)
    "Display entries tagged with TAG in a fit window.
Do not make the new window current unless FOCUS is set."
    (org-tags-view nil tag)
    (fit-window-to-buffer)
    (unless focus
      (other-window 1)))

  (defun air-org-display-any-tag ()
    "Display entries tagged with a tag selected interactively."
    (interactive)
    (air--org-display-tag (air--org-select-tag)))

  (defun air--org-select-tag ()
    "Interactively select or enter a single tag."
    (let ((org-last-tags-completion-table
	   (if (derived-mode-p 'org-mode)
	       (org-uniquify
		(delq nil (append (org-get-buffer-tags)
				  (org-global-tags-completion-table))))
	     (org-global-tags-completion-table))))
      (completing-read
       "Tag: " 'org-tags-completion-function nil nil nil
       'org-tags-history)))

  (defun air--org-global-custom-ids ()
    "Find custom ID fields in all org agenda files."
    (let ((files (org-agenda-files))
	  file
	  air-all-org-custom-ids)
      (while (setf file (pop files))
	(with-current-buffer (org-get-agenda-file-buffer file)
	  (save-excursion
	    (save-restriction
	      (widen)
	      (goto-char (point-min))
	      (while (re-search-forward "^[ \t]*:CUSTOM_ID:[ \t]+\\(\\S-+\\)[ \t]*$"
					nil t)
		(add-to-list 'air-all-org-custom-ids
			     `(,(match-string-no-properties 1)
			       ,(concat file ":" (number-to-string (line-number-at-pos))))))))))
      air-all-org-custom-ids))

  (defun air--org-find-custom-id (custom-id)
    "Return the location of CUSTOM-ID."
    (let* ((all-custom-ids (air--org-global-custom-ids)))
      (let* ((val (cadr (assoc custom-id all-custom-ids)))
	     (id-parts (split-string val ":"))
	     (file (car id-parts))
	     (line (string-to-int (cadr id-parts))))
	(with-current-buffer (org-get-agenda-file-buffer file)
	  (goto-char (point-min))
	  (forward-line line)
	  (org-reveal)
	  (org-up-element)
	  (list (current-buffer) (point))))))

  (defun air-org-goto-custom-id (&optional split)
    "Go to the location of a custom ID read interactively, maybe in a SPLIT."
    (interactive "P")
    (let* ((all-custom-ids (air--org-global-custom-ids))
	   (custom-id (completing-read
		       "Custom ID: "
		       all-custom-ids))
	   (id-location (air--org-find-custom-id custom-id)))
      (when id-location
	(let* ((buf (car id-location))
	       (loc (cadr id-location)))
	  (pop-to-buffer buf (if split t nil))
	  (goto-char loc)
	  (org-reveal)))))

  (defun air-org-insert-custom-id-link ()
    "Insert an Org link to a custom ID selected interactively."
    (interactive)
    (let* ((all-custom-ids (air--org-global-custom-ids))
	   (custom-id (completing-read
		       "Custom ID: "
		       all-custom-ids)))
      (when custom-id
	(let* ((val (cadr (assoc custom-id all-custom-ids)))
	       (id-parts (split-string val ":"))
	       (file (car id-parts))
	       (line (string-to-int (cadr id-parts))))
	  (org-insert-link nil (concat file "::#" custom-id) custom-id)))))

  (defun air--org-swap-tags (tags)
    "Replace any tags on the current headline with TAGS.
The assumption is that TAGS will be a string conforming to Org Mode's
tag format specifications, or nil to remove all tags."
    (let ((old-tags (org-get-tags-string))
	  (tags (if tags
		    (concat " " tags)
		  "")))
      (save-excursion
	(beginning-of-line)
	(re-search-forward
	 (concat "[ \t]*" (regexp-quote old-tags) "[ \t]*$")
	 (line-end-position) t)
	(replace-match tags)
	(org-set-tags t))))

  (defun air-org-set-tags (tag)
    "Add TAG if it is not in the list of tags, remove it otherwise.
TAG is chosen interactively from the global tags completion table."
    (interactive (list (air--org-select-tag)))
    (let* ((cur-list (org-get-tags))
	   (new-tags (mapconcat 'identity
				(if (member tag cur-list)
				    (delete tag cur-list)
				  (append cur-list (list tag)))
				":"))
	   (new (if (> (length new-tags) 1) (concat " :" new-tags ":")
		  nil)))
      (air--org-swap-tags new)))

  :config
  (load "/home/magnus/emacsen/common/straight/repos/org/lisp/org-macs.el")
  (setf org-confirm-elisp-link-function nil)
  (setf org-agenda-custom-commands
	'(("d" "Daily agenda and all TODOs"
	   ((tags "PRIORITY=\"A\""
		  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		   (org-agenda-overriding-header "High-priority unfinished tasks:")))
	    (agenda "" ((org-agenda-span 'day)))
	    (alltodo ""
		     ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
						     (air-org-skip-subtree-if-priority ?A)
						     (org-agenda-skip-if nil '(scheduled deadline))))
		      (org-agenda-overriding-header "ALL normal priority tasks:"))))
	   ((org-agenda-compact-blocks t)))))

  ;; (setf org-refile-targets '((nil :maxlevel . 2)
  ;; 			     (org-agenda-files :maxlevel . 2)))

  ;; (setf org-refile-use-outline-path t)
  ;; (setf org-refile-allow-creating-parent-nodes 'confirm)
  (setf org-modules '(org-habit))
  (setf org-agenda-include-diary nil)

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
			("economy" . ?e)))

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

  (setf org-journal-dir "~/sync/org/journal/")
  (setf org-directory "~/sync/org/")
  (setf org-default-notes-file "~/sync/org/notes.org")
  (setf org-default-tasks-file "~/sync/org/tasks.org")

  ;; Collect all .org from my Org directory and subdirs
  (setf org-agenda-file-regexp "\\`[^.].*\\.org\\'") ; default value

  (load-org-agenda-files-recursively "~/sync/org/") ; trailing slash required

  (setf org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (emacs-lisp . t)
     (lisp . t)
     (python . t)
     (ruby . t)
     (shell . t)
     (ditaa . t)
     ))

  (font-lock-add-keywords
   'org-mode
   '(("^ +\\([-*]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; (add-hook 'org-mode-hook 'auto-fill-mode)
  ;; (add-hook 'org-mode-hook 'flyspell-mode)

  (defun mmm/org-capture-mode-hook ()
    (bind-keys :map org-capture-mode-map
      ("C-d" . insert-current-time)
      ("M->" . org-priority-up)
      ("M-<" . org-priority-down)
      ("C-t" . air-org-set-tags)))
  (add-hook 'org-capture-mode-hook 'mmm/org-capture-mode-hook)

  (define-key org-mode-map (kbd "M-C-n") 'org-end-of-item-list)
  (define-key org-mode-map (kbd "M-C-p") 'org-beginning-of-item-list)
  (define-key org-mode-map (kbd "M-C-u") 'outline-up-heading)
  (define-key org-mode-map (kbd "M-C-w") 'org-table-copy-region)
  (define-key org-mode-map (kbd "M-C-y") 'org-table-paste-rectangle)

  (define-key org-mode-map [remap org-return] (lambda () (interactive)
						(if (org-in-src-block-p)
						    (org-return)
						  (org-return-indent)))))

;; https://gist.githubusercontent.com/mm--/60e0790bcbf8447160cc87a66dc949ab/raw/04fa1c17237a57a2b17c3e3f42455d2a13eabeaf/org-refile-hydra.el
;; Adapted from https://emacs.stackexchange.com/questions/8045/org-refile-to-a-known-fixed-location
(defun my/refile (file headline &optional arg)
  "Refile to a specific location.

With a 'C-u' ARG argument, we jump to that location (see
`org-refile').

Use `org-agenda-refile' in `org-agenda' mode."
  (let* ((pos (with-current-buffer (or (get-buffer file)	;Is the file open in a buffer already?
				       (find-file-noselect file)) ;Otherwise, try to find the file by name (Note, default-directory matters here if it isn't absolute)
		(or (org-find-exact-headline-in-buffer headline)
		    (error "Can't find headline `%s'" headline))))
	 (filepath (buffer-file-name (marker-buffer pos)));If we're given a relative name, find absolute path
	 (rfloc (list headline filepath nil pos)))
    (if (and (eq major-mode 'org-agenda-mode) (not (and arg (listp arg)))) ;Don't use org-agenda-refile if we're just jumping
	(org-agenda-refile nil rfloc)
      (org-refile arg nil rfloc))))

(defun josh/refile (file headline &optional arg)
  "Refile to HEADLINE in FILE. Clean up org-capture if it's activated.

With a `C-u` ARG, just jump to the headline."
  (interactive "P")
  (let ((is-capturing (and (boundp 'org-capture-mode) org-capture-mode)))
    (cond
     ((and arg (listp arg))	    ;Are we jumping?
      (my/refile file headline arg))
     ;; Are we in org-capture-mode?
     (is-capturing      	;Minor mode variable that's defined when capturing
      (josh/org-capture-refile-but-with-args file headline arg))
     (t
      (my/refile file headline arg)))
    (when (or arg is-capturing)
      (setf hydra-deactivate t))))

(defun josh/org-capture-refile-but-with-args (file headline &optional arg)
  "Copied from `org-capture-refile' since it doesn't allow passing arguments. This does."
  (unless (eq (org-capture-get :type 'local) 'entry)
    (error
     "Refiling from a capture buffer makes only sense for `entry'-type templates"))
  (let ((pos (point))
	(base (buffer-base-buffer (current-buffer)))
	(org-capture-is-refiling t)
	(kill-buffer (org-capture-get :kill-buffer 'local)))
    (org-capture-put :kill-buffer nil)
    (org-capture-finalize)
    (save-window-excursion
      (with-current-buffer (or base (current-buffer))
	(org-with-wide-buffer
	 (goto-char pos)
	 (my/refile file headline arg))))
    (when kill-buffer (kill-buffer base))))

(defmacro josh/make-org-refile-hydra (hydraname file keyandheadline)
  "Make a hydra named HYDRANAME with refile targets to FILE.
KEYANDHEADLINE should be a list of cons cells of the form (\"key\" . \"headline\")"
  `(defhydra ,hydraname (:color blue :after-exit (unless (or hydra-deactivate
							     current-prefix-arg) ;If we're just jumping to a location, quit the hydra
						   (josh/org-refile-hydra/body)))
     ,file
     ,@(cl-loop for kv in keyandheadline
		collect (list (car kv) (list 'josh/refile file (cdr kv) 'current-prefix-arg) (cdr kv)))
     ("q" nil "cancel")))

;;;;;;;;;;
;; Here we'll define our refile headlines
;;;;;;;;;;

(josh/make-org-refile-hydra josh/org-refile-hydra-work
			    "work.org"
			    (("t" . "todos")
			     ("n" . "notes")))

(josh/make-org-refile-hydra josh/org-refile-hydra-private
			    "private.org"
			    (("t" . "todos")
			     ("n" . "notes")))

(josh/make-org-refile-hydra josh/org-refile-hydra-work-archive
			    "work-archive.org"
			    (("t" . "todos")
			     ("n" . "notes")))

(josh/make-org-refile-hydra josh/org-refile-hydra-private-archive
			    "private-archive.org"
			    (("t" . "todos")
			     ("n" . "notes")))

(defhydra josh/org-refile-hydra (:foreign-keys run)
  "Refile"
  ("w" josh/org-refile-hydra-work/body "work" :exit t)
  ("p" josh/org-refile-hydra-private/body "private" :exit t)
  ("W" josh/org-refile-hydra-work-archive/body "work archive" :exit t)
  ("P" josh/org-refile-hydra-private-archive/body "work archive" :exit t)
  ("j" org-refile-goto-last-stored "Jump to last refile" :exit t)
  ("q" nil "cancel"))

(global-set-key (kbd "<f9> r") 'josh/org-refile-hydra/body)

(use-package org-timeline
  :config
  (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append))

(use-package org-kanban)

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

(use-package org-bullets
  :after org
  :init
  ;; (add-hook 'org-mode-hook 'org-bullets-mode)
  (setf org-bullets-bullet-list '("◉" "○")))

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

(use-package org-gcal)

(use-package ox-reveal
  :init
  (setf org-reveal-postamble "Magnus Malm")
  (setf org-reveal-root "file:///home/magnus/src/reveal.js"))

(use-package org-pdfview)

(use-package ical2org
  :ensure nil
  :load-path "lisp/"
  :config
  (setf ical2org/completing-read #'ivy-completing-read))

(use-package calfw-ical)
(use-package calfw-org)

(use-package calfw
  :config
  (setf calendar-week-start-day 1)
  (setf cfw:fchar-junction ?╬
	cfw:fchar-vertical-line ?║
	cfw:fchar-horizontal-line ?═
	cfw:fchar-left-junction ?╠
	cfw:fchar-right-junction ?╣
	cfw:fchar-top-junction ?╦
	cfw:fchar-top-left-corner ?╔
	cfw:fchar-top-right-corner ?╗)

  (setf mmm/cfw-sources
	(list
	 (cfw:org-create-source "Green")
	 (cfw:ical-create-source (first mmm/cfw-cal-magnus)
				 (second mmm/cfw-cal-magnus)
				 (third mmm/cfw-cal-magnus))
	 (cfw:ical-create-source (first mmm/cfw-cal-misc)
				 (second mmm/cfw-cal-misc)
				 (third mmm/cfw-cal-misc))))
  (defun my-open-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources mmm/cfw-sources))

  (setf cfw:org-capture-template'("c" "calfw2org" entry (file nil)  "* %?
   %(cfw:org-capture-day)")))

(use-package poporg)

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

(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setf org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags))
;; Place tags close to the right-hand side of the window
(add-hook 'org-finalize-agenda-hook #'place-agenda-tags)

(setf org-tags-column 0)

(use-package org-web-tools)

(use-package org-projectile
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (progn
    (setf org-projectile-projects-file
          "~/sync/org/projects.org")
    (setf org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)))

(use-package typo)

(defun my-org-mode-hook-func ()
  (add-hook 'org-mode-hook 'emoji-cheat-sheet-plus-display-mode)
  (auto-fill-mode 1)
  (flyspell-mode 1)
  (org-bullets-mode 1)
  (typo-mode 1)
  (blink-cursor-mode 0)
  (emoji-cheat-sheet-plus-display-mode 1)
  (let
      ((sub-keymap (make-sparse-keymap)))
    (define-key sub-keymap "t" 'air-org-set-tags)
    (define-key sub-keymap "T" 'org-change-tag-in-region)
    (define-key sub-keymap "A" 'org-agenda)
    (define-key sub-keymap "c" 'org-capture)
    (define-key sub-keymap "a" 'org-agenda-list)
    (define-key sub-keymap "d" 'org-export-dispatch)
    (define-key sub-keymap "D" 'org-export-dispatch)
    (define-key sub-keymap "R" 'josh/org-refile-hydra/body)
    (key-chord-define-global "OO" sub-keymap))
  )
(add-hook 'org-mode-hook 'my-org-mode-hook-func)

(setf org-icalendar-timezone "Europe/Stockholm")

;; (use-package org-caldav
;;   :config
;;   (defun my-org-caldav-advice (orig-fun &rest args)
;;     (let ((xml-default-ns-bak xml-default-ns)
;; 	  (org-version-bak org-version))
;;       (setf xml-default-ns '(("" . "DAV:")))
;;       (setf org-version "8.2.7")
;;       (apply orig-fun args)
;;       (setf xml-default-ns xml-default-ns-bak)
;;       (setf org-version org-version-bak)))
;;   (advice-add 'org-caldav-sync :around #'my-org-caldav-advice)
;;   (setf org-caldav-uuid-extension ".EML")
;;   (setf org-caldav-save-directory (expand-file-name "~/sync/org/calendars/" user-emacs-directory)))

(use-package org-caldav
  :init
  ;; This is the sync on close function; it also prompts for save after syncing so
  ;; no late changes get lost
  (defun org-caldav-sync-at-close ()
    (org-caldav-sync)
    (save-some-buffers))

  ;; This is the delayed sync function; it waits until emacs has been idle for
  ;; "secs" seconds before syncing.  The delay is important because the caldav-sync
  ;; can take five or ten seconds, which would be painful if it did that right at save.
  ;; This way it just waits until you've been idle for a while to avoid disturbing
  ;; the user.
  (defvar org-caldav-sync-timer nil
     "Timer that `org-caldav-push-timer' used to reschedule itself, or nil.")
  (defun org-caldav-sync-with-delay (secs)
    (when org-caldav-sync-timer
      (cancel-timer org-caldav-sync-timer))
    (setq org-caldav-sync-timer
	  (run-with-idle-timer
	   (* 1 secs) nil 'org-caldav-sync)))

  ;; Actual calendar configuration edit this to meet your specific needs
  (setq org-caldav-url "put_your_caldav_url_here")
      (setq org-caldav-calendars
    '((:calendar-id "desk-org"
	    	:files ("~/path-to-file.org" "~/path-to-file-2.org")
		:inbox "~/Calendars/org-caldav-inbox.org")
	  (:calendar-id "shared_cal1"
		:files ("~/Calendars/shared_cal1.org")
		:inbox "~/Calendars/shared_cal1.org")
      (:calendar-id "default"
		:files ("~/Calendars/shared_cal2.org")
		:inbox "~/Calendars/shared_cal2.org")))
  (setq org-caldav-backup-file "~/org-caldav/org-caldav-backup.org")
  (setq org-caldav-save-directory "~/org-caldav/")

  :config
  (setq org-icalendar-alarm-time 1)
  ;; This makes sure to-do items as a category can show up on the calendar
  (setq org-icalendar-include-todo t)
  ;; This ensures all org "deadlines" show up, and show up as due dates
  (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due))
  ;; This ensures "scheduled" org items show up, and show up as start times
  (setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
  ;; Add the delayed save hook with a five minute idle timer
  (add-hook 'after-save-hook
	    (lambda ()
	      (when (eq major-mode 'org-mode)
		(org-caldav-sync-with-delay 300))))
  ;; Add the close emacs hook
  (add-hook 'kill-emacs-hook 'org-caldav-sync-at-close))


(use-package org-static-blog
  :config
  (setf org-static-blog-publish-title "Tiny Happy Bits")
  (setf org-static-blog-publish-url "https://blog.tinyhappybits.com/")
  (setf org-static-blog-publish-directory "~/tinyhappybits/blog/")
  (setf org-static-blog-posts-directory "~/tinyhappybits/blog/posts/")
  (setf org-static-blog-drafts-directory "~/tinyhappybits/blog/drafts/")
  (setf org-static-blog-enable-tags t)
  (setf org-export-with-toc nil)
  (setf org-export-with-section-numbers nil)

  (setf org-static-blog-page-header
	"<meta name=\"author\" content=\"Magnus Malm\">
<meta name=\"referrer\" content=\"no-referrer\">
<link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<link rel=\"icon\" href=\"static/favicon.ico\">
<link rel=\"apple-touch-icon-precomposed\" href=\"static/favicon-152.png\">
<link rel=\"msapplication-TitleImage\" href=\"static/favicon-144.png\">
<link rel=\"msapplication-TitleColor\" href=\"#0141ff\">
<script src=\"static/katex.min.js\"></script>
<script src=\"static/auto-render.min.js\"></script>
<link rel=\"stylesheet\" href=\"static/katex.min.css\">
<script>document.addEventListener(\"DOMContentLoaded\", function() { renderMathInElement(document.body); });</script>
<meta http-equiv=\"content-type\" content=\"application/xhtml+xml; charset=UTF-8\">
<meta name=\"viewport\" content=\"initial-scale=1,width=device-width,minimum-scale=1\">")

  (setf org-static-blog-page-preamble
	"<div class=\"header\">
  <a href=\"https://bastibe.de\">Tiny Happy Bits</a>
  <div class=\"sitelinks\">
    <a href=\"https://twitter.com/paperflyer\">Twitter</a> | <a href=\"https://github.com/bastibe\">Github</a> | <a href=\"https://bastibe.de/projects.html\">Projects</a>
  </div>
</div>")

  (setf org-static-blog-page-postamble
	"<div id=\"archive\">
  <a href=\"https://bastibe.de/archive.html\">Other posts</a>
</div>
<center><button id=\"disqus_button\" onclick=\"load_disqus()\">Load Disqus Comments</button></center>
<div id=\"disqus_thread\"></div>
<script type=\"text/javascript\">
    function load_disqus() {
        var dsq = document.createElement('script');
        dsq.type = 'text/javascript';
        dsq.async = true;
        dsq.src = 'https://bastibe.disqus.com/embed.js';
        (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
        document.getElementById('disqus_button').style.visibility = 'hidden';
    };
</script>
<center><a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by-sa/3.0/88x31.png\" /></a><br /><span xmlns:dct=\"https://purl.org/dc/terms/\" href=\"https://purl.org/dc/dcmitype/Text\" property=\"dct:title\" rel=\"dct:type\">bastibe.de</span> by <a xmlns:cc=\"https://creativecommons.org/ns#\" href=\"https://bastibe.de\" property=\"cc:attributionName\" rel=\"cc:attributionURL\">Bastian Bechtold</a> is licensed under a <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.</center>")
  )
