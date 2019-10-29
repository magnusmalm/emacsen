(use-package magit
  :bind
  (("C-c m" . magit-status)
   ("<f2>" . magit-status)
   ("C-c l" . magit-log-buffer-file)
   ("C-c M-g" . magit-dispatch-popup))

  :custom-face
  (magit-hash ((t (:foreground "spring green"))))

  :config
  (setenv "GIT_PAGER" "")
  (setf magit-blame-echo-style 'lines)
  (setf magit-repository-directories '(("~/src" . 1) ("~/devel" . 3)))
  (setf magit-commit-arguments (quote ("--signoff")))
  (setf magit-set-upstream-on-push t)
  (setf magit-revert-buffers 1)
  (setf magit-log-show-refname-after-summary t)
  (setf magit-log-arguments '("--graph" "--decorate" "-n128"))
  (setf magit-log-section-arguments '("--decorate" "-n256"))
  (setf magit-completing-read-function 'ivy-completing-read)
  (setf magit-use-sticky-arguments nil)
  (setf magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

  (setf magit-status-margin '(nil age magit-log-margin-width nil 18))

  (setf magit-status-headers-hook
	'(magit-insert-error-header
	  magit-insert-repo-header
	  magit-insert-remote-header
	  magit-insert-diff-filter-header
	  magit-insert-head-branch-header
	  magit-insert-upstream-branch-header
	  magit-insert-push-branch-header
	  magit-insert-tags-header))

  (setf magit-status-sections-hook
	'(magit-insert-status-headers
	  magit-insert-merge-log
	  magit-insert-rebase-sequence
	  magit-insert-am-sequence
	  magit-insert-sequencer-sequence
	  magit-insert-bisect-output
	  magit-insert-bisect-rest
	  magit-insert-bisect-log
	  magit-insert-untracked-files
	  magit-insert-unstaged-changes
	  magit-insert-staged-changes
	  magit-insert-stashes
	  magit-insert-unpulled-from-upstream
	  magit-insert-unpulled-from-pushremote
	  magit-insert-unpushed-to-upstream
	  magit-insert-unpushed-to-pushremote
	  magit-insert-modules-unpulled-from-upstream
	  magit-insert-modules-unpulled-from-pushremote
	  magit-insert-modules-unpushed-to-upstream
	  magit-insert-modules-unpushed-to-pushremote
	  magit-insert-recent-commits))

  (magit-define-popup-switch 'magit-log-popup
    ?m "Omit merge commits" "--no-merges")

  (magit-define-popup-switch
    'magit-log-popup
    ?1 "First parent" "--first-parent")

  (add-hook 'after-save-hook 'magit-after-save-refresh-status)
  (setf magit-save-repository-buffers 'dontask))

(use-package magit-popup)

;; (use-package magit-rockstar
;;   :config
;;   (magit-define-popup-action 'magit-rebase-popup
;;     ?R "Rockstar" 'magit-rockstar)

;;   (magit-define-popup-action 'magit-commit-popup
;;     ?n "Reshelve" 'magit-reshelve))

;; (use-package magit-todos
;;   :straight (:host github :repo "alphapapa/magit-todos")
;;   :config
;;   (setf magit-todos-exclude-globs '("kernel-dev"))
;;   (setf magit-todos-group-by
;; 	'(magit-todos-item-keyword magit-todos-item-first-path-component))
;;   (setf magit-todos-ignore-directories '("foo" "kernel-dev"))
;;   (setf magit-todos-rg-extra-args nil)
;;   (setf magit-todos-rg-ignore-directories '("foo" "gnu"))
;;   (setf magit-todos-scanner 'magit-todos--scan-with-rg)
;;   (setf magit-todos-update t)
;;   (magit-todos-mode 1))

;; (use-package magit-lfs
;;   :ensure t)

(use-package git-timemachine
  :straight (:host github
	     :repo "emacsmirror/git-timemachine"
	     :branch "master")

  ;; https://github.com/emacsmirror/git-timemachine
  :bind (("C-x M-t" . git-timemachine)
	 ("C-x M-T" . my-git-timemachine))
  :config
  ;; http://blog.binchen.org/posts/enhance-emacs-git-gutter-with-ivy-mode.html
  (defun my-git-timemachine-show-selected-revision ()
    "Show last (current) revision of file."
    (interactive)
    (let* ((collection (mapcar (lambda (rev)
				 ;; re-shape list for the ivy-read
				 (cons (concat (substring-no-properties (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
			       (git-timemachine--revisions))))
      (ivy-read "commits:"
		collection
		:action (lambda (rev)
			  ;; compatible with ivy 9+ and ivy 8
			  (unless (string-match-p "^[a-z0-9]*$" (car rev))
			    (setf rev (cdr rev)))
			  (git-timemachine-show-revision rev)))))

  (defun my-git-timemachine ()
    "Open git snapshot with the selected version.  Based on ivy-mode."
    (interactive)
    (unless (featurep 'git-timemachine)
      (require 'git-timemachine))
    (git-timemachine--start #'my-git-timemachine-show-selected-revision)))

(use-package git-messenger
  :config
  (setf git-messenger:show-detail t))

;; (autoload 'org-read-date "org")

;; (defun magit-org-read-date (prompt &optional _default)
;;   (org-read-date 'with-time nil nil prompt))

;; (magit-define-popup-option 'magit-log-popup
;;   ?s "Since date" "--since=" #'magit-org-read-date)

;; (magit-define-popup-option 'magit-log-popup
;;   ?u "Until date" "--until=" #'magit-org-read-date)

;; (magit-define-popup-switch
;;   'magit-log-popup
;;   ?s "Always sort by date" "--date-order")

(use-package magit-imerge)

;; (define-derived-mode magit-staging-mode magit-status-mode "Magit staging"
;;   "Mode for showing staged and unstaged changes."
;;   :group 'magit-status)

;; (defun magit-staging-refresh-buffer ()
;;   (magit-insert-section (status)
;;     (magit-insert-unstaged-changes)
;;     (magit-insert-staged-changes)))

;; (defun magit-staging ()
;;   (interactive)
;;   (magit-mode-setup #'magit-staging-mode))



;; TODO: Rewrite to use ivy
;;; search commit log by anything
;; (defvar anything-c-source-log-edit-comment
;;   '((name . "Log-edit Comment")
;;     (candidates . anything-c-log-edit-comment-candidates)
;;     (action . (("Insert" . (lambda (str) (insert str)))))
;;     (migemo)
;;     (multiline))
;;   "Source for browse and insert Log-edit comment.")

;; (defun anything-c-log-edit-comment-candidates ()
;;   (let* ((candidates
;;           (shell-command-to-string "\\git \\log -500 | \\grep -E '^    .+'"))
;;          (logs (string-to-list (split-string candidates "\n    "))))
;;     (push (replace-regexp-in-string "^    " "" (pop logs)) logs)
;;     logs))

;; (defun anything-show-log-edit-comment ()
;;   "`anything' for Log-edit comment."
;;   (interactive)
;;   (anything-other-buffer 'anything-c-source-log-edit-comment
;;                          "*anything log-edit comment*"))
;; (define-key magit-log-mode-map (kbd "C-s") 'anything-show-log-edit-comment)

(defun magma/copy-short-hash ()
  (interactive)
  (magit-copy-buffer-revision)
  (let* ((hash (caar magit-revision-stack))
	 (short-hash (magit-rev-parse '"--short" hash)))
    (kill-new (message "%s" short-hash))))

(use-package git-identity
  :after magit
  :config
  (git-identity-magit-mode 1)
  ;; Bind I to git-identity-info in magit-status
  (define-key magit-status-mode-map (kbd "I") 'git-identity-info)
  :custom
  ;; Warn if the global identity setting violates your policy
  (git-identity-verify t)
  ;; The default user name
  (git-identity-default-username "Magnus Malm"))

;; And set git-identity-list in your custom-file or init file
(setq git-identity-list
      '(("magnusmalm@gmail.com"
         :domains ("github.com")
         :dirs ("~/emacsen" "~/src"))
        ("magnus.malm@westermo.se"
         :domains ("git.labs.westermo.se")
         :dirs ("~/devel/etbnd-client" "~/devel/5.x"))))

(defun magit-insert-head-branch-header (&optional branch)
  "Insert a header line about the current branch.
If `HEAD' is detached, then insert information about that commit
instead.  The optional BRANCH argument is for internal use only."
  (let ((branch (or branch (magit-get-current-branch)))
        (output (magit-rev-format "%h %s" (or branch "HEAD"))))
    (string-match "^\\([^ ]+\\) \\(.*\\)" output)
    (magit-bind-match-strings (commit summary) output
      (when (equal summary "")
        (setq summary "(no commit message)"))
      (if branch
          (magit-insert-section (branch branch)
            (insert (format "%-10s" "Head: "))
            (when magit-status-show-hashes-in-headers
              (insert (propertize commit 'font-lock-face 'magit-hash) ?\s))
            (insert (propertize branch 'font-lock-face 'magit-branch-local))
            (insert ?\s)
            (insert (funcall magit-log-format-message-function branch summary))
            (insert ?\n))
        (magit-insert-section (commit commit)
          (insert (format "%-10s" "Head: "))
          (insert (propertize commit 'font-lock-face 'magit-hash))
          (insert ?\s)
          (insert (funcall magit-log-format-message-function nil summary))
          (insert ?\n))))))

(use-package magit-pretty-graph
  :straight (:host github
	     :repo "georgek/magit-pretty-graph"))
