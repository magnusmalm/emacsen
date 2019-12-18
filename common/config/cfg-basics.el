(if (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(if (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'which-func)

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer))

(use-package movement
  :straight nil
  :ensure nil
  :after crux
  :bind* (
	  ("M-j" . backward-char)
	  ("M-l" . forward-char)
	  ("M-i" . previous-line)
	  ("M-k" . next-line)
	  ("M-u" . backward-word)
	  ("M-o" . forward-word)
	  ("M-J" . backward-paragraph)
	  ("M-L" . forward-paragraph)
	  ("M-h" . crux-move-beginning-of-line)
	  ("M-H" . move-end-of-line)
	  ("M-I" . scroll-down-command)
	  ("M-K" . scroll-up-command)
	  ("M-U" . beginning-of-buffer)
	  ("M-O" . end-of-buffer)
	  ("M-g" . goto-line-show)
	  ))

(use-package auto-compile
  :demand   t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package flx)

(use-package blackout
  :straight (:host github :repo "raxod502/blackout"))

(use-package no-littering
  :init
  (setf no-littering-etc-directory
        (expand-file-name "config/" user-emacs-directory))
  (setf no-littering-var-directory
        (expand-file-name "var/" user-emacs-directory)))

(use-package recentf
  :config
  (setf recentf-auto-cleanup 'mode)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (recentf-mode 1))

(use-package cl-lib)

(column-number-mode t)

(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'ed 'eval-defun)

(setf message-log-max 16384)

(setf enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)
(setf max-mini-window-height 0.5)

;; (setf nsm-settings-file
;;       (expand-file-name "var/network-security.data" user-emacs-directory))

;;;; Annoyances
(setf ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

;; show keystrokes
(setf echo-keystrokes 0.01)

(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setf x-stretch-cursor t)

(setf vc-follow-symlinks t)

;;;; ENCRYPTION
(setq epg-gpg-program "gpg2")
(setf auth-sources
      '("~/.authinfo.gpg"))

(setf epa-pinentry-mode 'loopback)

(setf plstore-cache-passphrase-for-symmetric-encryption t)

(add-to-list 'completion-ignored-extensions "\\.d")

;;;; BACKUPS

(setf backup-by-copying t)

(setf delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(defconst emacs-tmp-dir
  (format "%s%s%s@%s/" temporary-file-directory "emacs-" (user-real-login-name) system-name))

(setf server-socket-dir emacs-tmp-dir)

(setf auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package which-key
  :blackout
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setf which-key-use-C-h-for-paging t)
  (setf which-key-prevent-C-h-from-cycling t)
  (setf which-key-max-display-columns nil)
  (setf which-key-max-description-length 50))


(setf user-full-name "Magnus Malm")

;;;; Imenu
(defun my-shell-mode-setup-imenu ()
  (setf imenu-generic-expression (append '((nil "^\\([A-Z_]+\\)=.*" 1))
                                         (nthcdr 1 (car sh-imenu-generic-expression)))))
(add-hook 'sh-mode-hook 'my-shell-mode-setup-imenu)
(add-hook 'sh-mode-hook 'flycheck-mode)

(defun imenu-elisp-sections ()
  (add-to-list 'imenu-generic-expression '("Sections" "^;\\{1,4\\} \\(.+\\)$" 1) t))
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

(defun imenu-elisp-use-packages ()
  (add-to-list 'imenu-generic-expression '("Package" "^.*use-package \\(.+\\)$" 1) t))
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-use-packages)

(defun imenu-yml-node-name ()
  (add-to-list 'imenu-generic-expression '("Node" "^  - name: \\(.+\\)$" 1) t))
(add-hook 'yaml-mode-hook 'imenu-yml-node-name)

(defun recenter-no-redraw (&optional arg)
  "Like `recenter', but no redrawing."
  (interactive "P")
  (let ((recenter-redisplay nil))
    (recenter arg)))

(add-hook 'imenu-after-jump-hook 'recenter-no-redraw)

(use-package flimenu
  :config
  (progn
    (flimenu-global-mode)))

;; Turn on iMenu for code outlines for all prog and text modes, if possible
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook (lambda () (ignore-errors (imenu-add-menubar-index)))))

;; Visual bookmarks (configured to survive Emacs restarts)
(use-package bm
  :demand t

  :init
  ;; where to store persistant files
  ;; (setf bm-repository-file (concat user-emacs-directory "bm-dir"))

  ;; restore on load (even before you require bm)
  (setf bm-restore-repository-on-load t)

  :config
  (let
      ((sub-keymap (make-sparse-keymap)))
    (define-key sub-keymap "B" 'bm-toggle)
    (define-key sub-keymap "N" 'bm-next)
    (define-key sub-keymap "P" 'bm-previous)
    (define-key sub-keymap "L" 'bm-show-all)
    (define-key sub-keymap "K" 'bm-remove-all-all-buffers)
    (key-chord-define-global "BB" sub-keymap))

  ;; Allow cross-buffer 'next'
  (setf bm-cycle-all-buffers t)

  ;; save bookmarks
  (setq-default bm-buffer-persistence t)

  ;; Loading the repository from file when on start up.
  (add-hook' after-init-hook 'bm-repository-load)

  ;; Restoring bookmarks when on file find.
  (add-hook 'find-file-hooks 'bm-buffer-restore)

  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
				 (bm-buffer-save-all)
				 (bm-repository-save)))

  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save))

(use-package saveplace
  :ensure nil
  :init (save-place-mode)
  :config
  (progn
    ;; (setf save-place-file (concat user-emacs-directory "var/places"))
    ))

(use-package tramp
  :ensure nil
  :straight nil
  :config
  (setf tramp-persistency-file-name
  	(expand-file-name "var/tramp-history.el" user-emacs-directory)))

(setf tramp-completion-reread-directory-timeout nil)
(setf vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
	      vc-ignore-dir-regexp
	      tramp-file-name-regexp))
(setf tramp-verbose 1)
(setf tramp-default-method "ssh")
(setf password-cache-expiry (* 60 60 8))
;; )

(use-package google-this
  :bind (:map google-this-mode-submap
	 ("C-x g" . google-this-mode-submap))
  :blackout
  :config
  (google-this-mode 1))

(use-package anzu
  :blackout
  :bind (("M-%" . anzu-query-replace)
	 ("C-M-%" . anzu-query-replace-regexp))
  :init (global-anzu-mode +1)
  :blackout)

(use-package phi-search
  :bind ("π" . phi-search))

;;;; Startup stuff
(setf inhibit-startup-message t)
(setf inhibit-splash-screen t)
(setf initial-scratch-message nil)
(setf initial-buffer-choice "~/")


(setq scroll-margin 3
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      hscroll-margin 5
      hscroll-step 2)

;; (setf scroll-margin 2)
;; (setf scroll-step 1)
;; (setf scroll-conservatively 10000)
;; (setf scroll-error-top-bottom t)

;;;; COMPANY
(use-package company
  :commands (company-complete-common-or-cycle company-manual-begin company-grab-line)
  :init
  (defvar-local company-fci-mode-on-p nil)
  (setq company-minimum-prefix-length 1
	company-selection-wrap-around t
	company-show-numbers t
	company-idle-delay 0.5
	company-tooltip-limit 20
	company-dabbrev-downcase nil
	company-dabbrev-ignore-case nil
	company-dabbrev-code-other-buffers t
	company-tooltip-align-annotations t
	company-require-match 'never
	company-global-modes
	'(not erc-mode message-mode help-mode gud-mode eshell-mode)
	company-backends '(company-capf)
	company-frontends
	'(company-pseudo-tooltip-frontend
	  company-echo-metadata-frontend))
  :bind (("M-s-SPC" . smart-tab))
  :bind (
	 :map company-active-map
	 ("<tab>" . company-complete-common-or-cycle)
	 ("<escape>" . company-abort)
	 ("M-k" . company-select-next)
	 ("M-i" . company-select-previous)
	 )
  :bind* (("C-<tab>" . my-insert-tab-char))
  :config
  (defun my-insert-tab-char ()
    "Insert a tab char. (ASCII 9, \t)"
    (interactive)
    (insert "\t"))
  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setf company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))

  (setq company-backends '((company-capf
			    company-ispell
			    company-keywords)
			   ;; company-yasnippet)
			   (company-abbrev company-dabbrev)))

  :hook ((company-completion-started . company-turn-off-fci)
	 (company-completion-finished . company-maybe-turn-on-fci)
	 (company-completion-cancelled-hook . company-maybe-turn-on-fci)
	 (after-init . global-company-mode)))

(use-package company-prescient
  :after company
  :init (company-prescient-mode))

(use-package company-flx
  :after company
  :config
  (add-hook 'company-mode-hook (lambda ()
                                 (add-to-list 'company-backends 'company-capf)))
  (company-flx-mode +1))

(use-package company-quickhelp
  :bind (:map company-active-map
	 ("M-h" . company-quickhelp-manual-begin))
  :config
  (setf company-quickhelp-delay t)
  (setf company-quickhelp-delay 1.0)
  (setf company-quickhelp-max-lines nil)
  (setf company-quickhelp-use-propertized-text t)
  (company-quickhelp-mode 1))

;; (use-package company-c-headers
;;   :config
;;   (add-to-list 'company-backends 'company-c-headers))

;;;; NOTIFICATIONS
(use-package notify
  :ensure nil
  :load-path "lisp/")

(defun notify-send (title message)
  (shell-command (format "notify-send -u critical %s %s" title message)))

(use-package alert
  :commands alert
  :config
  (setf alert-default-style 'libnotify))

(use-package eldoc
  :straight nil
  :blackout
  :config
  (setf eldoc-print-after-edit t))

(use-package ripgrep)

(use-package deadgrep)

(use-package expand-region
  :bind (("C-M-c" . er/expand-region)
	 ("C-M-h" . er/mark-defun)))

(defun my-minibuffer-setup-hook ()
  (setf gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setf gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(add-to-list 'auto-mode-alist '("\\.conf\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.ctml\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.lass\\'" . css-mode))

(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "brave-browser")

(use-package executable
  :config
  (setf executable-prefix-env t) ;Use "#!/usr/bin/env python3" style magic number
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p))

(use-package amx)

(use-package counsel
  :bind* (
          ("C-M-S" . counsel-projectile-rg)
	  ("M-s-s" . counsel-projectile-rg)
          ("M-a" . counsel-M-x)
	  ("C-S-p" . counsel-M-x)
          ("C-h v" . counsel-describe-variable)
          ("C-h f" . counsel-describe-function)
          ("C-*" . counsel-descbinds)
          ("C-o" . counsel-find-file)
	  ("<f6>" . counsel-minor)
	  ("S-<f6>" . counsel-major)
          ("C-M-y" . counsel-yank-pop))
  :bind (("C-'" . counsel-semantic-or-imenu)
	 ("C-M-p" . counsel-semantic-or-imenu)
         ("M-S" . mmm/ripgrep-in-current-directory)
	 ("M-s" . counsel-grep-or-swiper))
  :bind (:map counsel-mode-map
	 ("M-k" . ivy-next-line)
	 ("M-i" . ivy-previous-line)
	 ("M-I" . ivy-scroll-down-command)
	 ("M-K" . ivy-scroll-up-command))

  :config
  (put 'counsel-find-symbol 'no-counsel-M-x t)
  (setf counsel-find-file-ignore-regexp
	(concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)"
	 ;; File names ending with .o
	 "\\|\\(?:\\`.+?o\\'\\)"
	 ;; File names ending with .d
	 ;; "\\|\\(?:\\`.+d\\'\\)"
	 ;; Filemake temp files
	 "\\|\\(?:\\`.*_flymake.*\\'\\)"
	 ))

  (defun counsel-yank-bash-history ()
    "Yank the bash history"
    (interactive)
    (let (hist-cmd collection val)
      (shell-command "history -r")	; reload history
      (setf collection
            (nreverse
             (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
                                             (buffer-string))
                           "\n"
                           t)))
      (when (and collection (> (length collection) 0)
		 (setf val (if (= 1 (length collection)) (car collection)
                             (ivy-read (format "Bash history:") collection))))
        (kill-new val)
        (message "%s => kill-ring" val))))

  (defun mmm/counsel-rg (arg)
    "C-u prefix => No initial input, proj scope
   C-0 prefix => No initial input, CWD scope"
    (interactive "P")
    (if arg
        (cond ((and (numberp arg) (= arg 0))
               (setf current-prefix-arg nil)
	       (message "%s" arg)
	       (let ((dir (file-name-directory buffer-file-name)))
		 (counsel-ag nil dir nil
			     (format "%s " dir))))
              ((= (car arg) 4)
               (setf current-prefix-arg nil)
               (counsel-projectile-rg))
	      ;; (counsel-rg nil (projectile-project-root) nil
	      ;; (format "%s " (projectile-project-name)))
              (nil))
      (counsel-ag (symbol-name (symbol-at-point))
                  (projectile-project-root) nil
                  (format "%s " (projectile-project-name)))))

  (defun mmm/ripgrep-in-current-directory (arg)
    (interactive "P")
    (counsel-rg nil default-directory nil (format "[%s] rg: " default-directory)))
  ;; (let ((dir (file-name-directory buffer-file-name)))
  ;;   (counsel-rg nil dir nil
  ;; 	          (format "%s " dir))))

  (ivy-set-prompt 'counsel-projectile-rg #'counsel-prompt-function-dir)
  (setf counsel-grep-base-command
        "rg -i -M 250 --no-heading --line-number --color never --ignore-file '/home/magnus/.ripgrep-ignore' '%s' %s")
  (setf counsel-rg-base-command
        "rg -M 250 --no-heading --line-number --color never --ignore-file '/home/magnus/.ripgrep-ignore' %s ."))

(use-package crux
  :bind (("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c C-d" . crux-duplicate-current-line-or-region)
         ("C-x M-r" . crux-rename-file-and-buffer)
         ;; ("C-<return>" . crux-smart-open-line-above)
         ("C-c I" . crux-find-user-init-file)
         ("M-h" . crux-move-beginning-of-line))
  :init (require 'crux)
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify))

(use-package avy
  :bind* (("C-<return>". avy-goto-char-2)
	  ("C-S-<return>". avy-goto-line))
  :config
  (setf avy-style 'at-full)
  (setq avy-styles-alist '((avy-goto-char-2 . post)))
  (setq avy-keys '(?a ?j ?s ?k ?d ?l ?f ?h))
  (setf avy-single-candidate-jump nil)
  (setf avy-background t)
  (defun avy-show-dispatch-help ()
    "Display action shortucts in echo area."
    (interactive)
    (message "%s" (mapconcat
		   (lambda (action)
		     (cl-destructuring-bind (key . fn) action
		       (format "%s: %s"
			       (propertize
				(char-to-string key)
				'face 'avy-lead-face)
			       fn)))
		   avy-dispatch-alist
		   "\n")))
  (defun avy-goto-open-paren-followed-by-char-or-num (&optional arg)
    "Jump to an open bracket followed by a letter or a number.
 The window scope is determined by `avy-all-windows' (ARG negates it)."
    (interactive "P")
    (avy-with avy-goto-char
      (avy--generic-jump
       "([[:alnum:]]"
       arg
       avy-style)))
  (defun my-avy-action-copy-and-yank (pt)
    "Copy and yank sexp starting on PT."
    (avy-action-copy pt)
    (yank))
  (setq avy-dispatch-alist '((?x . avy-action-kill-move)
			     (?X . avy-action-kill-stay)
			     (?m . avy-action-mark)
			     (?p . my-avy-action-copy-and-yank)
			     (?P . avy-action-teleport)
			     (?n . avy-action-copy)
			     (?y . avy-action-yank)))
  (defhydra hydra-avy (:exit t :hint nil :column t)
    "avy"
    ("c" avy-goto-char "char" :column "Char")
    ("C" avy-goto-char-2 "Char 2 tier")
    ("ac" avy-goto-char-2-above "Char 2 tier above point")
    ("bc" avy-goto-char-2-below "Char 2 tier below point")
    ("lc" avy-goto-char-in-line "Char in current line")
    ("tc" avy-goto-char-timer "Char timed")

    ("w" avy-goto-word-1 "Word" :column "Word")
    ("aw" avy-goto-word-1-above "Word above point")
    ("bw" avy-goto-word-1-below "Word below")

    ("i" avy-goto-symbol-1 "Identifier" :column "Identifier")
    ("ai" avy-goto-symbol-1-above "Identifier above point")
    ("bi" avy-goto-symbol-1-below "Identifier below point")

    ("l" avy-goto-line "Line" :column "Line")
    ("al" avy-goto-line-above "Line above point")
    ("bl" avy-goto-line-below "Line below point")
    ("el" avy-goto-end-of-line "End of line")

    ("x" avy-goto-open-paren-followed-by-char-or-num "" :column "Other")

    ("?" avy-show-dispatch-help "Show actions" :foreign-keys run)

    ("q"  nil)))

(use-package link-hint
  :defer 1
  :config
  (let
      ((sub-keymap (make-sparse-keymap)))
    (define-key sub-keymap "L" 'link-hint-open-link)
    (define-key sub-keymap "C" 'link-hint-copy-link)
    (key-chord-define-global "LL" sub-keymap))
  (setf link-hint-avy-style 'pre)
  (setf link-hint-avy-background t))

(use-package projectile
  :preface
  (defun projectile-project-find-function (dir)
    (let* ((root (projectile-project-root dir)))
      (and root (cons 'transient root))))
  :blackout
  :bind (("H-p" . projectile-command-map)
	 ("M-P M-P" . counsel-projectile-switch-project)
	 ("C-M-P" . counsel-projectile-switch-project)
	 ("S-<f2>" . counsel-projectile-switch-project)
	 ("M-P M-F" . projectile-find-file)
	 ("C-M-o" . projectile-find-file)
	 ("C-p" . projectile-find-file)
	 ("M-P M-S" . counsel-projectile-rg)
	 ("M-P M-M" . magit-status)
	 ("M-P M-C" . projectile-compile-project)
	 ("M-P M-W" . git-messenger:popup-message)
	 ("M-P M-D" . projectile-dired))
  :config
  (setf projectile-git-command "fdfind . -0")
  (setf projectile-generic-command "fdfind . -0")
  (setf projectile-enable-caching t)
  (setf projectile-mode-line '(:eval (format " Proj:%s" (projectile-project-name))))
  (setf projectile-keymap-prefix (kbd "C-x p"))
  (setf projectile-switch-project-action
        #'projectile-commander)
  (setf projectile-project-root-files-top-down-recurring
        (append '("compile_commands.json"
                  ".ccls")
                projectile-project-root-files-top-down-recurring))
  (setf projectile-completion-system 'ivy)
  (setf projectile-globally-ignored-directories
        '(".workdir" ".cquery_cached_index" ".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".ccls-cache" "kernel-dev"))
  (def-projectile-commander-method ?s
    "Open a *shell* buffer for the project."
    (projectile-run-shell))
  (def-projectile-commander-method ?c
    "Run `compile' in the project."
    (projectile-compile-project nil))
  (with-eval-after-load 'project
    (add-to-list 'project-find-functions 'projectile-project-find-function))
  (projectile-global-mode)
  (defun magma/counsel-projectile-switch-to-buffer ()
    "Jump to a buffer in the current project."
    (interactive)
    (let ((ivy-update-fns-alist
           '((ivy-switch-buffer . counsel--switch-buffer-update-fn)))
          (ivy-unwind-fns-alist
           '((ivy-switch-buffer . counsel--switch-buffer-unwind))))
      (if (and (eq projectile-require-project-root 'prompt)
               (not (projectile-project-p)))
          (counsel-projectile-switch-to-buffer-action-switch-project)
	(ivy-read (projectile-prepend-project-name "Switch to buffer: ")
                  ;; We use a collection function so that it is called each
                  ;; time the `ivy-state' is reset. This is needed for the
                  ;; "kill buffer" action.
                  #'counsel-projectile--project-buffers
                  :matcher #'ivy--switch-buffer-matcher
                  :require-match t
                  :sort counsel-projectile-sort-buffers
                  :action counsel-projectile-switch-to-buffer-action
                  :keymap counsel-projectile-switch-to-buffer-map
                  :caller 'counsel-projectile-switch-to-buffer))))
  )

(use-package counsel-projectile
  :config
  (setf counsel-projectile-switch-project-action
	'(1 ("v" counsel-projectile-switch-project-action-vc "open project in vc-dir / magit / monky")
	    ("f" counsel-projectile-switch-project-action-find-file "jump to a project file")
	    ("d" counsel-projectile-switch-project-action-find-dir "jump to a project directory")
	    ("b" counsel-projectile-switch-project-action-switch-to-buffer "jump to a project buffer")
	    ("m" counsel-projectile-switch-project-action-find-file-manually "find file manually from project root")
	    ("S" counsel-projectile-switch-project-action-save-all-buffers "save all project buffers")
	    ("k" counsel-projectile-switch-project-action-kill-buffers "kill all project buffers")
	    ("K" counsel-projectile-switch-project-action-remove-known-project "remove project from known projects")
	    ("c" counsel-projectile-switch-project-action-compile "run project compilation command")
	    ("C" counsel-projectile-switch-project-action-configure "run project configure command")
	    ("E" counsel-projectile-switch-project-action-edit-dir-locals "edit project dir-locals")
	    ("o" counsel-projectile-switch-project-action "jump to a project buffer or file")
	    ("sg" counsel-projectile-switch-project-action-grep "search project with grep")
	    ("ss" counsel-projectile-switch-project-action-ag "search project with ag")
	    ("sr" counsel-projectile-switch-project-action-rg "search project with rg")
	    ("xs" counsel-projectile-switch-project-action-run-shell "invoke shell from project root")
	    ("xe" counsel-projectile-switch-project-action-run-eshell "invoke eshell from project root")
	    ("xt" counsel-projectile-switch-project-action-run-term "invoke term from project root")))
  ;; (counsel-projectile-mode)
  )

(use-package swiper
  :bind (:map  ivy-minibuffer-map
	 ("C-7" . swiper-mc)
	 ("M-s" . insert-symbol-at-point)
         ("M-S" . insert-word-at-point)
	 :map swiper-map
	 ("M-I" . ivy-scroll-down-command)
	 ("M-K" . ivy-scroll-up-command)
	 ("M-s" . insert-symbol-at-point)
	 ("M-S" . insert-word-at-point))

  :custom-face
  ;; (swiper-match-face-1 ((nil :box nil :background "#4F4F4F" :foreground nil)))
  ;; (swiper-match-face-2 ((nil :box nil :background "#5F7F5F" :foreground nil)))
  ;; (swiper-match-face-3 ((nil :box nil :background "#7F9F7F" :foreground nil)))
  ;; (swiper-match-face-4 ((nil :box nil :background "#8FB28F" :foreground nil)))

  (swiper-match-face-1 ((nil :box (:line-width -1 :color "red") :background nil)))
  (swiper-match-face-2 ((nil :box (:line-width -1 :color "green") :background nil)))
  (swiper-match-face-3 ((nil :box (:line-width -1 :color "blue") :background nil)))
  (swiper-match-face-4 ((nil :box (:line-width -1 :color "yellow") :background nil)))

  (swiper-background-match-face-1 ((nil :box (:line-width -1 :color "red") :background nil)))
  (swiper-background-match-face-2 ((nil :box (:line-width -1 :color "green") :background nil)))
  (swiper-background-match-face-3 ((nil :box (:line-width -1 :color "blue") :background nil)))
  (swiper-background-match-face-4 ((nil :box (:line-width -1 :color "yellow") :background nil)))


  ;; ((t (:foreground "spring green"))))

  :config
  (defun insert-symbol-at-point ()
    (interactive)
    (insert (format "%s" (with-ivy-window (thing-at-point 'symbol)))))

  (defun insert-word-at-point ()
    (interactive)
    (insert (format "%s" (with-ivy-window (thing-at-point 'word)))))
  ;; (setf ivy-use-virtual-buffers nil)
  (setf swiper-include-line-number-in-search nil)
  (setf swiper-action-recenter t))

(use-package ivy
  :blackout
  :bind* (("M-M" . ivy-switch-buffer)
	  ("M-m" . counsel-projectile-switch-to-buffer)
  	  ("C-c C-r" . ivy-resume))
  :bind (:map ivy-minibuffer-map
	 ("C-'" . ivy-avy)
	 ("M-i" . ivy-previous-line)
	 ("M-j" . backward-char)
	 ("M-k" . ivy-next-line)
	 ("M-l" . forward-char)
	 ("M-I" . ivy-scroll-down-command)
	 ("M-K" . ivy-scroll-up-command)
	 ("M-p" . ivy-previous-history-element)
	 ("M-n" . ivy-next-history-element)
	 ("M-v" . yank)
	 ("M-u" . backward-word)
	 ("M-o" . forward-word)
	 ("M-e" . backward-kill-word)
	 ("M-r" . kill-word)
	 ("M-x" . ivy-kill-line)
	 ("C-S-u" . ivy-rotate-sort)
	 ("C-S-i" . ivy-rotate-preferred-builders)
	 ("C-S-o" . ivy-dispatching-done)
	 ("C-M-S-o" . ivy-dispatching-call)
	 ("C-M-j" . ivy-immediate-done)
	 ("C-g" . ar/ivy-keyboard-quit-dwim))
  :blackout
  :init
  :config
  (setf ivy-action-wrap t)
  (setf ivy-count-format "(%d/%d) ")
  (setf ivy-display-function nil)
  (setf ivy-wrap t)
  (setf ivy-use-virtual-buffers t)
  (setf ivy-height 25)
  (setf ivy-initial-inputs-alist nil)
  (setf ivy-more-chars-alist
	'((t . 2)))
  (setf ivy-re-builders-alist
	'((ivy-switch-buffer . ivy--regex-ignore-order)
	  (t . ivy--regex-ignore-order)))
  (setf ivy-extra-directories nil)
  (setf ivy-ignore-buffers '("\\ " "\\\*"))
  (ivy-set-actions
   'counsel-find-file
   `(("x"
      (lambda (x) (delete-file (expand-file-name x ivy--directory)))
      ,(propertize "delete" 'face 'font-lock-warning-face))))
  (defun ivy-yank-action (x)
    (kill-new x))

  (defun ivy-copy-to-buffer-action (x)
    (with-ivy-window
      (insert x)))

  (defun ar/ivy-keyboard-quit-dwim ()
    "If region active, deactivate. If there's content, clear the minibuffer. Otherwise quit."
    (interactive)
    (cond ((and delete-selection-mode (region-active-p))
           (setf deactivate-mark t))
          ((> (length ivy-text) 0)
           (delete-minibuffer-contents))
          (t
           (minibuffer-keyboard-quit))))

  (ivy-set-actions
   t
   '(("i" ivy-copy-to-buffer-action "insert")
     ("y" ivy-yank-action "yank")))
  (ivy-mode 1))

;; (use-package ivy-explorer
;;   :config
;;   (ivy-explorer-mode 1))

;; (use-package ivy-historian
;;   :init (ivy-mode +1)
;;   (historian-mode +1)
;;   :config (ivy-historian-mode +1))

(use-package ivy-xref
  :init (if (< emacs-major-version 27)
            (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
          (setq xref-show-definitions-function #'ivy-xref-show-defs)))

;; (use-package ivy-xref
;;   :init (setf xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; (use-package ivy-rich
;;   :straight (:host github
;; 	     :repo "Yevgnen/ivy-rich"
;; 	     :branch "customize")
;;   :init
;;   (setf ivy-rich-path-style 'abbrev)
;;   :config
;;   (defun ivy-rich-switch-buffer-icon (candidate)
;;     (with-current-buffer
;;    	(get-buffer candidate)
;;       (let ((icon (all-the-icons-icon-for-mode major-mode)))
;; 	(if (symbolp icon)
;; 	    (all-the-icons-icon-for-mode 'fundamental-mode)
;; 	  icon))))
;;   (setf ivy-rich--display-transformers-list
;; 	'(ivy-switch-buffer
;;           (:columns
;;            ((ivy-rich-switch-buffer-icon :width 2)
;;             (ivy-rich-candidate (:width 20))
;;             (ivy-rich-switch-buffer-size (:width 7))
;;             (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
;;             (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
;;             (ivy-rich-switch-buffer-project (:width 15 :face success))
;;             (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
;;            :predicate
;;            (lambda (cand) (get-buffer cand)))
;; 	  counsel-M-x
;; 	  (:columns
;; 	   ((counsel-M-x-transformer (:width 40))  ; thr original transfomer
;; 	    (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the command
;; 	  counsel-describe-function
;; 	  (:columns
;; 	   ((counsel-describe-function-transformer (:width 40))  ; the original transformer
;; 	    (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the function
;; 	  counsel-describe-variable
;; 	  (:columns
;; 	   ((counsel-describe-variable-transformer (:width 40))  ; the original transformer
;; 	    (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return the docstring of the variable
;; 	  counsel-recentf
;; 	  (:columns
;; 	   ((ivy-rich-candidate (:width 0.8)) ; return the candidate itself
;; 	    (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))))) ; return the last modified time of the file)
;;   (ivy-rich-mode 1))

(use-package ivy-rich
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
   	(get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
	(if (symbolp icon)
	    (all-the-icons-icon-for-mode 'fundamental-mode)
	  icon))))
  (defun ivy-rich-switch-buffer-nr-lines (candidate)
    (with-current-buffer
	(get-buffer candidate)
      (let ((size (count-lines (point-min) (point-max))))
	(cond
	 ((> size 1000000) (format "%.1fM lines " (/ size 1000000.0)))
	 ((> size 10000) (format "%.1fk lines " (/ size 10000.0)))
	 (t (format "%d lines " size))))))

  (setq ivy-rich--display-transformers-list
	'(ivy-switch-buffer
	  (:columns
	   ((ivy-rich-switch-buffer-icon :width 2)
	    (ivy-rich-candidate (:width 30))
	    (ivy-rich-switch-buffer-project (:width 15 :face success))
	    (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
	    ;; (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
	    (ivy-rich-switch-buffer-indicators (:width 4 :face error))
	    (ivy-rich-switch-buffer-nr-lines (:width 13))
	    (ivy-rich-switch-buffer-size (:width 7))
	    (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))))
	    )
	   :predicate
	   (lambda (cand) (get-buffer cand)))
	  counsel-M-x
	  (:columns
	   ((counsel-M-x-transformer (:width 60))  ; thr original transformer
	    (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the command
	  counsel-describe-function
	  (:columns
	   ((counsel-describe-function-transformer (:width 40))  ; the original transformer
	    (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the function
	  counsel-describe-variable
	  (:columns
	   ((counsel-describe-variable-transformer (:width 40))  ; the original transformer
	    (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return the docstring of the variable
	  counsel-recentf
	  (:columns
	   ((ivy-rich-candidate (:width 0.8)) ; return the candidate itself
	    (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))))) ; return the last modified time of the file

  (ivy-rich-mode 1))

;; (use-package ivy-posframe
;;   :config
;;   (setq
;;    ;; ivy-posframe-width (window-width)
;; 	ivy-posframe-min-width 90
;; 	ivy-posframe-width 110
;;         ivy-posframe-hide-minibuffer nil
;;         ivy-posframe-border-width 1
;;         ;; ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
;;         ivy-display-function #'ivy-posframe-display-at-frame-center)
;;   (ivy-posframe-enable))

;; (use-package ivy-explorer
;;   :diminish ivy-explorer-mode
;;   :config
;;   (if (display-graphic-p)
;;       (setq ivy-explorer-message-function #'ivy-explorer--posframe))
;;   (ivy-explorer-mode 1))

(use-package prescient
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :after (ivy prescient)
  :config
  (ivy-prescient-mode))

(use-package wgrep-ag
  :after ag)

(use-package rg
  :after wgrep-ag projectile
  :ensure-system-package
  (rg . ripgrep))

(use-package ag
  :ensure-system-package
  (ag . silversearcher-ag)
  :config
  (setf ag-highlight-search t))


(setf search-default-mode 'character-fold-to-regexp)

(use-package pandoc-mode)

(use-package markdown-mode
  :ensure-system-package pandoc
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setf markdown-command "pandoc"))

(setf next-error-highlight t)
(bind-key "n" 'next-error ivy-occur-grep-mode-map)
(setf next-error-highlight t)

;; (setf next-error-recenter '(4))
(setf next-error-recenter nil)

(defun magma/buffer-filename-and-func-name ()
  (interactive)
  (if buffer-file-name
      (let ((text (concat (file-relative-name buffer-file-name (projectile-project-root))
			  ":" (format "%s" (line-number-at-pos))
			  " (" (which-function) ")")))
	(kill-new text)
	(message text))
    (message "Buffer is not visiting a regular file.")))
(bind-key "C-M-F" 'magma/buffer-filename-and-func-name)

(defun magma/buffer-filename-and-pos ()
  (interactive)
  (if buffer-file-name
      (let ((text (concat (file-relative-name buffer-file-name (projectile-project-root)) (s-replace "Line " ":" (what-line)))))
	(kill-new text)
	(message text))
    (message "Buffer is not visiting a regular file.")))
(bind-key "C-M-L" 'magma/buffer-filename-and-pos)

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))

(use-package hydra)

(use-package pdf-tools
  :config

  ;; Install what need to be installed !
  (pdf-tools-install t t t)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setf pdf-annot-activate-created-annotations t)
  ;; more fine-grained zooming
  (setf pdf-view-resize-factor 1.1)
  ;;
  (add-hook 'pdf-view-mode-hook
	    (lambda ()
	      (pdf-misc-size-indication-minor-mode)
	      (pdf-links-minor-mode)
	      (pdf-isearch-minor-mode)
	      (which-function-mode -1)
	      (pdf-view-midnight-minor-mode)
	      )
	    )
  (add-to-list 'auto-mode-alist (cons "\\.pdf$" 'pdf-view-mode))

  ;; workaround for pdf-tools not reopening to last-viewed page of the pdf:
  ;; https://github.com/politza/pdf-tools/issues/18#issuecomment-269515117
  (defun brds/pdf-set-last-viewed-bookmark ()
    (interactive)
    (when (eq major-mode 'pdf-view-mode)
      (bookmark-set (brds/pdf-generate-bookmark-name))))

  (defun brds/pdf-jump-last-viewed-bookmark ()
    (when
	(brds/pdf-has-last-viewed-bookmark)
      (bookmark-jump (brds/pdf-generate-bookmark-name))))

  (defun brds/pdf-has-last-viewed-bookmark ()
    (member (brds/pdf-generate-bookmark-name) (bookmark-all-names)))

  (defun brds/pdf-generate-bookmark-name ()
    (concat "PDF-LAST-VIEWED: " (buffer-file-name)))

  (defun brds/pdf-set-all-last-viewed-bookmarks ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(brds/pdf-set-last-viewed-bookmark))))

  (add-hook 'kill-buffer-hook #'brds/pdf-set-last-viewed-bookmark)
  (add-hook 'pdf-view-mode-hook #'brds/pdf-jump-last-viewed-bookmark)
  (unless noninteractive  ; as `save-place-mode' does
    (add-hook 'kill-emacs-hook #'brds/pdf-set-all-last-viewed-bookmarks))


  ;; Keys
  (bind-keys :map pdf-view-mode-map
    ("M-s" . isearch-forward)
    ("C-s" . save-buffer)
    ("M-i" . pdf-view-scroll-up-or-next-page)
    ("M-k" . pdf-view-scroll-down-or-previous-page)
    ("M-U"  . pdf-view-first-page)
    ("M-O"  . pdf-view-last-page)
    ("M-j"  . image-forward-hscroll)
    ("M-l"  . image-backward-hscroll)
    ("M-K"  . pdf-view-next-page)
    ("M-I"  . pdf-view-previous-page)
    ("e"  . pdf-view-goto-page)
    ("g"  . pdf-view-goto-page)
    ("d"  . pdf-view-midnight-minor-mode)
    ("u"  . pdf-view-revert-buffer)
    ("al" . pdf-annot-list-annotations)
    ("ad" . pdf-annot-delete)
    ("aa" . pdf-annot-attachment-dired)
    ("am" . pdf-annot-add-markup-annotation)
    ("at" . pdf-annot-add-text-annotation)
    ("y"  . pdf-view-kill-ring-save)
    ("i"  . pdf-misc-display-metadata)
    ("s"  . pdf-occur)
    ("b"  . pdf-view-set-slice-from-bounding-box)
    ("r"  . pdf-view-reset-slice))

  )

(use-package man
  :straight nil
  :config
  (set-face-attribute 'Man-overstrike nil :inherit 'bold :foreground "#f0dfaf")
  (set-face-attribute 'Man-underline nil :inherit 'underline :foreground "#cc9393"))

(add-hook 'emacs-lisp-mode-hook
	  (lambda () (setq-local lisp-indent-function #'fuco1/lisp-indent-function)))

(setf calendar-latitude 59.393768)
(setf calendar-longitude 15.838480)
(setf calendar-location-name "Arboga, Sweden")

(use-package dumb-jump
  :bind (("H-." . dumb-jump-go))
  :config (setq dumb-jump-selector 'ivy))

(require 're-builder)
(setq reb-re-syntax 'string)
