(use-package list-environment)

(use-package iregister)

(use-package cl-format
  :config
  (add-hook 'emacs-lisp-mode-hook 'cl-format-font-lock-mode))

(use-package pushover
  :config
  (setf pushover-user-key mmm/pushover-user-key)
  (setf pushover-api-key mmm/pushover-token))

(use-package sx
  :config
  ;; (setf sx-cache-directory (expand-file-name "var/stackoverflow" user-emacs-directory))
  (bind-keys :prefix "C-c s"
             :prefix-map my-sx-map
             :prefix-docstring "Global keymap for SX."
             ("q" . sx-tab-all-questions)
             ("i" . sx-inbox)
             ("o" . sx-open-link)
             ("u" . sx-tab-unanswered-my-tags)
             ("a" . sx-ask)
             ("s" . sx-search)))

(use-package direnv
  :config
  (direnv-mode))

(use-package discover-my-major)

(use-package discover)

(use-package helpful)

(use-package man-commands)

(use-package latex-preview-pane)

(use-package mpdel)

(use-package undo-tree
  :bind (("M-Z" . undo-tree-redo)
	 ("M-z" . undo-tree-undo))
  :blackout
  :config
  (global-undo-tree-mode))

(use-package simple-mpc)

(use-package mingus)

(use-package blimp
  :straight (:host github
		   :repo "walseb/blimp")
  :hook (image-mode . blimp-mode))

(use-package keychain-environment
  :straight (:host github
		   :repo "tarsius/keychain-environment")
  :config
  (keychain-refresh-environment))

(use-package suggest)

(use-package wicd-mode
  :straight (:host github
		   :repo "rafoo/wicd-mode.el"))


(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package docker-tramp)
(use-package docker)

(use-package ssh-tunnels
  :config
  (setf ssh-tunnels-configurations
	'((:name "my tunnel"
	   :local-port 1234
	   :remote-port 3306
	   :login "me@host"))))

(use-package pass
  :config
  (defun password-store-contents (entry)
    "Return all contents of ENTRY.
Returns all contents of the password data as a list of strings,
one by line."
    (s-lines (password-store--run-show entry)))

  (defun mmmp/assword-store-url (entry)
    (let ((url (password-store-contents entry))
          (url_line nil))
      (while url
        (when (string-prefix-p "url: " (car url))
          (setf url_line (nth 1 (s-split " " (car url)))))
        (setf url (cdr url))
        )
      (if (or (string-prefix-p "http://" url_line)
              (string-prefix-p "https://" url_line)
              (string-prefix-p "www." url_line)
              )
          (browse-url url_line)
        (error "%s" "No url found or string does not look like a URL"))))

  (defun counsel-pass (&optional initial-input)
    (interactive)
    (ivy-read "pass: " 'password-store-list
              :initial-input initial-input
              :dynamic-collection t
              :history 'counsel-pass-history
              :action '(1
                        ("c" password-store-copy "Copy password to clipboard")
                        ("e" password-store-edit "Edit entry")
                        ("O" mmm/password-store-url "Browse url of entry")
                        ("o" (lambda (entry)
                               (let ((passwd (password-store-get entry)))
                                 (password-store-copy entry)
                                 (mmm/password-store-url entry))) "Copy to clipboard & browse url")))))

(use-package back-button
  :bind (("H-<right>" . back-button-local-forward)
	 ("H-<left>"  . back-button-local-backward))
  :blackout
  :config
  (back-button-mode 1))

(use-package restclient
  :config
  (defun my-restclient-mode-hook-func ()
    (setq-local company-backends (add-to-list 'company-backends 'company-restclient)))
  :hook (restclient-mode . my-restclient-mode-hook-func)
  )

(use-package ob-restclient
  :after (restclient org))

(use-package company-restclient
  :after (company restclient))

(use-package free-keys
  :config
  (setf free-keys-modifiers '("" "C" "M" "C-M" "s" "H"))
  (setf free-keys-keys
	"abcdefghijklmnopqrstuvwxyzåäöABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ1234567890!@#$%^&*()-=[]{};'\\:\"|,./<>?`~"))

(use-package goto-last-change
  :bind ("C-x C-/" . goto-last-change))

(use-package outorg)

(use-package outshine)

;; (use-package navi-mode)

(use-package wiki-summary)

(use-package md4rd
  :config
  (setq md4rd-subs-active '(lisp+Common_Lisp emacs commandline embeddedlinux debian)))

(use-package ssh-tunnels
  :config
  (setq ssh-tunnels-configurations
      '((:name "solkattenarboga-swank"
         :local-port 9900
         :remote-port 9900
         :login "solkattenarboga"))))

(use-package twittering-mode
  :init
  (defalias 'epa--decode-coding-string 'decode-coding-string)

  :config
  (twittering-enable-unread-status-notifier)
  (setq twittering-use-master-password t)

  (add-hook 'twittering-mode-hook
            (lambda ()
              (setq twittering-timer-interval 300)
              (setq twittering-url-show-status nil)
	      (setf twittering-icon-mode t)
              (setq twittering-status-format "%i %s\n%FILL[ ]{%T}\n %FACE[glyphless-char]{%@ from %f%L%r%R}\n")
              (set-face-attribute 'twittering-username-face nil
                                  :underline nil
                                  :weight 'bold
                                  :foreground "darksalmon")))

  (add-hook 'twittering-edit-mode-hook
            (lambda ()
              (auto-fill-mode -1)
              (visual-line-mode))))

(use-package frog-menu)

(use-package shx)

(use-package emamux)
