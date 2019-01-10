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
  :delight
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

(use-package twittering-mode
  :config
  (setf twittering-use-master-password t)
  (setf twittering-icon-mode t)
  (setf twittering-display-remaining t))

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
  :delight
  :config
  (back-button-mode 1))

(use-package smart-forward
  :bind (("M-<up>" . smart-up)
	 ("M-<down>" . smart-down)
	 ("M-<left>" . smart-backward)
	 ("M-<right>" . smart-forward)))

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
