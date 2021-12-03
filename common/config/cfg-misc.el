(unbind-key (kbd "C-z")) ; suspend-frame
(unbind-key (kbd "s-p")) ; ns-print-buffer
(unbind-key (kbd "s-q")) ; save-buffers-kill-emacs
(unbind-key (kbd "s-t")) ; ns-popup-font-panel
(unbind-key (kbd "C-x C-c")) ; save-buffers-kill-terminal

;; (bind-key "H-r" 'repeat)

(bind-key "C-c C-u" 'mmm:uuid)

(use-package list-environment)

(use-package iregister)

;; (use-package cl-format
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook 'cl-format-font-lock-mode))

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

;; (use-package discover)

(use-package helpful)

(use-package man-commands)

(use-package latex-preview-pane)

(use-package blimp
  :straight (:host github
	     :repo "walseb/blimp")
  :hook (image-mode . blimp-mode))

(use-package wicd-mode
  :straight (:host github
	     :repo "rafoo/wicd-mode.el"))

;; (use-package dockerfile-mode)
;; (use-package docker-compose-mode)
;; (use-package docker-tramp)
;; (use-package docker)

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

(use-package ivy-pass)

(use-package back-button
  :bind (("H-S-<right>" . back-button-local-forward)
	 ("H-S-<left>"  . back-button-local-backward))
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

(use-package ssh-tunnels
  :config
  (setq ssh-tunnels-configurations
	'((:name "mware"
	   :type "-R"
	   :local-port 4269
	   :remote-port 4269
	   :login "mware"))))

(use-package nginx-mode)

(use-package keyfreq
  :config
  (setq keyfreq-excluded-commands
	'(self-insert-command
	  org-self-insert-command
	  abort-recursive-edit
	  forward-char
	  backward-char
	  previous-line
	  next-line))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package hercules
  :config
  (hercules-def
   :toggle-funs #'org-babel-mode
   :keymap 'org-babel-map
   :transient t)
  (define-key org-mode-map (kbd "C-c C-v") 'org-babel-mode)

  (hercules-def
   :show-funs #'my-flycheck-mode
   :keymap 'flycheck-command-map
   :transient t)
  (bind-key "C-<f12>" #'my-flycheck-mode)

  (hercules-def
   :show-funs #'windresize
   :hide-funs '(windresize-exit windresize-cancel-and-quit)
   :keymap 'windresize-map)

  (bind-key "<f12>" #'windresize))

(use-package git-undo)
;; :straight (:host github :repo " https://github.com/jwiegley/git-undo-el"))

(use-package zone-nyan
  :config
  (setq zone-programs [zone-nyan]))

(use-package chronos
  :config
  (setf chronos-expiry-functions '(chronos-desktop-notifications-notify)))

;; (use-package mount-mode
;;   :straight (:host github
;; 	     :repo "zellerin/mount-mode")
;;   :config
;;   (setf mount-devices-mask "^sd[b-z][0-9]"))

(use-package ascii-art-to-unicode)

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

;;;  Long line handling
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)

(global-so-long-mode 1)

(use-package debian-el)
(use-package dpkg-dev-el)

(use-package lorem-ipsum)

(use-package dsvn
  :config
  (require 'vc-svn))

(use-package ytdl)

(use-package csv-mode)

(use-package recomplete)

(use-package company-org-block)

(use-package grip-mode
  :ensure t
  :config
  (setf grip-preview-use-webkit nil)
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode)))
