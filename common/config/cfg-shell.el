
(setf shell-file-name "bash")
(add-to-list 'exec-path "/usr/local/bin")

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "HISTFILE"))

(defun setup-eshell ()
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
  (define-key eshell-mode-map "C-a" 'eshell-maybe-bol)
  (setf eshell-buffer-shorthand t))

(add-hook 'eshell-mode-hook 'setup-eshell)

(use-package multi-term
  :init
  (setf multi-term-dedicated-select-after-open-p t)
  (setf multi-term-scroll-to-bottom-on-output 'this)
  :config
  (setf multi-term-program "/bin/bash"))

(use-package pcmpl-args)
(use-package pcmpl-git)

;; (setf bookmark-default-file (expand-file-name "var/bookmarks" user-emacs-directory))
(use-package eshell-bookmark
  :config
  (add-hook 'eshell-mode-hook 'eshell-bookmark-setup))

(use-package company-shell
  :config
  (add-hook 'shell-mode-hook
	    (lambda () (setq-local company-backends
			      '((company-shell company-files))))))
