;; Superior Lisp Interaction Mode for Emacs
(use-package slime
  :defer t
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (use-package slime-company)
  (add-hook 'slime-mode-hook
            (lambda ()
              (add-to-list 'slime-contribs 'inferior-slime)
              (load (expand-file-name "~/quicklisp/slime-helper.el"))
              (add-to-list 'slime-contribs 'slime-fancy))))

(use-package lispy
  :bind (:map lispy-mode-map
	 ("C-i" . special-lispy-tab)
	 ("L" . special-lispy-flow)
	 ("J" . special-lispy-back))
  :config
  (lispy-define-key lispy-mode-map "i" 'lispy-up)
  (lispy-define-key lispy-mode-map "k" 'lispy-down)
  (lispy-define-key lispy-mode-map "j" 'lispy-left)
  (lispy-define-key lispy-mode-map "l" 'lispy-right)
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'lisp-interaction-mode-hook (lambda () (lispy-mode 1))))
