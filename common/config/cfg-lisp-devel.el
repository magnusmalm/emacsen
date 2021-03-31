;; (use-package slime-docker
;;   :custom
;;   (slime-docker-implementations `((sbcl ("sbcl")))))

;; (use-package lispy
;;   :config
;;   (defun enable-lispy-mode () (lispy-mode 1))
;;   (add-hook 'emacs-lisp-mode-hook #'enable-lispy-mode)
;;   (add-hook 'sly-mode-hook #'enable-lispy-mode))

(use-package sly
  :init
  (defun my-sly-mode-hook-fn ()
    (unless (sly-connected-p)
      (save-excursion (sly))))
  :hook (sly-mode . my-sly-mode-hook-fn)
  :config
  (setf inferior-lisp-program "sbcl"))

(use-package sly-quicklisp)



;; (use-package sly-named-readtables
;;   :requires sly)

;; (use-package sly-asdf
;;   :requires sly)

;; (use-package slime)
;; (use-package lispy)



;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; ;; Replace "sbcl" with the path to your implementation
;; (setq inferior-lisp-program "sbcl")
