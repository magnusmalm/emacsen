(setq-default fill-column 100)

(use-package smerge-mode
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

;; (use-package smerge-mode
;;   :straight nil
;;   :ensure nil
;;   :config
;;   :hook (magit-diff-visit-file . (lambda ()
;;                                    (when smerge-mode
;;                                      (unpackaged/smerge-hydra/body)))))

(use-package doom-todo-ivy
  :straight (:host github
	     :repo "jsmestad/doom-todo-ivy"))

(use-package yaml-mode
  :config
  (defun my-yaml-mode-hook-func ()
    (prettify-symbols-mode -1)
    ;; (prism-whitespace-mode 1)
    )
  (add-hook 'yaml-mode-hook 'my-yaml-mode-hook-func))

(use-package yaml-tomato)

(setf gdb-many-windows t)

(setf compilation-scroll-output t)
(use-package bury-successful-compilation
  :config
  (bury-successful-compilation 1))

(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,
close the *compilation* buffer if the compilation is successful,
and set the focus back to Emacs frame"
  (if (string-match "^finished" msg)
      (progn
	(notify "Compilation" "Compilation Successful"))
    (notify "Compilation" "Compilation Failed")))

(add-to-list 'compilation-finish-functions
	     'notify-compilation-result)

(use-package flycheck
  :config
  (setf flycheck-cppcheck-suppressions '("variableScope"))
  (setf flycheck-display-errors-delay 0.3)
  (setf flycheck-idle-change-delay 1.0)
  (setf flycheck-indication-mode 'left-fringe)
  (setf flycheck-display-errors-funcjtion 'flycheck-display-error-messages)
  :hook (prog-mode . flycheck-mode))

;;;; INDENTING
(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'lisp-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'mrepl-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'haml-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'c-mode)
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode)
	 (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

(use-package elf-mode)

(use-package dtrt-indent
  :config
  (setf dtrt-indent-global-mode t)
  (setf dtrt-indent-ignore-single-chars-flag t)
  (setf dtrt-indent-min-quality 70.0)
  (setf dtrt-indent-run-after-smie t))

(use-package gitlab)
(use-package ivy-gitlab)

;; (use-package lua-mode
;;   :config
;;   ;; (setf lua-default-application "luajit")
;;   (setf lua-default-application "lua"))

(use-package go-guru)

(use-package ivy-xref
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  :config
  (setf ivy-xref-remove-text-properties nil)
  (setf ivy-xref-use-file-path t))

(use-package json-snatcher
  :config
  (defun js-mode-bindings ()
    "Sets a hotkey for using the json-snatcher plugin"
    (when (string-match  "\\.json$" (buffer-name))
      (local-set-key (kbd "C-c C-g") 'jsons-print-path)))
  (add-hook 'js-mode-hook 'js-mode-bindings)
  (add-hook 'js2-mode-hook 'js-mode-bindings))

(use-package json-reformat)

(use-package json-mode
  :config
  (defun my-json-mode-hook-func ()
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 2)
    (setq indent-tabs-mode nil))
  (add-hook 'json-mode-hook 'my-json-mode-hook-func))


(use-package jq-mode
  :straight (:host github
	     :repo "ljos/jq-mode")
  :bind (:map json-mode-map
	 ("C-c C-j" . 'jq-interactively))
  :config
  ;; (setf jq-interactive-default-options "--sort-keys")
  (setf jq-interactive-default-options "")
  )

;;; LSP

(use-package ccls
  :config
  (setf ccls-executable "~/scripts/ccls"))

;; (use-package lsp-mode
;;   :config
;;   (setf lsp-print-io nil)
;;   (setf lsp-trace nil)
;;   (setf lsp-enable-indentation nil)
;;   (setf lsp-enable-on-type-formatting nil)
;;   (setf lsp-print-performance nil)
;;   (setf lsp-enable-snippet nil)
;;   (setf lsp-prefer-flymake nil)
;;   (setf lsp-auto-guess-root t)
;;   (setf lsp-document-sync-method 'incremental) ;; none, full, incremental, or nil
;;   (setf lsp-response-timeout 10)
;;   (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
;;   :hook ((go-mode c-mode python-mode) . lsp)
;;   :commands lsp)

;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :config
;;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;   (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
;;   (setf lsp-ui-doc-enable nil)
;;   (setf lsp-ui-doc-header t)
;;   (setf lsp-ui-doc-include-signature nil)
;;   (setf lsp-ui-doc-position 'top) ;; top, bottom, or at-point
;;   (setf lsp-ui-doc-use-childframe t)
;;   (setf lsp-ui-doc-use-webkit nil) ;; TODO: Frame with height 0!? Buggy?
;;   (setf lsp-ui-peek-list-width 60)
;;   (setf lsp-ui-peek-peek-height 25)
;;   (setf lsp-ui-flycheck-enable t)
;;   (setf lsp-ui-flycheck-list-position 'right)
;;   (setf lsp-ui-flycheck-live-reporting t)
;;   (setf lsp-ui-sideline-enable t)
;;   (setf lsp-ui-sideline-ignore-duplicate t)
;;   (setf lsp-ui-sideline-show-symbol nil)
;;   (setf lsp-ui-sideline-show-hover t)
;;   (setf lsp-ui-sideline-show-diagnostics t)
;;   (setf lsp-ui-sideline-show-code-actions t)
;;   :hook
;;   (lsp-mode . lsp-ui-mode)
;;   (lsp-mode . lsp-ui-sideline-mode)
;;   ;; (lsp-after-open . (lambda () (lsp-ui-flycheck-enable 1)))
;;   )


;; ;; Lsp completion
;; (use-package company-lsp
;;   :commands company-lsp)

;; (use-package lsp-treemacs
;;   :commands lsp-treemacs-errors-list)

;; TODO: Try out lsp mode
(use-package eglot
  :config
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure))

(defun my-devel-mode-hook-func ()
  "Show the current function name in the header line."
  (which-function-mode)

  (rainbow-identifiers-mode -1))

(add-hook 'prog-mode-hook 'my-devel-mode-hook-func)

(use-package makefile-runner
  :straight (makefile-runner :type git :host github :repo "danamlund/emacs-makefile-runner")
  :bind ("<C-f11>" . makefile-runner))

(use-package smartparens
  :bind (:map smartparens-mode-map
	 ("M-<left>" . sp-previous-sexp)
	 ("M-<right>" . sp-next-sexp)
	 ("M-<up>" . sp-up-sexp)
	 ("M-<down>" . sp-down-sexp))
  )

(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(use-package transient-dwim
  :straight (:host github
	     :repo "conao3/transient-dwim.el"))

(use-package dired-git-info
  :bind (:map dired-mode-map
	 (")" . dired-git-info-mode))
  :hook
  (dired-after-readin-hook . dired-git-info-auto-enable)
  :config
  (setq dgi-auto-hide-details-p nil))
