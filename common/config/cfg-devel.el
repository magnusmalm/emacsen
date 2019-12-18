(setq-default fill-column 100)

(use-package smerge-mode
  :straight nil
  :ensure nil
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
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

(use-package doom-todo-ivy
  :straight (:host github
		   :repo "jsmestad/doom-todo-ivy"))

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook (lambda () (prettify-symbols-mode -1))))

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
  :blackout flymake-mode
  :config
  (setf flycheck-cppcheck-suppressions '("variableScope"))
  (setf flycheck-display-errors-delay 0.3)
  (setf flycheck-idle-change-delay 1.0)
  (setf flycheck-indication-mode 'left-fringe)
  (setf flycheck-display-errors-funcjtion 'flycheck-display-error-messages)
  :hook (prog-mode . flycheck-mode))

;;;; INDENTING
(use-package aggressive-indent
  :blackout
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
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
  :blackout
  :config
  (setf dtrt-indent-global-mode t)
  (setf dtrt-indent-ignore-single-chars-flag t)
  (setf dtrt-indent-min-quality 70.0)
  (setf dtrt-indent-run-after-smie t))

(use-package gitlab)
(use-package ivy-gitlab)

(use-package lua-mode
  :config
  (setf lua-default-application "luajit"))

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

(use-package json-mode)

(use-package jq-mode
  :straight (:host github
	     :repo "ljos/jq-mode")
  :bind (:map json-mode-map
	 ("C-c C-j" . 'jq-interactively))
  :config
  (setf jq-interactive-default-options "--sort-keys"))

;;; LSP

(use-package ccls
  :config
  (setf ccls-executable "~/scripts/ccls"))

(use-package lsp-mode
  :config
  (setf lsp-print-io nil)
  (setf lsp-trace nil)
  (setf lsp-enable-indentation nil)
  (setf lsp-enable-on-type-formatting nil)
  (setf lsp-print-performance nil)
  (setf lsp-enable-snippet nil)
  (setf lsp-prefer-flymake nil)
  (setf lsp-auto-guess-root t)
  (setf lsp-document-sync-method 'incremental) ;; none, full, incremental, or nil
  (setf lsp-response-timeout 10)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((go-mode c-mode python-mode) . lsp)
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setf lsp-ui-doc-enable nil)
  (setf lsp-ui-doc-header t)
  (setf lsp-ui-doc-include-signature nil)
  (setf lsp-ui-doc-position 'top) ;; top, bottom, or at-point
  (setf lsp-ui-doc-use-childframe t)
  (setf lsp-ui-doc-use-webkit nil) ;; TODO: Frame with height 0!? Buggy?
  (setf lsp-ui-peek-list-width 60)
  (setf lsp-ui-peek-peek-height 25)
  (setf lsp-ui-flycheck-enable t)
  (setf lsp-ui-flycheck-list-position 'right)
  (setf lsp-ui-flycheck-live-reporting t)
  (setf lsp-ui-sideline-enable t)
  (setf lsp-ui-sideline-ignore-duplicate t)
  (setf lsp-ui-sideline-show-symbol nil)
  (setf lsp-ui-sideline-show-hover t)
  (setf lsp-ui-sideline-show-diagnostics t)
  (setf lsp-ui-sideline-show-code-actions t)
  :hook
  (lsp-mode . lsp-ui-mode)
  (lsp-mode . lsp-ui-sideline-mode)
  ;; (lsp-after-open . (lambda () (lsp-ui-flycheck-enable 1)))
  )


;; Lsp completion
(use-package company-lsp
  :commands company-lsp)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(defun my-devel-mode-hook-func ()
  ;; Show the current function name in the header line
  (which-function-mode)

  (rainbow-identifiers-mode -1))

(add-hook 'prog-mode-hook 'my-devel-mode-hook-func)

(use-package makefile-runner
  :straight (makefile-runner :type git :host github :repo "danamlund/emacs-makefile-runner")
  :bind ("<C-f11>" . makefile-runner))


(use-package treemacs)

(use-package treemacs-projectile)

;; (use-package fill-function-arguments
;;   :hook (prog-mode . (lambda ()
;; 		       (unless (eq 'org-mode major-mode)
;; 			 (local-set-key (kbd "M-q") #'fill-function-arguments-dwim))))
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook (lambda ()
;;                                     (setq-local fill-function-arguments-first-argument-same-line t)
;;                                     (setq-local fill-function-arguments-second-argument-same-line t)
;;                                     (setq-local fill-function-arguments-last-argument-same-line t)
;;                                     (setq-local fill-function-arguments-argument-separator " ")))
;;   (add-hook 'sgml-mode-hook (lambda ()
;;                               (setq-local fill-function-arguments-first-argument-same-line t)
;;                               (setq-local fill-function-arguments-argument-sep " ")
;;                               (local-set-key (kbd "M-q") #'fill-function-arguments-dwim))))
