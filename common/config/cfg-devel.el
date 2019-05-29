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

;; (use-package eglot
;;   :config
;;   (add-hook 'prog-mode-hook 'eglot-ensure))

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
  :demand t
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  :bind ("H-L" . hydra-flycheck/body)
  :config
  (setf flycheck-cppcheck-suppressions '("variableScope"))
  (setf flycheck-display-errors-delay 0.3)
  (setf flycheck-idle-change-delay 1.0)
  (setf flycheck-indication-mode 'left-fringe)
  (setf flycheck-display-errors-function nil)

  ;; (use-package flycheck-pos-tip
  ;;   :config
  ;;   (setf flycheck-pos-tip-timeout (* 60 10))
  ;;   (flycheck-pos-tip-mode))

  (defhydra hydra-flycheck
    (:pre (progn (setf hydra-lv t) (flycheck-list-errors))
     :post (progn (setf hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
     :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("n"  flycheck-next-error                                       "Next")
    ("p"  flycheck-previous-error                                   "Previous")
    ("k"  flycheck-next-error                                       "Next")
    ("i"  flycheck-previous-error                                   "Previous")
    ("gg" flycheck-first-error                                      "First")
    ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil))
  (global-flycheck-mode))

;;;; INDENTING

;; (use-package aggressive-indent
;;   :blackout
;;   :config
;;   (global-aggressive-indent-mode 1)
;;   (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
;;   (add-to-list 'aggressive-indent-excluded-modes 'haml-mode)
;;   (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
;;   (add-to-list
;;    'aggressive-indent-dont-indent-if
;;    '(and (derived-mode-p 'c++-mode)
;; 	 (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
;;                              (thing-at-point 'line))))))

(use-package auto-indent-mode)

(use-package elf-mode)

(use-package dtrt-indent
  :blackout
  :config
  (setf dtrt-indent-global-mode t)
  (setf dtrt-indent-ignore-single-chars-flag t)
  (setf dtrt-indent-min-quality 70.0)
  (setf dtrt-indent-run-after-smie t))

;; (use-package flycheck-inline
;;   :straight (:host github
;; 	     :repo "flycheck/flycheck-inline")
;;   :config
;;   ;; (setf flycheck-inline-display-function 'flycheck-inline-display-phantom)
;;   ;; (setf flycheck-inline-clear-function 'flycheck-inline-clear-phantoms)
;;   (setf flycheck-inline-display-function
;;   	(lambda (msg pos)
;;           (let* ((ov (quick-peek-overlay-ensure-at pos))
;;   		 (contents (quick-peek-overlay-contents ov)))
;;             (setf (quick-peek-overlay-contents ov)
;;                   (concat contents (when contents "\n") msg))
;;             (quick-peek-update ov)))
;;   	flycheck-inline-clear-function #'quick-peek-hide)
;;   (global-flycheck-inline-mode))

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
  (setf ivy-xref-use-file-path t)
  )

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


(use-package lsp
  :straight lsp-mode
  :commands lsp
  :defer t
  :config
  (require 'lsp-clients)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  :init
  (setq lsp-eldoc-render-all nil
        lsp-print-io nil
        ;; lsp-inhibit-message t
        ;; lsp-message-project-root-warning t
        lsp-auto-guess-root t
        lsp-prefer-flymake nil
        ;; lsp-session-file (concat conf:cache-dir "lsp-session")
	))

(defvar-local conf:lsp-on-change-exist nil)

(defun conf:lsp-on-change-modify-hook ()
  (if (not conf:lsp-on-change-exist)
      (when (memq 'lsp-on-change after-change-functions)
        (setq conf:lsp-on-change-exist t)
        (remove-hook 'after-change-functions 'lsp-on-change t))
    (add-hook 'after-change-functions #'lsp-on-change nil t)
    (setq conf:lsp-on-change-exist nil)))


(use-package lsp-ui
  :after lsp
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-hover nil
        lsp-ui-doc-enable nil)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company-lsp
  :defer t
  :commands company-lsp
  :config
  (setq company-lsp-async t)
  (push '(company-lsp :with company-yasnippet) company-backends))




;; (use-package lsp-mode
;;   :commands lsp
;;   :config
;;   (setq lsp-prefer-flymake nil)
;;   (setq-default flycheck-disabled-checkers
;; 		'(c/c++-clang c/c++-cppcheck c/c++-gcc)))

;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :after lsp-mode)

;; (use-package company-lsp
;;   :after lsp-mode
;;   :commands company-lsp)

(use-package ccls
  :config
  (setf ccls-executable "~/scripts/ccls")
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

(defun my-devel-mode-hook-func ()
    ;; Show the current function name in the header line
  (which-function-mode)
  (setq-default header-line-format
		'((which-func-mode ("" which-func-format " "))))
  (setq mode-line-misc-info
        ;; We remove Which Function Mode from the mode line, because it's mostly
        ;; invisible here anyway.
        (assq-delete-all 'which-func-mode mode-line-misc-info))

  (semantic-mode 1)
  (rainbow-identifiers-mode -1))

(add-hook 'prog-mode-hook 'my-devel-mode-hook-func)

(use-package smartparens
  :blackout smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))

(use-package makefile-runner
  :straight (makefile-runner :type git :host github :repo "danamlund/emacs-makefile-runner")
  :bind ("<C-f11>" . makefile-runner))
