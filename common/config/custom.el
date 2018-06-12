(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(atomic-chrome-url-major-mode-alist
   '(("reddit\\.com" . markdown-mode)
     ("github\\.com" . gfm-mode)
     ("redmine" . textile-mode)))
 '(company-quickhelp-delay 2.0)
 '(company-quickhelp-max-lines nil)
 '(company-quickhelp-use-propertized-text t)
 '(company-selection-wrap-around t)
 '(completion-ignored-extensions
   '(".d" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/"
     "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl"
     ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux"
     ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo"))
 '(erc-auto-query 'window-noselect)
 '(erc-modules
   '(fill move-to-prompt stamp spelling hl-nicks netsplit fill button match track completion readonly networks ring autojoin noncommands irccontrols move-to-prompt stamp menu list))
 '(irony-eldoc-use-unicode t)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-position 'at-point)
 '(lsp-ui-flycheck-list-position 'right)
 '(lsp-ui-imenu-kind-position 'top)
 '(lsp-ui-peek-peek-height 20)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-update-mode 'line)
 '(next-error-recenter '(4))
 '(projectile-completion-system 'ivy)
 '(projectile-globally-ignored-directories
   '(".workdir" ".cquery_cached_index" ".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work"))
 '(safe-local-variable-values '((eval setenv "PYTHONPATH" (projectile-project-root))))
 '(swiper-action-recenter t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:background "light green"))))
 '(company-template-field ((t (:background "gray25" :foreground "gold"))))
 '(diredfl-dir-name ((t (:background "gray25" :foreground "#7474FFFFFFFF"))))
 '(eldoc-highlight-function-argument ((t (:inherit underline))))
 '(erc-distinct-1-face ((t (:foreground "deep sky blue"))))
 '(erc-distinct-3-face ((t (:foreground "medium spring green"))))
 '(erc-distinct-7-face ((t (:foreground "light coral"))))
 '(fg:erc-color-face1 ((t (:foreground "gray"))))
 '(fg:erc-color-face12 ((t (:foreground "deep sky blue"))))
 '(fg:erc-color-face14 ((t (:foreground "light gray"))))
 '(fg:erc-color-face2 ((t (:foreground "magenta"))))
 '(fg:erc-color-face3 ((t (:foreground "lawn green"))))
 '(fg:erc-color-face4 ((t (:foreground "coral"))))
 '(fg:erc-color-face5 ((t (:foreground "indian red"))))
 '(fg:erc-color-face6 ((t (:foreground "orchid"))))
 '(font-lock-function-name-face ((t (:foreground "dark orange"))))
 '(lsp-face-highlight-textual ((t (:box (:line-width 1 :color "spring green")))))
 '(lsp-ui-peek-filename ((t (:foreground "lawn green"))))
 '(lsp-ui-peek-header ((t (:background "dim gray" :foreground "gold"))))
 '(lsp-ui-peek-highlight ((t (:distant-foreground "white smoke" :foreground "white smoke" :box (:line-width -1 :color "yellow")))))
 '(lsp-ui-peek-line-number ((t (:foreground "dark gray"))))
 '(lsp-ui-peek-list ((t (:background "dark slate gray"))))
 '(lsp-ui-peek-peek ((t (:background "gray32"))))
 '(lsp-ui-peek-selection ((t (:background "dim gray" :foreground "white smoke"))))
 '(magit-branch-current ((t (:foreground "green" :box 1 :weight bold))))
 '(magit-branch-local ((t (:foreground "lawn green" :weight bold))))
 '(magit-branch-remote ((t (:foreground "cyan" :weight bold))))
 '(magit-dimmed ((t (:foreground "dark gray"))))
 '(magit-hash ((t (:foreground "spring green"))))
 '(magit-log-author ((t (:foreground "yellow"))))
 '(magit-log-date ((t (:foreground "dark gray"))))
 '(magit-popup-argument ((t (:foreground "yellow" :weight bold))))
 '(magit-popup-disabled-argument ((t (:foreground "dark gray" :weight normal))))
 '(magit-popup-key ((t (:foreground "green" :weight bold))))
 '(magit-popup-option-value ((t (:foreground "cyan" :weight bold))))
 '(magit-tag ((t (:foreground "orange" :weight bold))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#DCDCCC" :family "Sans Serif" :height 1.5 :underline nil))))
 '(org-level-1 ((t (:foreground "green1" :weight bold))))
 '(org-level-2 ((t (:foreground "green2"))))
 '(org-level-3 ((t (:foreground "green3"))))
 '(org-level-4 ((t (:foreground "LawnGreen"))))
 '(org-level-5 ((t (:foreground "SpringGreen1"))))
 '(org-level-6 ((t (:foreground "SpringGreen2"))))
 '(org-level-7 ((t (:foreground "SpringGreen3"))))
 '(org-level-8 ((t (:foreground "YellowGreen"))))
 '(org-tree-slide-header-overlay-face ((t (:foreground "yellow" :weight bold)))))
