(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(atomic-chrome-url-major-mode-alist
   '(("reddit\\.com" . markdown-mode)
     ("github\\.com" . gfm-mode)
     ("redmine" . textile-mode)))
 '(beacon-blink-when-focused t)
 '(beacon-blink-when-point-moves-horizontally 2)
 '(beacon-blink-when-point-moves-vertically 2)
 '(beacon-color "spring green")
 '(beacon-size 60)
 '(company-quickhelp-delay 2.0)
 '(company-quickhelp-max-lines nil)
 '(company-quickhelp-use-propertized-text t)
 '(company-selection-wrap-around t)
 '(custom-safe-themes
   '("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" default))
 '(desktop-file-name-format 'absolute)
 '(desktop-path '("~/emacsen/devel/var/" "~"))
 '(erc-auto-query 'window-noselect)
 '(erc-hide-list '("PART" "QUIT"))
 '(erc-modules
   '(fill move-to-prompt stamp spelling hl-nicks netsplit fill button match track completion readonly networks ring autojoin noncommands irccontrols move-to-prompt stamp menu list))
 '(eyebrowse-close-window-config-prompt t)
 '(eyebrowse-mode-line-left-delimiter "{")
 '(eyebrowse-mode-line-right-delimiter "}")
 '(eyebrowse-mode-line-separator "|")
 '(eyebrowse-new-workspace nil)
 '(eyebrowse-switch-back-and-forth t)
 '(eyebrowse-wrap-around t)
 '(git-messenger:show-detail t)
 '(imenu-list-focus-after-activation t)
 '(irony-eldoc-use-unicode t)
 '(ivy-posframe-border-width 0)
 '(jdee-db-active-breakpoint-face-colors (cons "#100e23" "#906cff"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#100e23" "#95ffa4"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#100e23" "#565575"))
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-position 'at-point)
 '(lsp-ui-flycheck-list-position 'right)
 '(lsp-ui-imenu-kind-position 'top)
 '(lsp-ui-peek-peek-height 20)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-update-mode 'line)
 '(magit-blame-echo-style 'lines)
 '(magit-todos-exclude-globs '("kernel-dev"))
 '(magit-todos-group-by
   '(magit-todos-item-keyword magit-todos-item-first-path-component))
 '(magit-todos-ignore-directories '("foo" "kernel-dev"))
 '(magit-todos-rg-extra-args nil)
 '(magit-todos-rg-ignore-directories '("foo" "gnu"))
 '(magit-todos-scanner 'magit-todos--scan-with-rg)
 '(magit-todos-update t)
 '(mu4e-completing-read-function 'ivy-completing-read)
 '(next-error-recenter '(4))
 '(org-icalendar-timezone "Europe/Stockholm")
 '(org-use-sub-superscripts '{})
 '(projectile-completion-system 'ivy)
 '(projectile-globally-ignored-directories
   '(".workdir" ".cquery_cached_index" ".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work"))
 '(safe-local-variable-values
   '((time-stamp-active . t)
     (eval setenv "PYTHONPATH"
	   (projectile-project-root))))
 '(sidebar-adjust-auto-window-width t)
 '(sidebar-filename-colored t)
 '(sidebar-header-line-height 1.5)
 '(sidebar-message-current t)
 '(sml/mode-width 'right)
 '(sml/modified-char "*")
 '(sml/position-percentage-format "")
 '(sml/show-client t)
 '(sml/show-eol t)
 '(sml/show-frame-identification nil)
 '(sml/theme 'respectful)
 '(sml/use-projectile-p 'after-prefixes)
 '(sml/vc-mode-show-backend t)
 '(super-save-auto-save-when-idle nil)
 '(super-save-idle-duration 0)
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
 '(eyebrowse-mode-line-active ((t (:inherit mode-line-emphasis :foreground "gold"))))
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
 '(org-agenda-date-today ((t (:foreground "white smoke" :underline "gold" :slant italic))))
 '(org-agenda-structure ((t (:inherit font-lock-comment-face :foreground "green" :underline "gold"))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#DCDCCC" :family "Sans Serif" :height 1.5 :underline nil))))
 '(org-level-1 ((t (:foreground "green1" :weight bold))))
 '(org-level-2 ((t (:foreground "green2"))))
 '(org-level-3 ((t (:foreground "green3"))))
 '(org-level-4 ((t (:foreground "LawnGreen"))))
 '(org-level-5 ((t (:foreground "SpringGreen1"))))
 '(org-level-6 ((t (:foreground "SpringGreen2"))))
 '(org-level-7 ((t (:foreground "SpringGreen3"))))
 '(org-level-8 ((t (:foreground "YellowGreen"))))
 '(org-time-grid ((t (:foreground "yellow"))))
 '(org-tree-slide-header-overlay-face ((t (:foreground "yellow" :weight bold))))
 '(org-warning ((t (:foreground "orange red" :underline nil :weight bold))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "yellow4"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "DarkOrange3"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "salmon"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "indian red"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "firebrick"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "dark red"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "red"))))
 '(sidebar-ignored-dir ((t (:foreground "dim gray"))))
 '(sidebar-ignored-file ((t (:foreground "dark gray"))))
 '(sml/folder ((t (:inherit sml/global :background "#383838" :foreground "light gray" :weight normal))))
 '(sml/minor-modes ((t (:inherit sml/folder :foreground "dark gray" :weight bold))))
 '(sml/vc ((t (:inherit sml/git :background "#5F5F5F" :foreground "deep sky blue"))))
 '(sml/vc-edited ((t (:inherit sml/prefix :background "#5F5F5F" :foreground "orange")))))
