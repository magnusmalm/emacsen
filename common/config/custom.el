(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((time-stamp-active . t)
     (eval setenv "PYTHONPATH"
	   (projectile-project-root))))
 (custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(eyebrowse-mode-line-active ((t (:inherit mode-line-emphasis :foreground "gold"))))
  '(magit-branch-current ((t (:foreground "green" :box 1 :weight bold))))
  '(magit-branch-local ((t (:foreground "lawn green" :weight bold))))
  '(magit-branch-remote ((t (:foreground "cyan" :weight bold))))
  '(magit-dimmed ((t (:foreground "dark gray"))))
  '(magit-log-author ((t (:foreground "yellow"))))
  '(magit-log-date ((t (:foreground "dark gray"))))
  '(magit-popup-argument ((t (:foreground "yellow" :weight bold))))
  '(magit-popup-disabled-argument ((t (:foreground "dark gray" :weight normal))))
  '(magit-popup-key ((t (:foreground "green" :weight bold))))
  '(magit-popup-option-value ((t (:foreground "cyan" :weight bold))))
  '(magit-tag ((t (:foreground "orange" :weight bold))))

  '(magit-hash ((t (:foreground "gold"))))
  '(show-paren-match ((t (:background "gray29" :weight bold))))
  '(show-paren-match-expression ((t (:inherit nil :background "dim gray"))))
  '(smerge-refined-added
    ((t
      (:inherit smerge-refined-change :background "#22aa22" :foreground "white"))))
  '(smerge-refined-removed
    ((t
      (:inherit smerge-refined-change :background "#aa2222" :foreground "white"))))
  '(sml/folder
    ((t
      (:inherit sml/global :background "#383838" :foreground "light gray" :weight normal))))
  '(sml/minor-modes
    ((t
      (:inherit sml/folder :foreground "dark gray" :weight bold))))
  '(sml/vc
    ((t
      (:inherit sml/git :background "#5F5F5F" :foreground "deep sky blue"))))
  '(sml/vc-edited
    ((t
      (:inherit sml/prefix :background "#5F5F5F" :foreground "orange"))))))
