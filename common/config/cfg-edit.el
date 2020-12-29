(bind-key "M-e" 'backward-kill-word)
(bind-key "M-r" 'kill-word)
(bind-key "M-d" 'delete-backward-char)
(bind-key "M-f" 'delete-forward-char)

(setf sentence-end-double-space nil)

(use-package editing
  :straight nil
  :ensure nil
  :bind (
	 ("M-x" . xah-cut-line-or-region)
	 ("M-c" . xah-copy-line-or-region)
	 ("M-v" . yank)
	 ("M-/" . toggle-letter-case)
	 ("M-q" . fill-paragraph)
	 ("M-S-SPC" . mark-paragraph)
	 ("M-SPC" . set-mark-command)
	 ))

(use-package undo-fu
  :straight (:host gitlab :repo "ideasman42/emacs-undo-fu")
  :bind (("M-z" . undo-fu-only-undo)
	 ("M-Z" . undo-fu-only-redo)
	 ("C-z" . undo-fu-only-undo)
	 ("C-S-z" . undo-fu-only-redo)))

;; (use-package hungry-delete
;;   :init
;;   (global-hungry-delete-mode)
;;   :bind (("M-d" . hungry-delete-backward)
;; 	 ("M-f" . hungry-delete-forward)))

(use-package browse-kill-ring
  :bind ("M-Y" . browse-kill-ring))

(use-package popup-kill-ring
  :bind (("M-y" . popup-kill-ring)
	 :map popup-kill-ring-keymap
	 ("RET" . popup-kill-ring-select)
	 ("M-k" . popup-kill-ring-next)
	 ("M-i" . popup-kill-ring-previous)
	 ("M-l" . popup-kill-ring-current)
	 ("M-j" . popup-kill-ring-hide)
	 ("<down>" . popup-kill-ring-next)
	 ("<up>" . popup-kill-ring-previous)
	 ("<right>" . popup-kill-ring-current)
	 ("<left>" . popup-kill-ring-hide)))

(use-package multi-line
  :straight (:host github
		   :repo "IvanMalison/multi-line")
  :bind ("C-c d" . multi-line))


(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)
	 ("C-c C-m l" . mc/edit-lines)
	 ("C-c C-m ^" . mc/edit-beginnings-of-lines)
	 ("C-c C-m $" . mc/edit-ends-of-lines)
	 ("C-c C-m ." . mc/mark-all-like-this)
	 ("C-c C-m >" . mc/mark-next-like-this)
	 ("C-c C-m <" . mc/mark-previous-like-this)
	 ("C-c C-m #" . mc/insert-numbers)
	 ("C-c C-m s" . mc/sort-regions)
	 ("C-c C-m r" . mc/reverse-regions)
	 :map mc/keymap
	 ("C-'" . hide-unmatched-lines-mode)))

(use-package ace-mc)

(use-package easy-escape
  :config
  (add-hook 'lisp-mode-hook 'my-lisp-mode-hook-fn)
  (add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook-fn))

;;;; turn on text selection highlighting and make typing override selected text
;;;; (Note: when delete-selection-mode is on, then transient-mark-mode is automatically on too.)
(delete-selection-mode 1)

;;;; Clipboard
;; (setf select-enable-clipboard t
;;       select-enable-primary t)


;;;; UTF-8
(setf utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(setf locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

(prefer-coding-system 'utf-8)

(when (display-graphic-p)
  (setf x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))


(setf replace-character-fold t)

(use-package wgrep)
(use-package wgrep-ag)

(use-package comment-dwim-2
  :bind ("M-'" . comment-dwim-2))

;; (use-package ws-butler
;;   :config
;;   (ws-butler-global-mode))

(use-package edit-at-point
  :bind (("M-C" . hydra-edit-at-point/body)))

(use-package subword
  :ensure nil
  :config
  (global-subword-mode +1)
  (setf c-subword-mode t))

;; (use-package smart-tab
;;   :config
;;   (global-smart-tab-mode 1))

;; (use-package wcheck-mode
;;   :ensure-system-package enchant
;;   :commands (wcheck-mode)
;;   :init
;;   (custom-set-faces
;;    '(wcheck-default-face ((t (:underline (:color: "red" :style wave)))))
;;    )

;;   (setq wcheck-language-data
;; 	'
;; 	(
;; 	 ("British English"
;;           (program . "/usr/bin/enchant")
;;           (args "-l" "-d" "en_GB")
;;           (action-program . "/usr/bin/enchant")
;;           (action-args "-a" "-d" "en_GB")
;;           (action-parser . enchant-suggestions-menu)
;;           (read-or-skip-faces
;;            ;; Only check comments & strings.
;;            ((emacs-lisp-mode c-mode)
;;             read font-lock-comment-face
;;             read font-lock-string-face
;;             )
;;            (nil))
;;           )
;; 	 ("Svenska"
;;           (program . "/usr/bin/enchant")
;;           (args "-l" "-d" "sv_SE")
;;           (action-program . "/usr/bin/enchant")
;;           (action-args "-a" "-d" "sv_SE")
;;           (action-parser . enchant-suggestions-menu)
;;           (read-or-skip-faces
;;            ;; Only check comments & strings.
;;            ((emacs-lisp-mode c-mode)
;;             read font-lock-comment-face
;;             read font-lock-string-face
;;             )
;;            (nil))
;;           )
;; 	 )
;; 	)
;;   (setq wcheck-language "British English")
;;   (setq wcheck-language-data-defaults
;; 	'((read-or-skip-faces
;;            ((emacs-lisp-mode lisp-mode)
;;             read font-lock-comment-face font-lock-doc-face)
;;            (sh-mode
;;             read font-lock-comment-face)
;;            (message-mode
;;             read nil message-header-subject message-cited-text)
;;            (latex-mode
;;             read nil font-latex-sectioning-1-face
;;             font-latex-sectioning-2-face
;;             font-latex-sectioning-3-face
;;             font-latex-sectioning-4-face font-latex-bold-face
;;             font-latex-italic-face font-lock-constant-face)
;;            (org-mode
;;             read nil org-level-1 org-level-2 org-level-3 org-level-4
;;             org-level-5 org-level-6 org-level-7 org-level-8)
;;            (git-commit-mode
;;             read nil git-commit-summary-face))))
;;   (defun enchant-suggestions-menu (marked-text)
;;     (cons (cons "[Add to dictionary]" 'enchant-add-to-dictionary)
;;           (wcheck-parser-ispell-suggestions)))

;;   (defvar enchant-dictionaries-dir "~/.config/enchant")

;;   (defun enchant-add-to-dictionary (marked-text)
;;     (let* ((word (aref marked-text 0))
;;            (language (aref marked-text 4))
;;            (file (let ((code (nth 1 (member "-d" (wcheck-query-language-data
;;                                                   language 'action-args)))))
;;                    (when (stringp code)
;;                      (concat (file-name-as-directory enchant-dictionaries-dir)
;;                              code ".dic")))))

;;       (when (and file (file-writable-p file))
;; 	(with-temp-buffer
;;           (insert word) (newline)
;;           (append-to-file (point-min) (point-max) file)
;;           (message "Added word \"%s\" to the %s dictionary"
;;                    word language)))))
;;   )

(use-package define-word)
(use-package synosaurus
  :bind (:map synosaurus-mode-map
	 (("H-M-l" . synosaurus-lookup)
	  ("H-M-c" . synosaurus-choose-and-replace)))
  :config
  (setf synosaurus-choose-method 'popup))

(use-package ialign
  :bind ("<H-tab>" . ialign))

;; (use-package yasnippet
;;   :config
;;   (yas-global-mode 1)
;;   (defun activate-extra-mode ()
;;     (yas-activate-extra-mode 'fundamental-mode))
;;   :hook (yas-minor-mode . activate-extra-mode)
;;   :custom
;;   (yas-snippet-dirs (list (expand-file-name "config/yasnippets/snippets" user-emacs-directory))))

;; (use-package yasnippet
;;   :blackout yas-minor-mode
;;   :custom
;;   (yas-snippet-dirs (list (expand-file-name "config/yasnippets/snippets" user-emacs-directory))))

;; (use-package yasnippet-snippets
;;   :blackout yas-minor-mode
;;   :after yasnippet
;;   :config (yasnippet-snippets-initialize))

;; (setf flyspell-issue-welcome-flag nil)

;; (use-package ispell
;;   ;; :after auto-capitalize
;;   :bind ("C-c M-s" . mmm/toggle-ispell-language)
;;   :init
;;   (defvar mmm/auto-capitalize-words-old nil)
;;   ;; You should have aspell-sv and aspell-en packages installed
;;   (let ((langs '("british" "svenska")))
;;     (setf lang-ring (make-ring (length langs)))
;;     (dolist (elem langs) (ring-insert lang-ring elem)))

;;   :config
;;   (defun mmm/toggle-ispell-language ()
;;     (interactive)
;;     (if (string-equal "svenska" ispell-dictionary)
;; 	(progn (ispell-change-dictionary "british" t)
;; 	       (setf auto-capitalize-words '("I"))
;; 	       (message "british"))
;;       (progn (ispell-change-dictionary "svenska" t)
;; 	     (setf auto-capitalize-words nil)
;; 	     (message "svenska"))))

;;   (progn (setf ispell-program-name "/usr/bin/ispell")
;; 	 (setf ispell-dictionary "svenska")
;; 	 (setf ispell-personal-dictionary
;; 	       (expand-file-name "dict" user-emacs-directory))
;; 	 (setf ispell-silently-savep t)
;; 	 ))

;; (use-package flyspell
;;   ;; :defer 1
;;   :custom
;;   (flyspell-abbrev-p t)
;;   (flyspell-issue-message-flag nil)
;;   (flyspell-issue-welcome-flag nil)
;;   (flyspell-mode 1))

;; (use-package flyspell-correct
;;   :after flyspell
;;   :bind (:map flyspell-mode-map
;;          ("C-;" . 'flyspell-correct-at-point))
;;   :custom (flyspell-correct-interface 'flyspell-correct-avy-menu))

;; (use-package flyspell-correct-ivy
;;   :after flyspell-correct)

;; (use-package flyspell-correct-popup
;;   :after flyspell-correct)

;; (use-package flyspell-correct-avy-menu
;;   :after flyspell-correct)

(use-package ediff
  :ensure nil
  :config
  (setf ediff-window-setup-function 'ediff-setup-windows-plain)
  (setf ediff-split-window-function 'split-window-horizontally)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

;; Cycle through most common programming identifier styles
(use-package string-inflection
  :preface
  (defun inflect-string ()
    (interactive)
    (cond ((memq major-mode '(java-mode js-mode js2-mode rjsx-mode typescript-mode go-mode))
	   (string-inflection-java-style-cycle))
	  ((memq major-mode '(python-mode ruby-mode c-mode rust-mode))
	   (string-inflection-python-style-cycle))
	  ((derived-mode-p major-mode 'prog-mode)
	   (string-inflection-all-cycle))))
  :config
  (setf string-inflection-skip-backward-when-done t)
  :bind (("C-x C-y" . inflect-string)))

;; Cycle between quotes
(use-package cycle-quotes
  :bind (("C-x C-'" . cycle-quotes)))

(use-package string-edit)

;; (use-package auto-capitalize
;;   :straight (:host github :repo "yuutayamada/auto-capitalize-el")
;;   :config
;;   (setf auto-capitalize-predicate 'auto-capitalize-default-predicate-function)
;;   (setf auto-capitalize-aspell-file (format "%s" "/.aspell.en.pws"))
;;   (auto-capitalize-setup))

;; (use-package wc-mode)

(use-package wrap-region
  :config
  (wrap-region-add-wrapper "`" "`" nil '(markdown-mode))
  (wrap-region-add-wrapper "<b>" "</b>" "b" '(markdown-mode))
  )

(use-package visual-regexp)

(use-package visual-regexp-steroids)

(use-package writegood-mode
  :bind ("C-c g" . writegood-mode))

;; (use-package spell-fu
;;   :straight (:host gitlab :repo "ideasman42/emacs-spell-fu")
;;   :hook ((markdown-mode org-mode text-mode) . spell-fu-mode)
;;   :init
;;   (setq spell-fu-faces-exclude '(org-meta-line org-link org-code))
;;   :config
;;   (global-spell-fu-mode))
