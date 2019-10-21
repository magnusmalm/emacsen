(bind-key "M-e" 'backward-kill-word)
(bind-key "M-r" 'kill-word)

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
	 ("M-Z" . zap-to-char)
	 ("M-z" . undo)
	 ("C-z" . undo)
	 ))

(use-package hungry-delete
  :blackout
  :init
  (global-hungry-delete-mode)
  :bind (("M-d" . hungry-delete-backward)
	 ("M-f" . hungry-delete-forward)))

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
	 ("C-c C-m r" . mc/reverse-regions)))

(use-package ace-mc)

(use-package easy-escape
  :blackout easy-escape-minor-mode
  :config
  (add-hook 'lisp-mode-hook 'my-lisp-mode-hook-fn)
  (add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook-fn))

;;;; turn on text selection highlighting and make typing override selected text
;;;; (Note: when delete-selection-mode is on, then transient-mark-mode is automatically on too.)
(delete-selection-mode 1)

;;;; Clipboard
(setf select-enable-clipboard t
      select-enable-primary t)


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

(use-package ws-butler
  :blackout
  :config
  (ws-butler-global-mode))

(use-package edit-at-point
  :bind (("M-C" . hydra-edit-at-point/body))
  :config
  (defhydra hydra-edit-at-point (:color blue :hint nil :column t)
    "Copy/Cut/Duplicate/Move thing"
    ("w"  edit-at-point-word-copy "word" :column "Copy")
    ("i"  edit-at-point-symbol-copy "identifier")
    ("s"  edit-at-point-str-copy "string")
    ("e"  edit-at-point-paren-copy "expression")
    ("l"  edit-at-point-line-copy "line")
    ("f"  edit-at-point-defun-copy "function")

    ("W"  edit-at-point-word-cut "word" :column "Cut")
    ("I"  edit-at-point-symbol-cut "identifier")
    ("S"  edit-at-point-str-cut "string")
    ("E"  edit-at-point-paren-cut "expression")
    ("L"  edit-at-point-line-cut "line")
    ("F"  edit-at-point-defun-cut "function")

    ("dl"  edit-at-point-line-dup "line" :column "Duplicate")
    ("de"  edit-at-point-paren-dup "expression")
    ("df"  edit-at-point-defun-dup "function")

    ("ml"  edit-at-point-line-up "line up" :column "Move" :color red)
    ("Ml"  edit-at-point-line-down "line down"  :color red)

    ("q"  nil)))

(use-package iedit
  :config
  (defun iedit-dwim (arg)
    "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
    (interactive "P")
    (if arg
	(iedit-mode)
      (save-excursion
	(save-restriction
	  (widen)
	  ;; this function determines the scope of `iedit-start'.
	  (if iedit-mode
	      (iedit-done)
	    ;; `current-word' can of course be replaced by other
	    ;; functions.
	    (narrow-to-defun)
	    (iedit-start (current-word) (point-min) (point-max)))))))
  :bind (("C-;" . iedit-dwim)))

(use-package clipmon
  :config
  (add-to-list 'after-init-hook 'clipmon-mode-start))

(use-package selected
  :init (selected-minor-mode)
  :bind (:map selected-keymap
	      ("M-W" . er/expand-region)
	      ("M-Q" . selected-off)
	      ("M-U" . upcase-region)
	      ("M-D" . downcase-region)
	      ("M-G" . google-this)
	      ("M-E" . iedit-dwim)
	      ("M-M" . apply-macro-to-region-lines)))

(use-package subword
  :ensure nil
  :blackout subword-mode
  :config
  (global-subword-mode +1)
  (setf c-subword-mode t))

(use-package smart-tab
  :blackout
  :config
  (global-smart-tab-mode 1))

(use-package guess-language
  :blackout
  :config
  (setf guess-language-languages '(en sv))
  (setf guess-language-langcodes '((en . ("british" . nil))
				   (sv . ("svenska" . nil))))
  (setf guess-language-min-paragraph-length 35)
  (defun my-guess-lang-mode-hook-function ()
    (guess-language-mode 1))
  (add-hook 'text-mode-hook #'my-guess-lang-mode-hook-function)
  (add-hook 'prog-mode-hook #'my-guess-lang-mode-hook-function))

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
;;   :blackout yas-minor-mode
;;   :custom
;;   (yas-snippet-dirs (list (expand-file-name "config/yasnippets/snippets" user-emacs-directory))))

;; (use-package yasnippet-snippets
;;   :blackout yas-minor-mode
;;   :after yasnippet
;;   :config (yasnippet-snippets-initialize))

(setf flyspell-issue-welcome-flag nil)

(use-package ispell
  :bind ("C-c M-s" . cycle-ispell-languages)
  :init
  ;; You should have aspell-sv and aspell-en packages installed
  (let ((langs '("american" "svenska")))
    (setf lang-ring (make-ring (length langs)))
    (dolist (elem langs) (ring-insert lang-ring elem)))

  (defun cycle-ispell-languages ()
    (interactive)
    (let ((lang (ring-ref lang-ring -1)))
      (ring-insert lang-ring lang)
      (ispell-change-dictionary lang)))

  ;; if (aspell installed) { use aspell}
  ;; else if (hunspell installed) { use hunspell }
  (defun flyspell-detect-ispell-args (&optional run-together)
    "if RUN-TOGETHER is true, spell check the CamelCase words."
    (let (args)
      (cond
       ((string-match  "aspell$" ispell-program-name)
	;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
	(setf args (list "--sug-mode=ultra" "--lang=en_US"))
	(if run-together
	    (setf args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2")))))
       ((string-match "hunspell$" ispell-program-name)
	(setf args nil)))
      args))

  (cond
   ((executable-find "ispell")
    (setf ispell-program-name "ispell"))
   ((executable-find "hunspell")
    (setf ispell-program-name "hunspell")
    ;; just reset dictionary to the safe one "en_US" for hunspell.
    ;; if we need use different dictionary, we specify it in command line arguments
    (setf ispell-local-dictionary "en_US")
    (setf ispell-local-dictionary-alist
	  '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
   (t (setf ispell-program-name nil)))

  ;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
  ;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
  (setq-default ispell-extra-args (flyspell-detect-ispell-args t))
  ;; (setf ispell-cmd-args (flyspell-detect-ispell-args))
  (defadvice ispell-word (around my-ispell-word activate)
    (let ((old-ispell-extra-args ispell-extra-args))
      (ispell-kill-ispell t)
      (setf ispell-extra-args (flyspell-detect-ispell-args))
      ad-do-it
      (setf ispell-extra-args old-ispell-extra-args)
      (ispell-kill-ispell t)))

  (defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
    (let ((old-ispell-extra-args ispell-extra-args))
      (ispell-kill-ispell t)
      ;; use emacs original arguments
      (setf ispell-extra-args (flyspell-detect-ispell-args))
      ad-do-it
      ;; restore our own ispell arguments
      (setf ispell-extra-args old-ispell-extra-args)
      (ispell-kill-ispell t)))

  (defun text-mode-hook-setup ()
    ;; Turn off RUN-TOGETHER option when spell check text-mode
    (setq-local ispell-extra-args (flyspell-detect-ispell-args)))
  (add-hook 'text-mode-hook 'text-mode-hook-setup)

  :config
  (progn (setf ispell-program-name "/usr/bin/ispell")
	 (setf ispell-dictionary "svenska")
	 ;; (setf ispell-personal-dictionary
	 ;;       (expand-file-name "dict/" user-emacs-directory))
	 ))

(use-package abbrev
  :blackout
  :straight nil
  :ensure nil
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
;;;; ispell + abbrev = cool auto-complete
  ;; (setf abbrev-file-name
  ;;	(expand-file-name "personal_abbrev.txt" user-emacs-directory))
  (setf save-abbrevs t)

  (setf save-abbrevs 'silently)
  (setq-default abbrev-mode t))

;; (use-package auto-dictionary
;;   :config
;;   (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))

(use-package flyspell-correct-popup
  :bind (:map popup-menu-keymap
	 ("TAB" . popup-next)
	 ("S-TAB" . popup-previous)))

(defun ap/iedit-or-flyspell ()
  "Call `iedit-mode' or correct misspelling with flyspell, depending..."
  (interactive)
  (if (or iedit-mode
	  (and (derived-mode-p 'prog-mode)
	       (not (or (nth 4 (syntax-ppss))
			(nth 3 (syntax-ppss))))))
      ;; prog-mode is active and point is in a comment, string, or
      ;; already in iedit-mode
      (iedit-mode)
    ;; Not prog-mode or not in comment or string
    (if (not (equal flyspell-previous-command this-command))
	;; FIXME: This mostly works, but if there are two words on the
	;; same line that are misspelled, it doesn't work quite right
	;; when correcting the earlier word after correcting the later
	;; one

	;; First correction; autocorrect
	(call-interactively 'flyspell-auto-correct-previous-word)
      ;; First correction was not wanted; use popup to choose
      (progn
	(save-excursion
	  (undo))  ; This doesn't move point, which I think may be the problem.
	(flyspell-region (line-beginning-position) (line-end-position))
	(call-interactively 'flyspell-correct-previous-word-generic)))))

(use-package flyspell-correct
  ;; :straight (:host github
  ;;		   :repo "d12frosted/flyspell-correct")
  :config
  (define-key flyspell-mode-map (kbd "C-;") #'flyspell-correct-wrapper))

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

