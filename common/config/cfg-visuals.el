(use-package frames
  :straight nil
  :ensure nil
  :bind (
	 ("C-S-w" . delete-frame)))


(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
	(output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "../" output)))
    output))

(setq-default mode-line-buffer-identification
	      (propertized-buffer-identification "%b "))

(defvar mode-line-directory
  '(:propertize
    (:eval (if (buffer-file-name)
	       (concat " " (shorten-directory
			    (file-relative-name buffer-file-name (projectile-project-root))
			    20)) " "))
    face mode-line)
  "Formats the current directory.")
(put 'mode-line-directory 'risky-local-variable t)

(setq-default mode-line-format
	      (list

	       mode-line-client
	       mode-line-modified

	       " "
	       mode-line-directory
	       mode-line-buffer-identification
	       " "

	       '(:eval (if vc-mode "@ "))
	       '(:eval (if vc-mode (propertize (substring vc-mode 5)
					       'face 'font-lock-comment-face)))
	       " "
	       '(:eval (if (derived-mode-p 'pdf-view-mode)
			   '(" P" (:eval (number-to-string (pdf-view-current-page)))
			     ;; Avoid errors during redisplay.
			     "/" (:eval (or (ignore-errors
					      (number-to-string (pdf-cache-number-of-pages)))
					    "???")))))
	       ;; line and column
	       '(:eval (unless (derived-mode-p 'pdf-view-mode)
			 '(" ("
			   (:eval (propertize "%03l" 'face 'font-lock-keyword-face)) ","
			   (:eval (propertize "%03c" 'face 'font-lock-keyword-face))
			   ") ")))

	       ;; relative position, size of file
	       ;; " ["
	       ;; (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
	       ;; "/"
	       ;; (propertize "%I" 'face 'font-lock-constant-face) ;; size
	       ;; "] "
	       (propertize "%n" 'face 'font-lock-constant-face) ;; narrow if appropriate
	       ;; spaces to align right
	       '(:eval (propertize
			" " 'display
			`((space :align-to (- (+ right right-fringe right-margin)
					      ,(+ 3 (string-width mode-name)))))))

	       ;; the current major mode
	       (propertize " %m " 'face 'font-lock-string-face)
	       ))

(use-package all-the-icons)

(use-package all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :blackout all-the-icons-dired-mode
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;;;; FONT CHECK
(defvar font-symbola-p nil
  "If non-nil, Symbola font is available on the system.
This font is required for emoji and other Unicode 6+ display.")

(defvar font-dejavu-sans-mono-p nil
  "If non-nil, Dejavu Sans Mono font is available on the system.")

(when (find-font (font-spec :name "Symbola"))
  ;; Manually choose a fallback font for Unicode
  ;; http://endlessparentheses.com/manually-choose-a-fallback-font-for-unicode.html
  (set-fontset-font "fontset-default" nil (font-spec :size 20 :name "Symbola"))
  (setf font-symbola-p t))

(when (find-font (font-spec :name "DejaVu Sans Mono"))
  (setf font-dejavu-sans-mono-p t))

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

(global-prettify-symbols-mode)
(add-hook 'latex-mode-hook (lambda () (prettify-symbols-mode -1)))
(setf prettify-symbols-unprettify-at-point 'right-edge)

;;;; UI
(when window-system
  (set-frame-font "DejaVu Sans Mono-10" nil t)
  (set-fringe-style '(9 . 9)))

(setf default-frame-alist '((font . "DejaVu Sans Mono-10")
			    (cursor-type . box)))
(add-to-list 'default-frame-alist '(mouse-color . "DarkOrange"))
(add-to-list 'default-frame-alist '(cursor-color . "gray"))

(setf frame-title-format
      '("emacs%@" (:eval (system-name)) ": " (:eval (if (buffer-file-name)
							(abbreviate-file-name (buffer-file-name))
						      "%b")) " [%*]"))
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
			   '((vertical-scroll-bars . nil)
			     (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

;;;; Misc
(setf show-paren-delay 0)
(show-paren-mode t)
(setf show-paren-style 'mixed)

;; turn on highlighting current line
(global-hl-line-mode 1)
(make-variable-buffer-local 'global-hl-line-mode)

(use-package ansi-color
  :config
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(blink-cursor-mode 0)

(setq-default cursor-in-non-selected-windows nil)

(use-package fontawesome
  :config
  (defun insert-fontawesome ()
    (interactive)
    (insert (call-interactively 'fontawesome))))

(use-package fill-column-indicator
  :init
  (add-hook 'prog-mode-hook #'fci-mode)
  (defvar sanityinc/fci-mode-suppressed nil)
  (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-mode)
    (when fci-mode
      (turn-off-fci-mode)))
  (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and (not popup-instances) sanityinc/fci-mode-suppressed)
      (setf sanityinc/fci-mode-suppressed nil)
      (turn-on-fci-mode))))

;; Show whitespace
(use-package leerzeichen)

(use-package linum-relative)

(use-package beacon
  :blackout
  :init (beacon-mode 1)
  :config
  (setf beacon-blink-when-focused t)
  (setf beacon-blink-when-point-moves-horizontally 2)
  (setf beacon-blink-when-point-moves-vertically 2)
  (setf beacon-color "spring green")
  (setf beacon-size 60)

  ;; Don't blink on specific major modes
  (add-to-list 'beacon-dont-blink-major-modes 'shell-mode)
  (add-to-list 'beacon-dont-blink-major-modes 'term-mode)
  (add-to-list 'beacon-dont-blink-major-modes 'eshell-mode)
  ;; Don't blink on next-line/previous-line at the top/bottom of the window
  (add-to-list 'beacon-dont-blink-commands 'next-line)
  (add-to-list 'beacon-dont-blink-commands 'previous-line))

(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode t)
  :bind (:map auto-highlight-symbol-mode-map
	 ("C-M-k" . ahs-change-range)
	 ("C-M-l" . ahs-forward)
	 ("C-M-j" . ahs-backward)))

(use-package visible-mark)

(use-package visual-ascii-mode)

(use-package git-gutter
  :blackout
  :bind (("H-g" . hydra-git-gutter/body))
  :blackout
  :config
  (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
			      :hint nil)
    "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
    ("j" git-gutter:next-hunk)
    ("k" git-gutter:previous-hunk)
    ("h" (progn (goto-char (point-min))
		(git-gutter:next-hunk 1)))
    ("l" (progn (goto-char (point-min))
		(git-gutter:previous-hunk 1)))
    ("s" git-gutter:stage-hunk)
    ("r" git-gutter:revert-hunk)
    ("p" git-gutter:popup-hunk)
    ("R" git-gutter:set-start-revision)
    ("q" nil :color blue)
    ("Q" (progn (git-gutter-mode -1)
		;; git-gutter-fringe doesn't seem to
		;; clear the markup right away
		(sit-for 0.1)
		(git-gutter:clear))
     :color blue))
  (global-git-gutter-mode +1))

(use-package default-text-scale
  :config
  (setf default-text-scale-amount 12)
  :bind (
	 ("C-+" . default-text-scale-increase)
	 ("C--" . default-text-scale-decrease)
	 ("C-=" . default-text-scale-reset)
	 ))

(add-hook 'prog-mode-hook
	  (lambda()
	    (local-set-key (kbd "M-<right>") 'hs-show-block)
	    (local-set-key (kbd "M-<left>")  'hs-hide-block)
	    (local-set-key (kbd "M-<up>")    'hs-hide-all)
	    (local-set-key (kbd "M-<down>")  'hs-show-all)
	    (hs-minor-mode t)))

(use-package ov)

(use-package symbol-overlay
  :bind (("C-M-S-i" . symbol-overlay-put)
	 :map symbol-overlay-map
	 ("r" . symbol-overlay-query-replace)
	 ("b" . symbol-overlay-jump-prev)
	 ("f" . symbol-overlay-jump-next)

	 ("M-b" . symbol-overlay-jump-prev)
	 ("M-i" . symbol-overlay-jump-prev)

	 ("M-k" . symbol-overlay-jump-next)
	 ("M-f" . symbol-overlay-jump-next)

	 ("M-n" . symbol-overlay-rename)
	 ("M-r" . symbol-overlay-query-replace)
	 ("M-K" . symbol-overlay-remove-all)
	 ("M-d" . symbol-overlay-jump-to-definition)))

(use-package rainbow-delimiters
  :custom-face
  (rainbow-delimiters-base-face ((t (:inherit nil))))
  (rainbow-delimiters-depth-1-face ((t (:foreground "yellow"))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "dark orange"))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "medium spring green"))))
  (rainbow-delimiters-depth-4-face ((t (:foreground "hot pink"))))
  (rainbow-delimiters-depth-5-face ((t (:foreground "DarkOrange1"))))
  (rainbow-delimiters-depth-6-face ((t (:foreground "white smoke"))))
  (rainbow-delimiters-depth-7-face ((t (:foreground "indian red"))))
  (rainbow-delimiters-depth-8-face ((t (:foreground "navajo white"))))
  (rainbow-delimiters-depth-9-face ((t (:foreground "dodger blue"))))
  :hook ((prog-mode lisp-mode sly-mrepl) . rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :config
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
  (setq-default rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face)
  (setq-default rainbow-identifiers-cie-l*a*b*-lightness 60))

(use-package rainbow-blocks)

(use-package rainbow-mode
  :hook '(css-mode emacs-lisp-mode))

(use-package emojify
  :config
  (setf emojify-download-emojis-p t)
  ;; (add-hook 'after-init-hook #'global-emojify-mode)
  :bind* (("M-S-SPC" . emojify-insert-emoji))
  )

(use-package emoji-cheat-sheet-plus
  :defer t
  :init
  (progn
    ;; enabled emoji in buffer
    (add-hook 'erc-mode-hook 'emoji-cheat-sheet-plus-display-mode)
    ;; insert emoji with helm
    (global-set-key (kbd "C-c C-S-e") 'emoji-cheat-sheet-plus-insert)))

(use-package highlight-indent-guides
  :config
  (setf highlight-indent-guides-auto-character-face-perc 15)
  (setf highlight-indent-guides-auto-enabled nil)
  (setf highlight-indent-guides-auto-even-face-perc 15)
  (setf highlight-indent-guides-auto-top-odd-face-perc 20)
  (setf highlight-indent-guides-method 'character)
  (setf highlight-indent-guides-responsive 'top))

(use-package popup)

(use-package quick-peek)

(use-package rich-minority
  :config
  (setq rm-whitelist
	(format "^ \\(%s\\)$"
		(mapconcat #'identity
			   '("Fly.*" "Projectile.*" "PgLn")
			   "\\|")))
  (rich-minority-mode 1))
