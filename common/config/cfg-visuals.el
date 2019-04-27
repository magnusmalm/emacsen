(setq mode-line-position
      '(;; %p print percent of buffer above top of window, o Top, Bot or All
        ;; (-3 "%p")
        ;; %I print the size of the buffer, with kmG etc
        ;; (size-indication-mode ("/" (-4 "%I")))
        ;; " "
        ;; %l print the current line number
        ;; %c print the current column
        (line-number-mode ("%l" (column-number-mode ":%c")))))

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
    (:eval (if (buffer-file-name) (concat " " (shorten-directory default-directory 20)) " "))
    face mode-line)
  "Formats the current directory.")
(put 'mode-line-directory 'risky-local-variable t)

(setq-default mode-line-format
	      '("%e"
		mode-line-front-space
		;; mode-line-mule-info -- I'm always on utf-8
		mode-line-client
		mode-line-modified
		;; mode-line-remote -- no need to indicate this specially
		;; mode-line-frame-identification -- this is for text-mode emacs only
		" "
		mode-line-directory
		mode-line-buffer-identification
		" "
		mode-line-position
		;;(vc-mode vc-mode)  -- I use magit, not vc-mode
		(flycheck-mode flycheck-mode-line)
		" "
		mode-line-modes
		mode-line-misc-info
		mode-line-end-spaces))

(use-package delight)

(use-package all-the-icons)

(use-package all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :delight all-the-icons-dired-mode
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
(setf show-paren-style 'expression)

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
  :delight
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

(use-package rainbow-delimiters
  :config
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode t))

(use-package visible-mark)

(use-package visual-ascii-mode)

(use-package visual-regexp-steroids
  :config
  (define-key global-map (kbd "C-c C-r r") 'vr/replace)
  (define-key global-map (kbd "C-c C-r q") 'vr/query-replace)
  ;; if you use multiple-cursors, this is for you:
  (define-key global-map (kbd "C-c C-r m") 'vr/mc-mark)
  ;; to use visual-regexp-steroids's isearch instead of the built-in regexp isearch, also include the following lines:
  (define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
  (define-key esc-map (kbd "C-s") 'vr/isearch-forward)) ;; C-M-s)

(use-package git-gutter
  :delight
  :bind (("H-g" . hydra-git-gutter/body))
  :delight
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

(use-package zoom
  :delight
  :config
  (zoom-mode t)
  (defun size-callback ()
    (cond ((> (frame-pixel-width) 1280) '(90 . 0.75))
          (t                            '(0.618 . 0.618))))
  (setf zoom-ignored-major-modes '(dired-mode markdown-mode ediff-mode magit-popup-mode treemacs-mode))
  (setf zoom-size 'size-callback))

(use-package default-text-scale
  :config
  (setf default-text-scale-amount 12))

(use-package color-identifiers-mode
  :disabled
  :config
  (add-hook 'after-init-hook 'global-color-identifiers-mode))

;; (use-package yafolding
;;   :bind* (("C-M-<return>" . 'yafolding-toggle-element))
;;   :config
;;   (yafolding-mode 1))

;; (use-package origami
;;   :bind (("C-<return>" . 'origami-recursively-toggle-node))
;;   :config
;;   (global-origami-mode))
;; Modern code folding
;; (use-package origami
;;   :config
;;   (with-eval-after-load 'hideshow
;;     ;; Unloading is unsafe, so this the best I can do to pretend `hideshow'
;;     ;; never existed.
;;     (setf minor-mode-map-alist
;;           (assq-delete-all 'hs-minor-mode minor-mode-map-alist)
;;           minor-mode-alist
;;           (assq-delete-all 'hs-minor-mode minor-mode-alist)
;;           minor-mode-list
;;           (delq 'hs-minor-mode minor-mode-list)))
;;   :bind (:map origami-mode-map
;;          ("M-0"   . origami-open-all-nodes)
;;          ("M-9"   . origami-close-all-nodes)
;;          ("C-M-/" . origami-recursively-toggle-node)))

(add-hook 'prog-mode-hook
  (lambda()
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
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

(use-package rainbow-identifiers
  :config
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
  (setq-default rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face)
  (setq-default rainbow-identifiers-cie-l*a*b*-lightness 60))

(use-package rainbow-mode
  :hook css-mode)

(setf resize-mini-windows t)

(use-package emojify
  :config
  (setf emojify-download-emojis-p t)
  (add-hook 'after-init-hook #'global-emojify-mode))

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


(use-package yequake
  :straight (:host github
	     :repo "alphapapa/yequake")
  :custom
  (yequake-frames
   '(("org-capture"
      (buffer-fns . (yequake-org-capture))
      (width . 0.75)
      (height . 0.5)
      (alpha . 0.95)
      (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (sticky . t)))))))

(use-package quick-peek)

(use-package rich-minority
  :config
  (setq rm-whitelist
	(format "^ \\(%s\\)$"
		(mapconcat #'identity
                           '("Fly.*" "Projectile.*" "PgLn")
                           "\\|")))
  (rich-minority-mode 1))

;; (use-package company-box
;;   :config
;;   (setf company-box-max-candidates 25)
;;   (setf company-box-backends-colors
;; 	'((company-yasnippet . (:candidate "yellow" :annotation some-face))
;; 	  (company-elisp . (:icon "yellow" :selected (:background "orange"
;; 						      :foreground "black")))
;; 	  (company-dabbrev . "purple")))
;;   :hook
;;   (company-mode . company-box-mode))


;; (setq company-box-icons-unknown 'fa_question_circle)

;; (setq company-box-icons-elisp
;;       '((fa_tag :face font-lock-function-name-face) ;; Function
;; 	(fa_cog :face font-lock-variable-name-face) ;; Variable
;; 	(fa_cube :face font-lock-constant-face) ;; Feature
;; 	(md_color_lens :face font-lock-doc-face))) ;; Face

;; (setq company-box-icons-yasnippet 'fa_bookmark)

;; (setq company-box-icons-lsp
;;       '((1 . fa_text_height) ;; Text
;;         (2 . (fa_tags :face font-lock-function-name-face)) ;; Method
;;         (3 . (fa_tag :face font-lock-function-name-face)) ;; Function
;;         (4 . (fa_tag :face font-lock-function-name-face)) ;; Constructor
;;         (5 . (fa_cog :foreground "#FF9800")) ;; Field
;;         (6 . (fa_cog :foreground "#FF9800")) ;; Variable
;;         (7 . (fa_cube :foreground "#7C4DFF")) ;; Class
;;         (8 . (fa_cube :foreground "#7C4DFF")) ;; Interface
;;         (9 . (fa_cube :foreground "#7C4DFF")) ;; Module
;;         (10 . (fa_cog :foreground "#FF9800")) ;; Property
;;         (11 . md_settings_system_daydream) ;; Unit
;;         (12 . (fa_cog :foreground "#FF9800")) ;; Value
;;         (13 . (md_storage :face font-lock-type-face)) ;; Enum
;;         (14 . (md_closed_caption :foreground "#009688")) ;; Keyword
;;         (15 . md_closed_caption) ;; Snippet
;;         (16 . (md_color_lens :face font-lock-doc-face)) ;; Color
;;         (17 . fa_file_text_o) ;; File
;;         (18 . md_refresh) ;; Reference
;;         (19 . fa_folder_open) ;; Folder
;;         (20 . (md_closed_caption :foreground "#009688")) ;; EnumMember
;;         (21 . (fa_square :face font-lock-constant-face)) ;; Constant
;;         (22 . (fa_cube :face font-lock-type-face)) ;; Struct
;;         (23 . fa_calendar) ;; Event
;;         (24 . fa_square_o) ;; Operator
;;         (25 . fa_arrows)) ;; TypeParameter
;;       )

(use-package font-lock+)
(add-to-list 'load-path "~/.local/share/icons-in-terminal/")
(require 'icons-in-terminal)
