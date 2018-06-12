;;;; package --- Summary: Common Emacs config
;;;; Commentary:
;;;; Code:
;;;; BASICS

(use-package cl-lib)

(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;;; BACKUPS

(setf backup-by-copying t)

(setf backup-directory-alist
      `(("." . ,(expand-file-name "var/backups" user-emacs-directory))))

(setf delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(defconst emacs-tmp-dir
  (format "%s%s%s/" temporary-file-directory "emacs" (user-real-login-name)))

(setf auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))

(setf auto-save-list-file-prefix
      emacs-tmp-dir)

;;;; BUFFERS
(use-package ibuffer
  :ensure nil
  :requires (ibuf-ext)
  :config
  (setf ibuffer-formats
        '((mark modified read-only " "
		git-status-mini
		" "
                (name 30 30 :left :elide) ; change: 30s were originally 18s
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  (defun oni:ibuffer-mode-func ()
    "Function for `ibuffer-mode-hook'."
    (ibuffer-switch-to-saved-filter-groups "default"))
  (add-hook 'ibuffer-mode-hook 'oni:ibuffer-mode-func))

(defun find-file-in-config-dir ()
  (interactive)
  (counsel-find-file (expand-file-name "config/" user-emacs-directory)))

(defun find-file-in-sync-dir ()
  (interactive)
  (counsel-find-file "~/sync/org/"))

(defun next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setf i (1+ i)) (next-buffer) )))

(defun previous-user-buffer ()
  "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setf i (1+ i)) (previous-buffer) )))

(defun next-emacs-buffer ()
  "Switch to the next Emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setf i (1+ i)) (next-buffer) )))

(defun previous-emacs-buffer ()
  "Switch to the previous Emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setf i (1+ i)) (previous-buffer) )))

(defun new-empty-buffer ()
  "Opens a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setf buffer-offer-save t)))

(defun close-current-buffer ()
  "Close the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))


(use-package recentf
  :config
  (setf recentf-save-file (expand-file-name "var/recentf" user-emacs-directory))
  (setf recentf-auto-cleanup 60)
  (recentf-mode 1))

(setf temp-buffer-resize-mode t)


(use-package autorevert
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init
  (add-hook 'find-file-hook #'(lambda () (auto-revert-mode 1)))
  :config
  (setf auto-revert-verbose nil)
  (setf auto-revert-remote-files t))

(use-package ibuffer-git)
(use-package ibuffer-vc)
(use-package ibuffer-tramp)
(use-package ibuffer-projectile)

(use-package ace-jump-buffer)

(use-package broadcast
  :bind (("s-b" . broadcast-mode)))

;;;; COMPANY
(use-package company
  :bind (
	 ("M-<tab>" . company-complete-common-or-cycle)
	 :map company-active-map
	 ("M-k" . company-select-next)
	 ("M-i" . company-select-previous)
	 ("M-K" . company-next-page)
	 ("M-I" . company-previous-page)
	 ("C-j" . company-complete-common)
	 ("C-M-j" . company-complete-selection))
  :commands (company-mode global-company-mode)
  :init
  (defvar-local company-fci-mode-on-p nil)
  :config
  (global-company-mode t)
  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setf company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))

  (setf company-show-numbers t)
  (setf company-minimum-prefix-length 2)
  (setf company-idle-delay 0.1)
  (setf company-global-modes '(not gud-mode))

  (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))

(use-package company-quickhelp
  :bind (:map company-active-map
	      ("M-h" . company-quickhelp-manual-begin))
  :config
  (progn
    (setf company-quickhelp-delay nil)
    (company-quickhelp-mode 1)))

(use-package company-shell
  :config
  (add-to-list 'company-backends 'company-shell)
  (add-hook 'shell-mode-hook
	    (lambda () (setq-local company-backends
			      '((company-shell company-files))))))

;;;; NOTIFICATIONS

(use-package notify
  :ensure nil
  :load-path "lisp/")

(defun notify-send (title message)
  (shell-command (format "notify-send -u critical %s %s" title message)))

(use-package alert
  :commands alert
  :config
  (setf alert-default-style 'libnotify))

;; press "S" in a dired buffer to see dired sort in action
;; Provided by dired-sort
(use-package dired
  :straight nil
  :ensure nil
  :requires ( dired-sort)
  :bind (("C-d" . dired)
	 :map dired-mode-map
	 ("RET" . dired-find-alternate-file)
	 ("K" . dired-k)
	 ("g" . dired-k))
  :init (setq-default diredp-hide-details-initially-flag nil
                      dired-dwim-target t
                      ;;omit boring auto save files in dired views
                      dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$")
  :config ;; after loading dired, do this stuff
  (setf dired-recursive-deletes 'always
	dired-recursive-copies 'always
	dired-listing-switches "-alh")
  (defun mmm/dired-up-dir ()
    (interactive)
    (find-alternate-file ".."))
  (bind-key "^" 'mmm/dired-up-dir dired-mode-map))

(use-package diredfl
  :config
  (add-hook 'dired-mode-hook 'diredfl-mode))

(use-package dired-collapse)


;;;; Copy Cut Paste, Paste previous and toggle letter case
(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer
 (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer
 (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-12-30"
  (interactive)
  (let (p1 p2)
    (if current-prefix-arg
        (setf p1 (point-min) p2 (point-max))
      (if (use-region-p)
          (setf p1 (region-beginning) p2 (region-end))
        (setf p1 (line-beginning-position) p2 (line-end-position))))
    (if (eq last-command this-command)
        (progn
          ;; (push-mark (point) "NOMSG" "ACTIVATE")
          (kill-append "\n" nil)
          (forward-line 1)
          (end-of-line)
          (kill-append (buffer-substring-no-properties (line-beginning-position) (line-end-position)) nil)
          (message "Line copy appended"))
      (progn
        (kill-ring-save p1 p2)
        (if current-prefix-arg
            (message "Buffer text copied")
          (message "Text copied"))))))


(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles from 3 cases: UPPER CASE, lower case, Title Case,
in that cyclic order."
  (interactive)
  (let (pos1 pos2 (deactivate-mark nil) (case-fold-search nil))
    (if (and transient-mark-mode mark-active)
	(setf pos1 (region-beginning)
	      pos2 (region-end))
      (setf pos1 (car (bounds-of-thing-at-point 'word))
	    pos2 (cdr (bounds-of-thing-at-point 'word))))

    (when (not (eq last-command this-command))
      (save-excursion
	(goto-char pos1)
	(cond
	 ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
	 ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
	 ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
	 (t (put this-command 'state "all lower") ))))

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region pos1 pos2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region pos1 pos2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region pos1 pos2) (put this-command 'state "all lower")))))

;;;; UTF-8
(setf utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(setf locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

(when (display-graphic-p)
  (setf x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))


;;;; Clipboard
(setf select-enable-clipboard t
      select-enable-primary t)

;;;; turn on text selection highlighting and make typing override selected text
;;;; (Note: when delete-selection-mode is on, then transient-mark-mode is automatically on too.)
(delete-selection-mode 1)

(setf replace-character-fold t)

(use-package wgrep)
(use-package wgrep-ag)

(use-package hungry-delete
  :diminish hungry-delete-mode
  :init
  (global-hungry-delete-mode))

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
	 ("C-c C-m r" . mc/reverse-regions))
  :config
  (setf mc/list-file (expand-file-name "var/mc-all-or-once.el" user-emacs-directory)))

(use-package ace-mc)

(use-package easy-escape
  :diminish easy-escape-minor-mode
  :config
  (progn
    (add-hook 'lisp-mode-hook 'easy-escape-minor-mode)
    (add-hook 'emacs-lisp-mode-hook 'easy-escape-minor-mode)))

(use-package whole-line-or-region
  :diminish whole-line-or-region-local-mode
  :config
  (progn
    (whole-line-or-region-mode 1)))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
	 ("C-SPC" . er/mark-defun)))

(use-package comment-dwim-2
  :bind ("M-'" . comment-dwim-2))

(use-package ws-butler
  :diminish ws-butler-mode
  :config
  (progn
    (ws-butler-global-mode 1)))

(use-package edit-at-point
  :bind*
  (("C-c C-s a". edit-at-point-word-copy)
   ("C-c C-s b". edit-at-point-word-cut)
   ("C-c C-s d". edit-at-point-word-paste)
   ("C-c C-s e". edit-at-point-symbol-copy)
   ("C-c C-s f". edit-at-point-symbol-cut)
   ("C-c C-s h". edit-at-point-symbol-paste)
   ("C-c C-s i". edit-at-point-str-copy)
   ("C-c C-s j". edit-at-point-str-cut)
   ("C-c C-s l". edit-at-point-str-paste)
   ("C-c C-s m". edit-at-point-line-copy)
   ("C-c C-s n". edit-at-point-line-cut)
   ("C-c C-s p". edit-at-point-line-paste)
   ("C-c C-s q". edit-at-point-line-dup)
   ("C-c C-s r". edit-at-point-line-up)
   ("C-c C-s s". edit-at-point-line-down)
   ("C-c C-s t". edit-at-point-paren-copy)
   ("C-c C-s u". edit-at-point-paren-cut)
   ("C-c C-s w". edit-at-point-paren-paste)
   ("C-c C-s x". edit-at-point-paren-dup)
   ("C-c C-s y". edit-at-point-defun-copy)
   ("C-c C-s z". edit-at-point-defun-cut)
   ("C-c C-s )"  . edit-at-point-defun-paste)
   ("C-c C-s SPC" . edit-at-point-defun-dup)
   ("H-w" . edit-at-point-word-copy)
   ("H-W" . edit-at-point-word-cut)
   ("H-i" . edit-at-point-symbol-copy)
   ("H-I" . edit-at-point-symbol-cut)
   ("H-s" . edit-at-point-str-copy)
   ("H-S" . edit-at-point-str-cut)
   ("H-e" . edit-at-point-paren-copy)
   ("H-E" . edit-at-point-paren-cut)
   ("H-f" . edit-at-point-defun-copy)
   ("H-F" . edit-at-point-defun-cut)))

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

(use-package syntax-subword
  :ensure t
  :config
  (setf syntax-subword-skip-spaces t))

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


(defmacro hook-into-modes (function mode-hooks)
  "Add FUNCTION to hooks in MODE-HOOKS."
  `(dolist (hook ,mode-hooks)
     (add-hook hook ,function)))

;; Never kill, just bury
(defun dont-kill-but-bury-scratch ()
  "Don't kill but burry *scratch* buffer."
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn (bury-buffer) nil)
    t))

(defun set-bfr-to-8-unx ()
  (interactive)
  (set-buffer-file-coding-system
   'utf-8-unix))


(defun hlu-make-script-executable ()
  "If file starts with a shebang, make `buffer-file-name' executable"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (and (looking-at "^#!")
		 (not (file-executable-p buffer-file-name)))
	(set-file-modes buffer-file-name
			(logior (file-modes buffer-file-name) #o100))
	(message (concat "Made " buffer-file-name " executable"))))))

(defun crontab-e ()
  (interactive)
  (with-editor-async-shell-command "crontab -e"))

(defun init-duration-message ()
  "Print time spent in initialization to *Messages*."
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Initialization complete. (%.3fs)\n%s" elapsed (make-string 80 ?\-))))


;;;; Sudo stuff
(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))


(defun last-term-buffer (l)
  "Return most recently used term buffer from list L."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
	(car l) (last-term-buffer (cdr l)))))

(defun get-term ()
  "Switch to the term buffer last used, or create a new one if none exists, or if the current buffer is already a term."
  (interactive)
  (let ((b (last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
	(multi-term)
      (switch-to-buffer b))))

(defun int-to-binary-string (i)
  "Convert an integer I into it's binary representation in string format."
  (let ((res ""))
    (while (not (= i 0))
      (setf res (concat (if (= 1 (logand i 1)) "1" "0") res))
      (setf i (lsh i -1)))
    (if (string= res "")
        (setf res "0"))
    res))

(defvar xah-brackets nil "string of brackets")
(setf xah-brackets "()[]{}（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠⧘⧙⧚⧛⸜⸝⸌⸍⸂⸃⸄⸅⸉⸊᚛᚜༺༻༼༽⏜⏝⎴⎵⏞⏟⏠⏡﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸")

(defvar
  xah-left-brackets
  '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" )
  "List of left bracket chars.")
(progn
  (setf xah-left-brackets '())
  (dotimes (x (- (length xah-brackets) 1))
    ;; (message "%s" x)
    (when (= (% x 2) 0)
      (push (char-to-string (elt xah-brackets x))
            xah-left-brackets)))
  (setf xah-left-brackets (reverse xah-left-brackets)))

(defvar
  xah-right-brackets
  '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")
  "List of right bracket chars.")
(progn
  (setf xah-right-brackets '())
  (dotimes (x (- (length xah-brackets) 1))
    ;; (message "%s" x)
    (when (= (% x 2) 1)
      (push (char-to-string (elt xah-brackets x))
            xah-right-brackets)))
  (setf xah-right-brackets (reverse xah-right-brackets)))

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (search-backward-regexp (regexp-opt xah-left-brackets) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (search-forward-regexp (regexp-opt xah-right-brackets) nil t))

;; These commands let you move cursor around brackets. 〔➤see Matching Brackets in Unicode〕
;; You should assign them keys, such as {【Alt+←】, 【Alt+→】} or {F11, F12} or {Ctrl+7, Ctrl+8} or {↖ Home, ↘ End} . 〔➤see Emacs: How to Define Keys〕
;; When cursor is on a left bracket, call mark-sexp 【Ctrl+Alt+Space】 to select the whole.
;; Use this together with commands to select text inside bracket or quote. 〔➤see Emacs: Select Line, Block, in Quote, Extend Selection〕
;; Move Cursor to Quote
;; The following commands move cursor to ASCII quotes. Very convenient for programing.
(defun xah-forward-quote ()
  "Move cursor to the next occurrence of ' or \" or `.
If there are consecutive quotes of the same char, keep moving until none.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-26"
  (interactive)
  (if (search-forward-regexp "'+\\|`+\\|\\\"+" nil t)
      t
    (progn
      (message "No more quotes after.")
      nil)))

(defun xah-forward-quote-twice ()
  "Call `xah-forward-quote' twice.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-26"
  (interactive)
  (when (xah-forward-quote)
    (xah-forward-quote)))

(defun xah-backward-quote ()
  "Move cursor to the previous occurrence of '' or \" or `.
If there are consecutive quotes of the same char, keep moving until none.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-26"
  (interactive)
  (if (search-backward-regexp "'+\\|`+\\|\\\"+" nil t)
      (when (char-before) ; isn't nil, at beginning of buffer
        (while (char-equal (char-before) (char-after))
          (left-char)
          t))
    (progn
      (message "No more quotes before.")
      nil)))

(defmacro bol-with-prefix (function)
  "Define a new function which calls FUNCTION.
Except it moves to beginning of line before calling FUNCTION when
called with a prefix argument. The FUNCTION still receives the
prefix argument."
  (let ((name (intern (format "endless/%s-BOL" function))))
    `(progn
       (defun ,name (p)
         ,(format
           "Call `%s', but move to BOL when called with a prefix argument."
           function)
         (interactive "P")
         (when p
           (forward-line 0))
         (call-interactively ',function))
       ',name)))

(global-set-key [remap paredit-kill] (bol-with-prefix paredit-kill))
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))
(global-set-key [remap kill-line] (bol-with-prefix kill-line))
(global-set-key [remap lispy-kill] (bol-with-prefix lispy-kill))


(defun kill-line-backward ()
  "Kill text between the beginning of the line to the cursor position.
If there's no text, delete the previous line ending."
  (interactive)
  (if (looking-back "\n" nil)
      (delete-char -1)
    (kill-line 0)))

(defvar recently-closed-buffers (cons nil nil)
  "A list of recently closed buffers.
The max number to track is controlled by the variable recently-closed-buffers-max.")
(defvar recently-closed-buffers-max 10 "The maximum length for recently-closed-buffers.")

(defun close-current-buffer ()
  "Close the current buffer.

Similar to (kill-buffer (current-buffer)) with the following addition:

prompt user to save if the buffer has been modified even if the buffer is not associated with a
file.  Make sure the buffer shown after closing is a user buffer.  if the buffer is a file, add
the path to the list recently-closed-buffers.

A Emacs buffer is one who's name starts with *.  Else it is a user buffer."
  (interactive)
  (let (emacsBuff-p isEmacsBufferAfter)
    (if (string-match "^*" (buffer-name))
	(setf emacsBuff-p t)
      (setf emacsBuff-p nil))

    ;; offer to save buffers that are non-empty and modified, even for non-file visiting
    ;; buffer. (because kill-buffer does not offer to save buffers that are not associated with
    ;; files)
    (when (and (buffer-modified-p)
               (not emacsBuff-p)
               (not (string-equal major-mode "dired-mode"))
               (if (equal (buffer-file-name) nil)
                   (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                 t))
      (if (y-or-n-p
	   (concat "Buffer " (buffer-name) " modified; Do you want to save?"))
	  (save-buffer)
	(set-buffer-modified-p nil)))

    ;; save to a list of closed buffer
    (when (not (equal buffer-file-name nil))
      (setf recently-closed-buffers
            (cons (cons (buffer-name) (buffer-file-name)) recently-closed-buffers))
      (when (> (length recently-closed-buffers) recently-closed-buffers-max)
	(setf recently-closed-buffers (butlast recently-closed-buffers 1))))

    ;; close
    (kill-buffer (current-buffer))

    ;; if emacs buffer, switch to a user buffer
    (if (string-match "^*" (buffer-name))
	(setf isEmacsBufferAfter t)
      (setf isEmacsBufferAfter nil))
    (when isEmacsBufferAfter
      (previous-user-buffer))))

(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
	(beginning-of-line))))

;;;; Mark chat buffers as read
(defun endless/mark-read ()
  "Mark buffer as read up to current line."
  (let ((inhibit-read-only t))
    (put-text-property
     (point-min) (line-beginning-position)
     'face       'font-lock-comment-face)))

(defun endless/bury-buffer ()
  "Bury buffer and maybe close its window."
  (interactive)
  (endless/mark-read)
  (bury-buffer)
  (when (cdr (window-list nil 'nomini))
    (delete-window)))

(defmacro tellib-del-from-list (list-var element)
  "Delete ELEMENT from LIST-VAR."
  `(set ,list-var (delete ,element (eval ,list-var))))

;; (defun ivy-imenu-get-candidates-from (alist  &optional prefix)
;;   (cl-loop for elm in alist
;;            nconc (if (imenu--subalist-p elm)
;; 		     (ivy-imenu-get-candidates-from
;; 		      (cl-loop for (e . v) in (cdr elm) collect
;; 			       (cons e (if (integerp v) (copy-marker v) v)))
;; 		      (concat prefix (if prefix ".") (car elm)))
;;                    (and (cdr elm) ; bug in imenu, should not be needed.
;;                         (setcdr elm (copy-marker (cdr elm))) ; Same as [1].
;;                         (list (cons (concat prefix (if prefix ".") (car elm))
;;                                     (copy-marker (cdr elm))))))))
(defun ivy-imenu-goto ()
  "Go to buffer position"
  (interactive)
  (let ((imenu-auto-rescan t) items)
    (unless (featurep 'imenu)
      (require 'imenu nil t))
    (setf items (imenu--make-index-alist t))
    (ivy-read "imenu items:"
              (ivy-imenu-get-candidates-from (delete (assoc "*Rescan*" items) items))
              :action (lambda (k) (goto-char k)))))

(defun mmm/date-iso ()
  "Insert the current date, short format, eg. 2016-12-09."
  (interactive)
  (insert (format-time-string "%F")))

(defun mmm/date-iso-with-time ()
  "Insert the current date, short format, eg. 2016-12-09T14:34:54+0100."
  (interactive)
  (insert (format-time-string "%FT%T%z")))

(defun mmm/date-long ()
  "Insert the current date, short format, eg. December 09, 2016."
  (interactive)
  (insert (format-time-string "%B %d, %Y")))

(defun mmm/date-short ()
  "Insert the current date, short format, eg. 2016.12.09."
  (interactive)
  (insert (format-time-string "%Y.%m.%d")))

(defun mmm/date-short-with-time ()
  "Insert the current date, short format, eg. 2016.12.09 14:34"
  (interactive)
  (insert (format-time-string "%Y.%m.%d %H:%M")))

(defun me/eval-region-and-kill-mark (beg end)
  "Execute the region as Lisp code.
Call `eval-region' and kill mark. Move back to the beginning of the region."
  (interactive "r")
  (eval-region beg end)
  (setf deactivate-mark t)
  (goto-char beg))

(defun me/browse-url-and-kill-mark (url &rest args)
  "Ask a WWW browser to load URL.
Call `browse-url' and kill mark."
  (interactive (browse-url-interactive-arg "URL: "))
  (apply #'browse-url url args)
  (setf deactivate-mark t))

(defun me/indent-rigidly-left-and-keep-mark (beg end)
  "Indent all lines between BEG and END leftward by one space.
Call `indent-rigidly-left' and keep mark."
  (interactive "r")
  (indent-rigidly-left beg end)
  (setf deactivate-mark nil))

(defun me/indent-rigidly-right-and-keep-mark (beg end)
  "Indent all lines between BEG and END rightward by one space.
Call `indent-rigidly-right' and keep mark."
  (interactive "r")
  (indent-rigidly-right beg end)
  (setf deactivate-mark nil))

(defun me/indent-rigidly-left-tab-and-keep-mark (beg end)
  "Indent all lines between BEG and END leftward to a tab stop.
Call `indent-rigidly-left-to-tab-stop' and keep mark."
  (interactive "r")
  (indent-rigidly-left-to-tab-stop beg end)
  (setf deactivate-mark nil))

(defun me/indent-rigidly-right-tab-and-keep-mark (beg end)
  "Indent all lines between BEG and END rightward to a tab stop.
Call `indent-rigidly-right-to-tab-stop' and keep mark."
  (interactive "r")
  (indent-rigidly-right-to-tab-stop beg end)
  (setf deactivate-mark nil))

(defun me/sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun me/duplicate-line-down ()
  "Duplicate downward the line under point."
  (interactive)
  (kill-whole-line 0)
  (yank)
  (newline)
  (yank)
  (move-beginning-of-line 1))

(defun me/duplicate-line-up ()
  "Duplicate upward the line under point."
  (interactive)
  (kill-whole-line 0)
  (yank)
  (move-beginning-of-line 1)
  (yank)
  (newline)
  (move-beginning-of-line 0))

(defun me/swap-line-down ()
  "Move down the line under point."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode)
  (delete-trailing-whitespace))

(defun me/swap-line-up ()
  "Move up the line under point."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode)
  (delete-trailing-whitespace))

(defun endless/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setf bef (endless/simple-get-word))
		 ;; Word was corrected or used quit.
		 (if (ispell-word nil 'quiet)
		     nil ; End the loop.
		   ;; Also end if we reach `bob'.
		   (not (bobp)))
	       ;; If there's no word at point, keep looking
	       ;; until `bob'.
	       (not (bobp)))
	(backward-word)
	(backward-char))
      (setf aft (endless/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
	(let ((aft (downcase aft))
	      (bef (downcase bef)))
	  (define-abbrev
	    (if p local-abbrev-table global-abbrev-table)
	    bef aft)
	  (message "\"%s\" now expands to \"%s\" %sally"
		   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(defun endless/sharp ()
  "Insert #' unless in a string or comment."
  (interactive)
  (call-interactively #'self-insert-command)
  (let ((ppss (syntax-ppss)))
    (unless (or (elt ppss 3)
                (elt ppss 4)
                (eq (char-after) ?'))
      (insert "'"))))

(defun mmm/copy-file-name-to-clipboard (full-path)
  "Copy the current buffer file name to the clipboard."
  (interactive "P")
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (if full-path (buffer-file-name) (buffer-name)))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defmacro mmm/defshortcut (key file)
  `(set-register ,key (cons 'file ,file)))

(defun air--pop-to-file (file &optional split)
  "Visit a FILE, either in the current window or a SPLIT."
  (if split
      (find-file-other-window file)
    (find-file file)))


(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun mmm:uuid ()
  (interactive)
  (insert (s-replace "\n" "" (shell-command-to-string "uuid"))))

;;; END BINDINGS

(column-number-mode t)

;;; https://github.com/hlissner/doom-emacs/blob/master/core/autoload/minibuffer.el
(defun doom/minibuffer-kill-word ()
  "Kill a word, backwards, but only if the cursor is after
`minibuffer-prompt-end', to prevent the 'Text is read-only' warning from
monopolizing the minibuffer."
  (interactive)
  (when (> (point) (minibuffer-prompt-end))
    (call-interactively #'backward-kill-word)))

;;; https://github.com/hlissner/doom-emacs/blob/master/core/autoload/minibuffer.el
;;;###autoload
(defun doom/minibuffer-kill-line ()
  "Kill the entire line, but only if the cursor is after
`minibuffer-prompt-end', to prevent the 'Text is read-only' warning from
monopolizing the minibuffer."
  (interactive)
  (when (> (point) (minibuffer-prompt-end))
    (call-interactively #'backward-kill-sentence)))

;;; https://github.com/hlissner/doom-emacs/blob/master/core/autoload/minibuffer.el
;;;###autoload
(defun doom/minibuffer-undo ()
  "Undo an edit in the minibuffer without throwing errors."
  (interactive)
  (ignore-errors (call-interactively #'undo)))

(defun my-minibuffer-setup-hook ()
  (setf gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setf gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(auto-insert-mode)

(setf browse-url-browser-function 'browse-url-chromium)

(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'ed 'eval-defun)

(setf message-log-max 16384)

(setf enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)

(setf nsm-settings-file
      (expand-file-name "var/network-security.data/" user-emacs-directory))

;;;; Annoyances
(setf ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

;; show keystrokes
(setf echo-keystrokes 0.01)

(use-package executable
  :config
  (setf executable-prefix-env t) ;Use "#!/usr/bin/env python3" style magic number
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p))

(use-package counsel
  :bind* (("M-s" . counsel-grep-or-swiper)
          ("C-M-S" . counsel-rg)
          ("M-S" . mmm/counsel-rg)
          ("M-a" . counsel-M-x)
          ("C-h v" . counsel-describe-variable)
          ("C-h f" . counsel-describe-function)
          ("C-*" . counsel-descbinds)
          ("C-c C-e" . counsel-colors-emacs)
          ("C-o" . counsel-find-file)
          ("C-M-y" . counsel-yank-pop))
  :bind (("C-'" . counsel-imenu))
  :bind (:map counsel-mode-map
              ("M-k" . ivy-next-line)
              ("M-i" . ivy-previous-line)
              ("M-I" . ivy-scroll-down-command)
              ("M-K" . ivy-scroll-up-command))

  :config
  (defun mmm/counsel-rg (arg)
    "C-u prefix => No initial input, proj scope
   C-0 prefix => No initial input, CWD scope"
    (interactive "P")
    (if arg
        (cond ((and (numberp arg) (= arg 0))
               (setf current-prefix-arg nil)
               (counsel-projectile-rg))
              ((= (car arg) 4)
               (setf current-prefix-arg nil)
               (counsel-projectile-rg nil (projectile-project-root) nil
                                      (format "%s " (projectile-project-name))))
              (nil))
      (counsel-rg (symbol-name (symbol-at-point))
                  (projectile-project-root) nil
                  (format "%s " (projectile-project-name)))))
  (ivy-set-prompt 'counsel-projectile-rg #'counsel-prompt-function-dir)
  (setf counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never --ignore-file '/home/magnus/.ripgrep-ignore' '%s' %s")
  (setf counsel-rg-base-command
        "rg -S -M 120 --no-heading --line-number --color never --ignore-file '/home/magnus/.ripgrep-ignore' %s ."))

(use-package uniquify
  :straight nil
  :ensure nil
  :config
  (setf uniquify-buffer-name-style 'forward))

(use-package crux
  :bind (("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c C-d" . crux-duplicate-current-line-or-region)
         ("C-x M-r" . crux-rename-file-and-buffer)
         ("C-<return>" . crux-smart-open-line-above)
         ("C-c I" . crux-find-user-init-file)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("M-h" . crux-move-beginning-of-line))
  :init (require 'crux)
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify))

(use-package pass
  :config
  (defun password-store-contents (entry)
    "Return all contents of ENTRY.
Returns all contents of the password data as a list of strings,
one by line."
    (s-lines (password-store--run-show entry)))

  (defun mmmp/assword-store-url (entry)
    (let ((url (password-store-contents entry))
          (url_line nil))
      (while url
        (when (string-prefix-p "url: " (car url))
          (setf url_line (nth 1 (s-split " " (car url)))))
        (setf url (cdr url))
        )
      (if (or (string-prefix-p "http://" url_line)
              (string-prefix-p "https://" url_line)
              (string-prefix-p "www." url_line)
              )
          (browse-url url_line)
        (error "%s" "No url found or string does not look like a URL"))))

  (defun counsel-pass (&optional initial-input)
    (interactive)
    (ivy-read "pass: " 'password-store-list
              :initial-input initial-input
              :dynamic-collection t
              :history 'counsel-pass-history
              :action '(1
                        ("c" password-store-copy "Copy password to clipboard")
                        ("e" password-store-edit "Edit entry")
                        ("O" mmm/password-store-url "Browse url of entry")
                        ("o" (lambda (entry)
                               (let ((passwd (password-store-get entry)))
                                 (password-store-copy entry)
                                 (mmm/password-store-url entry))) "Copy to clipboard & browse url")))))

(use-package list-environment)

(use-package iregister)

(use-package cl-format
  :config
  (add-hook 'emacs-lisp-mode-hook 'cl-format-font-lock-mode))

(use-package pushover
  :config
  (setf pushover-user-key mmm/pushover-user-key)
  (setf pushover-api-key mmm/pushover-token))

(use-package smart-tab
  :diminish smart-tab-mode
  :config
  (global-smart-tab-mode 1))

(use-package clipmon
  :config
  (add-to-list 'after-init-hook 'clipmon-mode-start))

(use-package selected
  :init (selected-minor-mode)
  :bind (:map selected-keymap
              ("w" . er/expand-region)
              ("q" . selected-off)
              ("u" . upcase-region)
              ("d" . downcase-region)
              ("g" . google-this)
              ("m" . apply-macro-to-region-lines)))

(use-package subword
  :ensure nil
  :diminish subword-mode
  :config
  (global-subword-mode +1)
  (setf c-subword-mode t))

(use-package sx
  :config
  (setf sx-cache-directory (expand-file-name "var/stackoverflow" user-emacs-directory))
  (bind-keys :prefix "C-c s"
             :prefix-map my-sx-map
             :prefix-docstring "Global keymap for SX."
             ("q" . sx-tab-all-questions)
             ("i" . sx-inbox)
             ("o" . sx-open-link)
             ("u" . sx-tab-unanswered-my-tags)
             ("a" . sx-ask)
             ("s" . sx-search)))

(use-package direnv
  :config
  (direnv-mode))

(use-package guess-language
  :config
  (setf guess-language-languages '(en sv))
  (setf guess-language-min-paragraph-length 35)
  (defun my-guess-lang-mode-hook-function ()
    (guess-language-mode 1))
  (add-hook 'text-mode-hook #'my-guess-lang-mode-hook-function))

;;; setup-gitlab.el --- GitLab                       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Magnus Malm

;; Author: Magnus Malm <magnus@okinawa>
;; Keywords: vc

(use-package gitlab)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setf which-key-use-C-h-for-paging t)
  (setf which-key-prevent-C-h-from-cycling t)
  (setf which-key-max-display-columns 3)
  (setf which-key-max-description-length 50))

(use-package discover-my-major)

(use-package discover)

(use-package helpful)

(use-package man-commands)

(use-package latex-preview-pane)

;;; FUNCTIONS

(defun mmm/emacs-version ()
  (interactive)
  (concat
   (first (split-string (emacs-version) "(" t " "))
   " "
   (first (last (split-string (emacs-version) "of" t " ")))))

(defun dired-mware ()
  (interactive)
  (dired "/scp:mware:~/"))

(defun dired-bergsing ()
  (interactive)
  (dired "/scp:bergsing:~/"))

(defun dired-tinyhappybits ()
  (interactive)
  (dired "/notmuch:~/"))

(setf inferior-lisp-program "/usr/local/bin/sbcl")
(setf slime-contribs '(slime-fancy))

(setf inferior-lisp-program "ros -Q run")

(setf user-full-name "Magnus Malm")


;;;; Imenu
(defun my-shell-mode-setup-imenu ()
  (setf imenu-generic-expression (append '((nil "^\\([A-Z_]+\\)=.*" 1))
                                         (nthcdr 1 (car sh-imenu-generic-expression)))))
(add-hook 'sh-mode-hook 'my-shell-mode-setup-imenu)

(defun imenu-elisp-sections ()
  (add-to-list 'imenu-generic-expression '("Sections" "^;\\{1,4\\} \\(.+\\)$" 1) t))
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

(defun imenu-elisp-use-packages ()
  (add-to-list 'imenu-generic-expression '("Package" "^.*use-package \\(.+\\)$" 1) t))
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-use-packages)

(defun imenu-yml-node-name ()
  (add-to-list 'imenu-generic-expression '("Node" "^  - name: \\(.+\\)$" 1) t))
(add-hook 'yaml-mode-hook 'imenu-yml-node-name)

(defun recenter-no-redraw (&optional arg)
  "Like `recenter', but no redrawing."
  (interactive "P")
  (let ((recenter-redisplay nil))
    (recenter arg)))

(add-hook 'imenu-after-jump-hook 'recenter-no-redraw)

(use-package avy
  :config
  (setf avy-style 'pre)
  (setf avy-background t)
  (defun avy-goto-open-paren-followed-by-char-or-num (&optional arg)
    "Jump to an open bracket followed by a letter or a number.
 The window scope is determined by `avy-all-windows' (ARG negates it)."
    (interactive "P")
    (avy-with avy-goto-char
      (avy--generic-jump
       "([[:alnum:]]"
       arg
       avy-style))))

(use-package flimenu
  :config
  (progn
    (flimenu-global-mode)))

(use-package back-button
  :bind (("C-<right>" . back-button-local-forward)
	 ("C-<left>"  . back-button-local-backward))
  :diminish back-button-mode
  :config
  (back-button-mode 1))

(use-package link-hint
  :ensure t
  :bind
  ("C-c C-v o" . link-hint-open-link)
  ("C-c C-v c" . link-hint-copy-link)
  ("H-l" . link-hint-open-link)
  ("H-L" . link-hint-copy-link)

  :config
  (setf link-hint-avy-style 'pre)
  (setf link-hint-avy-background t))

;; Visual bookmarks (configured to survive Emacs restarts)
(use-package bm
  :demand t

  :init
  ;; where to store persistant files
  (setf bm-repository-file (concat user-emacs-directory "bm-dir"))

  ;; restore on load (even before you require bm)
  (setf bm-restore-repository-on-load t)

  :config
  ;; Allow cross-buffer 'next'
  (setf bm-cycle-all-buffers t)

  ;; save bookmarks
  (setq-default bm-buffer-persistence t)

  ;; Loading the repository from file when on start up.
  (add-hook' after-init-hook 'bm-repository-load)

  ;; Restoring bookmarks when on file find.
  (add-hook 'find-file-hooks 'bm-buffer-restore)

  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
				 (bm-buffer-save-all)
				 (bm-repository-save)))

  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

  :bind* (("C-<f2>" . bm-toggle)
	  ("<f2>" . bm-next)
	  ("S-<f2>" . bm-previous)
	  ("<left-fringe> <mouse-5>" . bm-next-mouse)
	  ("<left-fringe> <mouse-4>" . bm-previous-mouse)
	  ("<left-fringe> <mouse-1>" . bm-toggle-mouse)))

(use-package beginend
  :diminish beginend-global-mode
  :diminish beginend-prog-mode
  :config
  (beginend-global-mode))

(use-package saveplace
  :ensure nil
  :init (save-place-mode)
  :config
  (progn
    (setf save-place-file (concat user-emacs-directory "var/places"))))

(use-package smart-forward
  :bind (("M-<up>" . smart-up)
	 ("M-<down>" . smart-down)
	 ("M-<left>" . smart-backward)
	 ("M-<right>" . smart-forward)))

(setf znc-identifier "okinawa")

(use-package projectile
  ;; Space-p => π (see ~/.xmodmap)
  :bind (("H-p" . projectile-command-map))
  :config
  (setf projectile-enable-caching t)
  (setf projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
  (setf projectile-keymap-prefix (kbd "C-x p"))
  (setf projectile-switch-project-action
        #'projectile-commander)
  (setq projectile-project-root-files-top-down-recurring
        (append '("compile_commands.json"
                  ".cquery")
                projectile-project-root-files-top-down-recurring))
  (def-projectile-commander-method ?s
    "Open a *shell* buffer for the project."
    ;; This requires a snapshot version of Projectile.
    (projectile-run-shell))

  (def-projectile-commander-method ?c
    "Run `compile' in the project."
    (projectile-compile-project nil))
  (projectile-mode))

(use-package counsel-projectile
  :config
  (setf counsel-projectile-switch-project-action
	'(1 ("v" counsel-projectile-switch-project-action-vc "open project in vc-dir / magit / monky")
	    ("f" counsel-projectile-switch-project-action-find-file "jump to a project file")
	    ("d" counsel-projectile-switch-project-action-find-dir "jump to a project directory")
	    ("b" counsel-projectile-switch-project-action-switch-to-buffer "jump to a project buffer")
	    ("m" counsel-projectile-switch-project-action-find-file-manually "find file manually from project root")
	    ("S" counsel-projectile-switch-project-action-save-all-buffers "save all project buffers")
	    ("k" counsel-projectile-switch-project-action-kill-buffers "kill all project buffers")
	    ("K" counsel-projectile-switch-project-action-remove-known-project "remove project from known projects")
	    ("c" counsel-projectile-switch-project-action-compile "run project compilation command")
	    ("C" counsel-projectile-switch-project-action-configure "run project configure command")
	    ("E" counsel-projectile-switch-project-action-edit-dir-locals "edit project dir-locals")
	    ("o" counsel-projectile-switch-project-action "jump to a project buffer or file")
	    ("sg" counsel-projectile-switch-project-action-grep "search project with grep")
	    ("ss" counsel-projectile-switch-project-action-ag "search project with ag")
	    ("sr" counsel-projectile-switch-project-action-rg "search project with rg")
	    ("xs" counsel-projectile-switch-project-action-run-shell "invoke shell from project root")
	    ("xe" counsel-projectile-switch-project-action-run-eshell "invoke eshell from project root")
	    ("xt" counsel-projectile-switch-project-action-run-term "invoke term from project root")
	    ("O" counsel-projectile-switch-project-action-org-capture "org-capture into project")))
  (counsel-projectile-mode))

(use-package tramp
  :ensure nil
  :config
  (setf tramp-persistency-file-name
	(expand-file-name "var/tramp-history.el" user-emacs-directory))

  (setf tramp-completion-reread-directory-timeout nil)
  (setf vc-ignore-dir-regexp
	(format "\\(%s\\)\\|\\(%s\\)"
		vc-ignore-dir-regexp
		tramp-file-name-regexp))
  (setf tramp-verbose 1)
  (setf tramp-default-method "ssh")
  (setf password-cache-expiry (* 60 60 8)))

(setf search-default-mode 'character-fold-to-regexp)

(use-package swiper
  :bind (("C-c u" . swiper-all)
	 :map  ivy-minibuffer-map
	 ("C-7" . swiper-mc)
	 :map swiper-map
	 ("M-I" . ivy-scroll-down-command)
	 ("M-K" . ivy-scroll-up-command)
         ("M-." . insert-symbol-at-point)
         ("M-," . insert-word-at-point))
  :config
  (defun insert-symbol-at-point ()
    (interactive)
    (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'symbol)))))

  (defun insert-word-at-point ()
    (interactive)
    (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'word)))))
  ;; (setf ivy-use-virtual-buffers nil)
  (setf swiper-include-line-number-in-search t))

(use-package prescient
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode))

(use-package company-prescient
  :config
  (company-prescient-mode))

(use-package winner
  :ensure nil
  :config
  (winner-mode))

(use-package ag
  :config
  (setf ag-highlight-search t))

(use-package google-this
  :bind (:map google-this-mode-submap
	      ("C-x g" . google-this-mode-submap))
  :diminish google-this-mode
  :config
  (google-this-mode 1))

(use-package anzu
  :bind (("M-%" . anzu-query-replace)
	 ("C-M-%" . anzu-query-replace-regexp))
  :init (global-anzu-mode +1)
  :diminish anzu-mode)

(use-package phi-search
  :bind ("π" . phi-search))

(use-package ivy
  :bind* (("M-m" . ivy-switch-buffer)
  	  ("C-c C-r" . ivy-resume))
  :bind (:map ivy-minibuffer-map
	      ("C-'" . ivy-avy)
	      ("M-k" . ivy-next-line)
	      ("M-i" . ivy-previous-line)
	      ("M-I" . ivy-scroll-down-command)
	      ("M-K" . ivy-scroll-up-command)
	      ("M-p" . ivy-previous-history-element)
	      ("M-n" . ivy-next-history-element)
	      ("M-v" . yank)
	      ("M-j" . backward-char)
	      ("M-l" . forward-char)
	      ("M-u" . backward-word)
	      ("M-o" . forward-word)
	      ("M-e" . backward-kill-word)
	      ("M-r" . kill-word)
	      ("M-x" . ivy-kill-line)
	      ("M-O" . ivy-dispatching-done)
	      ("C-M-O" . ivy-dispatching-call)
	      ("C-M-j" . ivy-immediate-done))
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  :config

  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setf ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setf ivy-height 15)
  ;; count candidates
  (setf ivy-count-format "%-4d ")
  ;; no regexp by default
  (setf ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setf ivy-re-builders-alist
	'((ivy-switch-buffer . ivy--regex-ignore-order)
	  (t . ivy--regex-ignore-order)))
  (setf ivy-extra-directories nil)
  (ivy-set-actions
   'counsel-find-file
   `(("x"
      (lambda (x) (delete-file (expand-file-name x ivy--directory)))
      ,(propertize "delete" 'face 'font-lock-warning-face))))
  (defun ivy-yank-action (x)
    (kill-new x))

  (defun ivy-copy-to-buffer-action (x)
    (with-ivy-window
      (insert x)))

  (ivy-set-actions
   t
   '(("i" ivy-copy-to-buffer-action "insert")
     ("y" ivy-yank-action "yank")))
  (setf resize-mini-windows t))

(use-package ivy-buffer-extend
  :straight nil
  :ensure nil
  :load-path "lisp/")

(setf shell-file-name "bash")
(add-to-list 'exec-path "/usr/local/bin")

(defun setup-eshell ()
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
  (define-key eshell-mode-map "C-a" 'eshell-maybe-bol))

(add-hook 'eshell-mode-hook 'setup-eshell)


(use-package multi-term
  :init
  (setf multi-term-dedicated-select-after-open-p t)
  (setf multi-term-scroll-to-bottom-on-output 'this)
  :config
  (setf multi-term-program "/bin/bash"))

(use-package multi-term)

(use-package pcmpl-args)
(use-package pcmpl-git)

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
         (setf ispell-personal-dictionary
	       (expand-file-name "dict/" user-emacs-directory))))

(use-package abbrev
  :straight nil
  :ensure nil
  :diminish abbrev-mode
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  ;;;; ispell + abbrev = cool auto-complete
  (setf abbrev-file-name
	(expand-file-name "personal_abbrev.txt" user-emacs-directory))
  (setf save-abbrevs t)

  (setf save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package auto-dictionary
  :config
  (add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))

(use-package flyspell-correct-popup)

;; ====================
;; insert date and time

(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%H:%M"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert "==========\n")
					;       (insert (let () (comment-start)))
  (insert (format-time-string current-date-time-format (current-time)))
  (insert "\n"))

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time)))
  (insert "\n"))

;; TODO
;; (global-set-key "\C-c\C-d" 'insert-current-date-time)
;; (global-set-key "\C-c\C-t" 'insert-current-time)


(blink-cursor-mode 0)

(setq-default cursor-in-non-selected-windows nil)

(global-prettify-symbols-mode)
(setf prettify-symbols-unprettify-at-point 'right-edge)

;;;; UI
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-frame-font "DejaVu Sans Mono-10" nil t)
  (set-fringe-style '(9 . 9)))

(setf default-frame-alist '((font . "DejaVu Sans Mono-10")
			    (cursor-type . box)))
(add-to-list 'default-frame-alist '(mouse-color . "DarkOrange"))
(add-to-list 'default-frame-alist '(cursor-color . "green"))

(setf frame-title-format
      '("emacs%@" (:eval (system-name)) ": " (:eval (if (buffer-file-name)
							(abbreviate-file-name (buffer-file-name))
						      "%b")) " [%*]"))
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

;;;; *scratch* buffer
(setf initial-scratch-message nil)
(setf initial-major-mode 'text-mode)

;;;; Startup stuff
(setf inhibit-startup-message t)
(setf inhibit-splash-screen t)
(setf initial-scratch-message nil)
(setf initial-buffer-choice "~/")

(setf scroll-step            1
      scroll-conservatively  10000)

;;;; Misc
(setf show-paren-delay 0)
(show-paren-mode t)
(setf show-paren-style 'expression)

(defun modi/is-font (fontname)
  "Return non-nil if the default font match FONTNAME."
  ;; http://superuser.com/a/1100378/209371
  (string-match-p fontname (format "%s" (face-attribute 'default :font))))

(with-eval-after-load 'setup-font-check
  (defvar default-font-size-pt
    (cond
     ((modi/is-font "Monoid") 11)
     ((modi/is-font "Pragmata") 13)
     (t 12))
    "Default font size in points."))

(defun modi/global-font-size-adj (scale &optional absolute)
  "Adjust the font sizes globally: in all buffers, mode line, echo area, etc.

M-<SCALE> COMMAND increases font size by SCALE points if SCALE is +ve,
                  decreases font size by SCALE points if SCALE is -ve
                  resets    font size if SCALE is 0.

If ABSOLUTE is non-nil, text scale is applied relative to the default font size
`default-font-size-pt'.
 Else, the text scale is applied relative to the current font size."
  (interactive "p")
  (if (= scale 0)
      (setf font-size-pt default-font-size-pt)
    (if (bound-and-true-p absolute)
        (setf font-size-pt (+ default-font-size-pt scale))
      (setf font-size-pt (+ font-size-pt scale))))
  ;; The internal font size value is 10x the font size in points unit.
  ;; So a 10pt font size is equal to 100 in internal font size value.
  (set-face-attribute 'default nil :height (* font-size-pt 10)))

(defun modi/global-font-size-incr ()  (interactive) (modi/global-font-size-adj +1))
(defun modi/global-font-size-decr ()  (interactive) (modi/global-font-size-adj -1))
(defun modi/global-font-size-reset () (interactive) (modi/global-font-size-adj 0))


;; turn on highlighting current line
(global-hl-line-mode 1)
(make-variable-buffer-local 'global-hl-line-mode)

(use-package ansi-color
  :config
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setf markdown-command "multimarkdown"))

(use-package fontawesome
  :config
  (defun insert-fontawesome ()
    (interactive)
    (insert (call-interactively 'fontawesome))))

(use-package zenburn-theme)

(load-theme 'zenburn t)

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

(use-package whitespace
  :diminish whitespace-mode
  :diminish global-whitespace-mode
  :init
  (progn
    (defun no-trailing-whitespace ()
      (setf show-trailing-whitespace nil)))
  :config
  (progn
    (setq-default indicate-empty-lines t) ; in the left fringe
    (setf require-final-newline t)
    (setf whitespace-style '(face trailing))
    (add-hook 'prog-mode-hook #'whitespace-mode)

    (global-whitespace-mode t)
    (add-hook 'minibuffer-setup-hook
	      'no-trailing-whitespace)
    (add-hook 'eww-mode-hook
	      'no-trailing-whitespace)
    (add-hook 'ielm-mode-hook
	      'no-trailing-whitespace)
    (add-hook 'gdb-mode-hook
	      'no-trailing-whitespace)
    (add-hook 'help-mode-hook
	      'no-trailing-whitespace)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package linum-relative)

(use-package beacon
  :init (beacon-mode 1)
  :config
  (progn
    (setf beacon-color "#cccec4")
    ;; Don't blink on specific major modes
    (add-to-list 'beacon-dont-blink-major-modes 'shell-mode)
    (add-to-list 'beacon-dont-blink-major-modes 'eshell-mode)
    ;; Don't blink on next-line/previous-line at the top/bottom of the window
    (add-to-list 'beacon-dont-blink-commands 'next-line)
    (add-to-list 'beacon-dont-blink-commands 'previous-line))
  :diminish beacon-mode)

(use-package highlight-symbol
  :bind (("C-<f3>" . highlight-symbol)
	 ("<f3>" . highlight-symbol-next)
	 ("S-<f3>" . highlight-symbol-prev)
	 ("M-<f3>" . highlight-symbol-query-replace)))

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

(use-package all-the-icons)

(use-package all-the-icons-dired
  :diminish all-the-icons-dired-mode
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package git-gutter
  :bind(
	("H-g C-g" . 'git-gutter)
	("H-g v" . 'git-gutter:popup-hunk)

	;; Jump to next/previous hunk
	("H-g p" . 'git-gutter:previous-hunk)
	("H-g n" . 'git-gutter:next-hunk)

	;; Stage current hunk
	("H-g s" . 'git-gutter:stage-hunk)

	;; Revert current hunk
	("H-g r" . 'git-gutter:revert-hunk)

	;; Mark current hunk
	("H-g SPC" . #'git-gutter:mark-hunk))
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode +1))

(use-package leerzeichen)

(use-package zoom
  :config
  (zoom-mode t)
  (defun size-callback ()
    (cond ((> (frame-pixel-width) 1280) '(90 . 0.75))
          (t                            '(0.618 . 0.618))))
  (setf zoom-ignored-major-modes '(dired-mode markdown-mode ediff-mode magit-popup-mode))
  (setf zoom-size 'size-callback))

(use-package xkcd)

(use-package color-identifiers-mode
  :disabled
  :config
  (add-hook 'after-init-hook 'global-color-identifiers-mode))

(use-package yafolding)

(use-package origami
  :bind (("C-<return>" . 'origami-recursively-toggle-node))
  :config
  (global-origami-mode))

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
  (setq-default rainbow-identifiers-cie-l*a*b*-lightness 90))


(setf resize-mini-windows t)

(use-package windmove
  :bind* (("s-i" . windmove-up)
	  ("s-k" . windmove-down)
	  ("s-j" . windmove-left)
	  ("s-l" . windmove-right)))

(use-package switch-window
  :config
  (setf switch-window-shortcut-style 'qwerty)
  (setf switch-window-minibuffer-shortcut (string-to-char "m")))

(use-package windsize)

(use-package zygospore
  :bind* ("M-1" . zygospore-toggle-delete-other-windows))

(use-package ace-window
  :bind (("M-b" . ace-window)
	 ("C-M-S-k" . ace-swap-window)
	 ("C-M-S-o" . ace-window))
  :config
  (ace-window-display-mode 1)
  (defvar aw-dispatch-alist
    '((?m aw-swap-window " Ace - Swap Window")
      (?n aw-flip-window)
      (?v aw-split-window-vert " Ace - Split Vert Window")
      (?b aw-split-window-horz " Ace - Split Horz Window")
      (?j aw-switch-buffer-in-window " Ace - Select Buffer"))
    "List of actions for `aw-dispatch-default'.")
  (defun aw-switch-buffer-in-window (window)
    "Select buffer in WINDOW."
    (aw-switch-to-window window)
    (call-interactively 'switch-to-buffer))
  (setf aw-dispatch-always nil)
  (setf aw-scope 'frame)
  (setf aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setf aw-leading-char-style 'path))

(defun dired-wrdc155-ftp ()
  (interactive)
  (dired "/ssh:magma@ftp.labs.westermo.se:/srv/ftp/share/malm/"))


(defun package-menu-find-marks ()
  "Find packages marked for action in *Packages*."
  (interactive)
  (occur "^[A-Z]"))

;; Only in Emacs 25.1+
(defun package-menu-filter-by-status (status)
  "Filter the *Packages* buffer by status."
  (interactive
   (list (completing-read
	  "Status: " '("new" "installed" "dependency" "obsolete"))))
  (package-menu-filter (concat "status:" status)))

(define-key package-menu-mode-map "s" #'package-menu-filter-by-status)
(define-key package-menu-mode-map "a" #'package-menu-find-marks)

(use-package atomic-chrome
  :custom
  (atomic-chrome-url-major-mode-alist
   '(("reddit\\.com" . markdown-mode)
     ("github\\.com" . gfm-mode)
     ("redmine" . textile-mode))
   "Major modes for URLs.")
  :config
  (atomic-chrome-start-server))

(use-package mpdel)

(use-package super-save
  :config
  (setf super-save-auto-save-when-idle t)
  (setf super-save-idle-duration 3)
  (super-save-mode +1))

(setf next-error-highlight 3)

(use-package emojify)

(define-minor-mode minor-mode-blackout-mode
  "Hides minor modes from the mode line."
  t)

(catch 'done
  (mapc (lambda (x)
	  (when (and (consp x)
		     (equal (cadr x) '("" minor-mode-alist)))
	    (let ((original (copy-sequence x)))
	      (setcar x 'minor-mode-blackout-mode)
	      (setcdr x (list "" original)))
	    (throw 'done t)))
	mode-line-modes))
(global-set-key (kbd "C-c C-m") 'minor-mode-blackout-mode)

(use-package undo-tree
  :bind (("M-Z" . undo-tree-redo)
	 ("M-z" . undo-tree-undo))
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package latex-preview-pane)

(if (server-is "mail")
    (progn
      (message "Loading MAIL stuff...")

      (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

      (require 'mu4e)
      (require 'smtpmail)
      (require 'gnus-dired)

      (setf mu4e-maildir "~/.mail/gmail")

      ;; Directory to save attachments
      (setf mu4e-attachment-dir "~/attachments/")
      ;; List of mail accounts.
      (defvar my-mu4e-account-alist nil)
      ;; List of email signatures that can be added to a mail.
      (defvar mu4e-mail-sigs nil)
      ;; Shortcuts to often visited mailboxes
      (setf mu4e-maildir-shortcuts nil)
      ;; Command used to get mail (offlineimap, mbsync, etc)
      (setf mu4e-get-mail-command nil)
      ;; If non-nil _and_ mu4e is running, get mail in the background periodically
      ;; Value in minutes
      (setf mu4e-update-interval nil)


      (setf mu4e-use-fancy-chars t)
      (setf mu4e-headers-date-format "%Y-%m-%d %H:%M")
      (setf mu4e-headers-skip-duplicates t)
      (setf mu4e-headers-include-related t)
      (setf mu4e-view-date-format "%a %Y-%m-%d %H:%M")
      (setf gnus-dired-mail-mode 'mu4e-user-agent)
      (setf mu4e-change-filenames-when-moving t)
      ;; show full addresses in view message (instead of just names)
      ;; toggle per name with M-RET
      (setf mu4e-view-show-addresses t)

      (setf message-send-mail-function 'smtpmail-send-it)
      (setf starttls-use-gnutls t)
      (setf smtpmail-debug-info t)

      ;; when you want to use some external command for html->text
      ;; conversion, e.g. the  html2text  program
      ;; (cpbotha: html2text sees to work better than the built-in one)
      (setf mu4e-html2text-command "html2text")

      ;; mu4e-action-view-in-browser is built into mu4e
      ;; by adding it to these lists of custom actions
      ;; it can be invoked by first pressing a, then selecting
      (add-to-list 'mu4e-headers-actions
		   '("in browser" . mu4e-action-view-in-browser) t)
      (add-to-list 'mu4e-view-actions
		   '("in browser" . mu4e-action-view-in-browser) t)

      ;; the headers to show in the headers list   a pair of a field
      ;; and its width, with `nil  meaning  unlimited
      ;; (better only use that for the last field.
      ;; These are the defaults:
      (setf mu4e-headers-fields
	    '( (:date . 20)
	       (:maildir . 20)
	       (:flags . 6)
	       (:from-or-to . 22)
	       (:subject . nil)))

      (use-package mu4e-maildirs-extension
	:ensure t)

      (mu4e-maildirs-extension)

      ;; enable inline images
      (setf mu4e-view-show-images t)

      ;; use imagemagick, if available
      (when (fboundp 'imagemagick-register-types)
	(imagemagick-register-types))

      (add-hook 'mu4e-compose-mode-hook
		(defun my-do-compose-stuff ()
		  "My settings for message composition."
		  (set-fill-column 72)
		  (flyspell-mode)))

      (setf mu4e-compose-signature-auto-include nil)

      (defun my-render-html-message ()
	(let ((dom (libxml-parse-html-region (point-min) (point-max))))
	  (erase-buffer)
	  (shr-insert-document dom)
	  (goto-char (point-min))))

      (setf mu4e-html2text-command 'my-render-html-message)
      ;; make the `gnus-dired-mail-buffers' function also work on
      ;; message-mode derived modes, such as mu4e-compose-mode
      (defun gnus-dired-mail-buffers ()
	"Return a list of active message buffers."
	(let (buffers)
	  (save-current-buffer
	    (dolist (buffer (buffer-list t))
	      (set-buffer buffer)
	      (when (and (derived-mode-p 'message-mode)
			 (null message-sent-message-via))
		(push (buffer-name buffer) buffers))))
	  (nreverse buffers)))
      (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

      (defun my-mu4e-set-account ()
	"Set the account for composing a message."
	(let* ((account
		(if mu4e-compose-parent-message
		    (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
		      (string-match "/\\(.*?\\)/" maildir)
		      (match-string 1 maildir))
		  (completing-read (format "Compose with account: (%s) "
					   (mapconcat #'(lambda (var) (car var))
						      my-mu4e-account-alist "/"))
				   (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
				   nil t nil nil (caar my-mu4e-account-alist))))
	       (account-vars (cdr (assoc account my-mu4e-account-alist))))
	  (message "Account set to %s" account)
	  (if account-vars
	      (mapc #'(lambda (var)
			(set (car var) (cadr var)))
		    account-vars)
	    (error "No email account found"))))
      (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

      (defun my-mu4e-choose-signature ()
	"Insert one of a number of sigs"
	(interactive)
	(let ((message-signature
	       (mu4e-read-option "Signature:"
				 mu4e-mail-sigs)))
	  (message-insert-signature)))
      (add-hook 'mu4e-compose-mode-hook
		(lambda () (local-set-key (kbd "C-c C-w") #'my-mu4e-choose-signature)))

      ;; works only for emacs with xwidget support
      (defun my-mu4e-action-view-with-xwidget (msg)
	"View the body of the message inside xwidget-webkit."
	(unless (fboundp 'xwidget-webkit-browse-url)
	  (mu4e-error "No xwidget support available"))
	(let* ((html (mu4e-message-field msg :body-html))
	       (txt (mu4e-message-field msg :body-txt))
	       (tmpfile (format "%s%x.html" temporary-file-directory (random t))))
	  (unless (or html txt)
	    (mu4e-error "No body part for this message"))
	  (with-temp-buffer
	    ;; simplistic -- but note that it's only an example...
	    (insert (or html (concat "<pre>" txt "</pre>")))
	    (write-file tmpfile)
	    (xwidget-webkit-browse-url (concat "file://" tmpfile) t))))

      (add-to-list 'mu4e-view-actions
		   '("xViewXWidget" . my-mu4e-action-view-with-xwidget) t)
      (message "Loading MAIL stuff done!")))

(if (server-is "irc")
    (progn
      (message "Loading IRC stuff...")

      (require 'erc)
      (require 'erc-match)
      (require 'erc-imenu)

      (add-to-list 'erc-modules 'log)
      (add-to-list 'erc-modules 'notifications)
      (setf erc-modules
	    '(log notifications fill move-to-prompt stamp spelling hl-nicks netsplit fill
		  button match track completion readonly networks ring
		  autojoin noncommands irccontrols move-to-prompt stamp menu list))

      (erc-spelling-mode 1)

      (setf erc-prompt  (lambda () (concat (buffer-name) "> ")))

      (add-hook 'erc-mode-hook 'flyspell-mode) ;start flyspell-mode
      (setf ispell-dictionary "svenska")    ;set the default dictionary

      (setf erc-save-buffer-on-part t)
      (setf erc-log-channels-directory "~/.erc/logs/")

      (defface erc-header-line-disconnected
	'((t (:inherit magit-diff-removed)))
	"Face to use when ERC has been disconnected.")

      (defun erc-update-header-line-show-disconnected ()
	"Use a different face in the header-line when disconnected."
	(erc-with-server-buffer
	 (cond ((erc-server-process-alive) 'erc-header-line)
	       (t 'erc-header-line-disconnected))))

      (setf erc-header-line-face-method 'erc-update-header-line-show-disconnected)
      (setf erc-hide-list '("JOIN" "PART" "QUIT"))

      (require 'erc-input-fill)

      (setf erc-timestamp-only-if-changed-flag nil
	    erc-timestamp-format "%H:%M "
	    erc-fill-prefix "    | "
	    erc-insert-timestamp-function 'erc-insert-timestamp-left)

      (setf erc-auto-query 'buffer)

      (use-package erc-hl-nicks)

      (use-package erc-colorize
	:ensure t
	:config
	(erc-colorize-mode 1))


      (eval-after-load 'erc-track
	'(progn
	   (defun erc-bar-move-back (n)
	     "Moves back n message lines. Ignores wrapping, and server messages."
	     (interactive "nHow many lines ? ")
	     (re-search-backward "^.*<.*>" nil t n))

	   (defun erc-bar-update-overlay ()
	     "Update the overlay for current buffer, based on the content of
erc-modified-channels-alist. Should be executed on window change."
	     (interactive)
	     (let* ((info (assq (current-buffer) erc-modified-channels-alist))
		    (count (cadr info)))
	       (if (and info (> count erc-bar-threshold))
		   (save-excursion
		     (end-of-buffer)
		     (when (erc-bar-move-back count)
		       (let ((inhibit-field-text-motion t))
			 (move-overlay erc-bar-overlay
				       (line-beginning-position)
				       (line-end-position)
				       (current-buffer)))))
		 (delete-overlay erc-bar-overlay))))

	   (defvar erc-bar-threshold 1
	     "Display bar when there are more than erc-bar-threshold unread messages.")
	   (defvar erc-bar-overlay nil
	     "Overlay used to set bar")
	   (setf erc-bar-overlay (make-overlay 0 0))
	   (overlay-put erc-bar-overlay 'face '(:underline "black"))
	   ;;put the hook before erc-modified-channels-update
	   (defadvice erc-track-mode (after erc-bar-setup-hook
					    (&rest args) activate)
	     ;;remove and add, so we know it's in the first place
	     (remove-hook 'window-configuration-change-hook 'erc-bar-update-overlay)
	     (add-hook 'window-configuration-change-hook 'erc-bar-update-overlay))
	   (add-hook 'erc-send-completed-hook (lambda (str)
						(erc-bar-update-overlay)))))

      (defun fit-window-to-buffer-width (&optional window max-width min-width)
	"Fit WINDOW according to its buffer's width.
WINDOW, MAX-WIDTH and MIN-WIDTH have the same meaning as in
`fit-window-to-buffer'."
	(interactive)
	(let ((fit-window-to-buffer-horizontally 'only))
	  (fit-window-to-buffer window nil nil max-width min-width)))
      (message "Loading IRC stuff done!")))

(if (server-is "devel")
    (progn
      (message "Loading DEVEL stuff...")
      (use-package magit
	:bind (("C-c m" . magit-status)
	       ("C-c l" . magit-log-buffer-file)
	       ("C-c M-g" . magit-dispatch-popup))
	:config
	(setenv "GIT_PAGER" "")
	(setf magit-commit-arguments (quote ("--signoff")))
	(setf magit-set-upstream-on-push t)
	(setf magit-revert-buffers 1)
	(setf magit-log-show-refname-after-summary t)
	(setf magit-completing-read-function 'ivy-completing-read)
	(setf magit-use-sticky-arguments nil)
	(setf magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

	(setf magit-status-margin '(nil age magit-log-margin-width nil 18))

	(setf magit-status-headers-hook
	      '(magit-insert-error-header
		magit-insert-repo-header
		magit-insert-remote-header
		magit-insert-diff-filter-header
		magit-insert-head-branch-header
		magit-insert-upstream-branch-header
		magit-insert-push-branch-header
		magit-insert-tags-header))

	(setf magit-status-sections-hook
	      '(magit-insert-status-headers
		magit-insert-merge-log
		magit-insert-rebase-sequence
		magit-insert-am-sequence
		magit-insert-sequencer-sequence
		magit-insert-bisect-output
		magit-insert-bisect-rest
		magit-insert-bisect-log
		magit-insert-untracked-files
		magit-insert-unstaged-changes
		magit-insert-staged-changes
		magit-insert-stashes
		magit-insert-unpulled-from-upstream
		magit-insert-unpulled-from-pushremote
		magit-insert-unpushed-to-upstream
		magit-insert-unpushed-to-pushremote
		magit-insert-modules-unpulled-from-upstream
		magit-insert-modules-unpulled-from-pushremote
		magit-insert-modules-unpushed-to-upstream-or-recent
		magit-insert-modules-unpushed-to-pushremote))

	(magit-define-popup-switch 'magit-log-popup
	  ?m "Omit merge commits" "--no-merges")

	(magit-define-popup-switch
	  'magit-log-popup
	  ?1 "First parent" "--first-parent")

	(autoload 'org-read-date "org")

	(defun magit-org-read-date (prompt &optional _default)
	  (org-read-date 'with-time nil nil prompt))

	(magit-define-popup-option 'magit-log-popup
	  ?s "Since date" "--since=" #'magit-org-read-date)

	(magit-define-popup-option 'magit-log-popup
	  ?u "Until date" "--until=" #'magit-org-read-date)

	(add-hook 'after-save-hook 'magit-after-save-refresh-status)
	(setf magit-save-repository-buffers 'dontask))

      (use-package magit-lfs
	:ensure t)

      (use-package magit-gh-pulls
	:ensure t
	:config
	(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

      (use-package magithub
	:after magit
	:config (magithub-feature-autoinject t))

      (use-package git-timemachine
	:straight (:host github
			 :repo "emacsmirror/git-timemachine"
			 :branch "master")

	;; https://github.com/emacsmirror/git-timemachine
	:bind (("C-x M-t" . git-timemachine)
	       ("C-x M-T" . my-git-timemachine))
	:config
	;; http://blog.binchen.org/posts/enhance-emacs-git-gutter-with-ivy-mode.html
	(defun my-git-timemachine-show-selected-revision ()
	  "Show last (current) revision of file."
	  (interactive)
	  (let* ((collection (mapcar (lambda (rev)
				       ;; re-shape list for the ivy-read
				       (cons (concat (substring-no-properties (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
				     (git-timemachine--revisions))))
	    (ivy-read "commits:"
		      collection
		      :action (lambda (rev)
				;; compatible with ivy 9+ and ivy 8
				(unless (string-match-p "^[a-z0-9]*$" (car rev))
				  (setf rev (cdr rev)))
				(git-timemachine-show-revision rev)))))

	(defun my-git-timemachine ()
	  "Open git snapshot with the selected version.  Based on ivy-mode."
	  (interactive)
	  (unless (featurep 'git-timemachine)
	    (require 'git-timemachine))
	  (git-timemachine--start #'my-git-timemachine-show-selected-revision)))

      (use-package magit-imerge)

      (use-package cc-mode)

      (use-package lsp-mode
	:config
	;; (lsp-define-stdio-client lsp-python "python"
	;; 			 #'projectile-project-root
	;; 			 '("pyls"))

	;; make sure this is activated when python-mode is activated
	;; lsp-python-enable is created by macro above
	)

      (use-package pipenv
	:hook (python-mode . pipenv-mode))

      (use-package lsp-python
	:config (add-hook 'python-mode-hook #'lsp-python-enable))

      (use-package cquery
	:config
	(setf cquery-extra-init-params '(:completion (:detailedLabel t)))
	(setf cquery-sem-highlight-method 'font-lock)
	(cquery-use-default-rainbow-sem-highlight))

      (use-package company-lsp
	:config
	(setf company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
	(setf company-lsp-async t)
	)

      (use-package ivy-xref
	:init (setf xref-show-xrefs-function #'ivy-xref-show-xrefs))

      (use-package lsp-ui
	:config
	(setf lsp-ui-sideline-show-symbol t)
	(defun my-cquery-find-vars ()
	  (interactive)
	  (lsp-ui-peek-find-custom 'vars "$cquery/vars"))

	(defun my-cquery-find-callers ()
	  (interactive)
	  (lsp-ui-peek-find-custom 'callers "$cquery/callers"))

	(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
	(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
	(bind-key "C-c C-x v" #'my-cquery-find-vars lsp-ui-mode-map)
	(bind-key "C-c C-x c" #'my-cquery-find-callers lsp-ui-mode-map)
	(add-hook 'lsp-mode 'lsp-ui-mode))

      (defun my-c-mode-hook-func ()
	(electric-pair-mode 1)
	(company-mode 1)
	;; (irony-mode 1)
	(key-chord-mode 1)
	(lsp-cquery-enable)
	(lsp-ui-mode)
	(add-to-list 'write-file-functions 'delete-trailing-whitespace)
	(add-to-list 'company-backends 'company-lsp))
      (add-hook 'c-mode-hook 'my-c-mode-hook-func)

      (defun insert-semicolorn ()
	"Add semicolon at the end of the line and return to current position"
	(interactive)
	(save-excursion
	  (end-of-line)
	  (insert ";")))

      ;; TODO: Move these
      (mapc
       (lambda (keyscommand)
	 (key-chord-define-global
	  (car keyscommand) (cdr keyscommand)))
       '(
	 ("1j" . "!")
	 ("2j" . "@")
	 ("3j" . "#")
	 ("4j" . "$")
	 ("5j" . "%")
	 ("6f" . "^")
	 ("7f" . "&")
	 ("8f" . "*")

	 ("PP" . counsel-projectile-switch-project)
	 ("PO" . counsel-projectile-find-file)
	 ("PS" . counsel-projectile-ag)
	 ("PC" . projectile-compile-project)
	 ("PB" . projectile-switch-to-buffer)
	 ("PD" . projectile-dired)

	 ("0c" . compile)

	 ("1p" . pwd)

	 ("qq" . avy-goto-char-timer)
	 ("QQ" . avy-goto-word-1)
	 ("1q" . avy-goto-subword-1)
	 ("0q" . avy-goto-char)
	 ("1l" . avy-goto-line)

	 ("vj" . "|")))



      (use-package company-quickhelp
	:bind (:map company-active-map
		    ("M-h" . company-quickhelp-manual-begin))
	:config
	(progn
	  (setf company-quickhelp-delay 1.0)
	  (company-quickhelp-mode 1)))

      (use-package realgud)

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

      (use-package ediff
	:ensure nil
	:config
	(setf ediff-window-setup-function 'ediff-setup-windows-plain)
	(setf ediff-split-window-function 'split-window-horizontally)
	(add-hook 'ediff-after-quit-hook-internal 'winner-undo))

      (use-package flycheck
	:demand t
	:init
	(add-hook 'prog-mode-hook #'flycheck-mode)
	:config
	(setf flycheck-display-errors-function nil)
	(use-package flycheck-pos-tip
	  :config
	  (setf flycheck-pos-tip-timeout (* 60 10))
	  (flycheck-pos-tip-mode)))

      (use-package flycheck-irony
	:demand t
	:config
	(eval-after-load 'flycheck
	  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;;;; YAML MODE

      (use-package yaml-mode)

      (use-package yaml-tomato)

;;;; INDENTING

      (use-package aggressive-indent
	:diminish aggressive-indent-mode
	:config
	(global-aggressive-indent-mode 1)
	(add-to-list 'aggressive-indent-excluded-modes 'python-mode)
	(add-to-list 'aggressive-indent-excluded-modes 'haml-mode)
	(add-to-list 'aggressive-indent-excluded-modes 'html-mode))

      (use-package auto-indent-mode)

      (use-package elf-mode)

      (use-package lispy
	:init
	(defun attic/lispy--eval ()
	  (interactive)
	  (if (equal major-mode 'scheme-mode)
	      (geiser-eval-next-sexp nil)
	    (special-lispy-eval)))
	:config
	(lispy-define-key lispy-mode-map-special "e" 'attic/lispy--eval)
	(defun lispy-left-no-mark ()
	  (interactive)
	  (deactivate-mark)
	  (lispy-left 1)))

      (use-package common-lisp-snippets)

      (use-package lua-mode
	:config
	(setf lua-default-application "luajit"))

      (use-package elpy
	:config
	(setf elpy-rpc-backend "jedi")
	(elpy-enable))

      (message "Loading DEVEL stuff done")))


(if (server-is "org")
    (progn
      (message "Loading ORG stuff...")

      ;; Hack to make loading latest org mode work.
      ;; org-git-version, org-release
      (require 'subr-x)
      (straight-use-package 'git)

      (defun org-git-version ()
	"The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
	(require 'git)
	(let ((git-repo (expand-file-name
			 "straight/repos/org/" user-emacs-directory)))
	  (string-trim
	   (git-run "describe"
		    "--match=release\*"
		    "--abbrev=6"
		    "HEAD"))))

      (defun org-release ()
	"The release version of org-mode.
Inserted by installing org-mode or when a release is made."
	(require 'git)
	(let ((git-repo (expand-file-name
			 "straight/repos/org/" user-emacs-directory)))
	  (string-trim
	   (string-remove-prefix
	    "release_"
	    (git-run "describe"
		     "--match=release\*"
		     "--abbrev=0"
		     "HEAD")))))

      (provide 'org-version)

      (use-package org
	:demand t
	:ensure t
	:bind
	(("C-c M-l" . org-store-link)
	 ("C-c C-l" . org-insert-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("<pause>" . org-capture)
	 ("C-M-|" . indent-rigidly)
	 ("M-<left>" . org-do-promote)
	 ("M-<right>" . org-do-demote)
	 ("M-S-<left>" . org-promote-subtree)
	 ("M-S-<right>" . org-demote-subtree)
	 ("M-S-<up>" . org-move-subtree-up)
	 ("M-S-<down>" . org-move-subtree-down)
	 ("C-c C-e" . org-export-dispatch)
	 ("C-c C-S-E" . hydra-org-clock/body)

	 ("C-c C-q" .   air-org-set-tags)

	 ("C-c c" .   air-org-task-capture)
	 ("C-C t a" . mmm-pop-to-org-agenda)
	 ("C-c t n" . air-pop-to-org-notes)
	 ("C-c t t" . air-pop-to-org-todo)
	 ("C-c t c" . my-open-calendar)
	 ("C-c t A" . org-agenda)

	 ("C-c f k" . org-search-view)
	 ("C-c f t" . org-tags-view)
	 ("C-c f i" . air-org-goto-custom-id)
	 :map org-mode-map
	 ("<" . mmm-org-insert-template))

	;; :ensure org-plus-contrib
	:init
	(defun air-pop-to-org-todo (&optional split)
	  "Visit my main TODO list, in the current window or a SPLIT."
	  (interactive "P")
	  (air--pop-to-file org-default-tasks-file split))

	(defun air-pop-to-org-notes (&optional split)
	  "Visit my main TODO list, in the current window or a SPLIT."
	  (interactive "P")
	  (air--pop-to-file org-default-notes-file split))

	(defun air-org-task-capture (&optional vanilla)
	  "Capture a task with my default template.
If VANILLA is non-nil, run the standard `org-capture'."
	  (interactive "P")
	  (if vanilla
	      (org-capture)
	    (org-capture nil "t")))

	(defun air--pop-to-org-agenda-view (key &optional split)
	  "Visit the org agenda KEY, in the current window or a SPLIT."
	  ;; I don't know why this works, but it works.
	  (let ((current-prefix-arg nil))
	    (org-agenda nil key))
	  (when (not split)
	    (delete-other-windows)))

	(defun mmm-org-insert-template ()
	  (interactive)
	  (if (looking-back "^")
	      (hydra-org-template/body)
	    (self-insert-command 1)))

	(defun air-org-skip-subtree-if-priority (priority)
	  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
	  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
		(pri-value (* 1000 (- org-lowest-priority priority)))
		(pri-current (org-get-priority (thing-at-point 'line t))))
	    (if (= pri-value pri-current)
		subtree-end
	      nil)))

	(defun air-org-skip-subtree-if-habit ()
	  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
	  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
	    (if (string= (org-entry-get nil "STYLE") "habit")
		subtree-end
	      nil)))

	(defun load-org-agenda-files-recursively (dir) "Find all directories in DIR."
	       (unless (file-directory-p dir)
		 (error "Not a directory `%s'" dir))
	       (unless (equal (directory-files dir nil org-agenda-file-regexp t) nil)
		 (add-to-list 'org-agenda-files dir))
	       (dolist (file (directory-files dir nil nil t))
		 (unless (member file '("." ".."))
		   (let ((file (concat dir file "/")))
		     (when (file-directory-p file)
		       (load-org-agenda-files-recursively file))))))

	(defun mmm-pop-to-org-agenda (split)
	  "Visit the org agenda, in the current window or a SPLIT."
	  (interactive "P")
	  (org-agenda-list)
	  (when (not split)
	    (delete-other-windows)))

	(defun hot-expand (str)
	  "Expand org template."
	  (insert str)
	  (org-try-structure-completion))

	(defun air--org-display-tag (tag &optional focus)
	  "Display entries tagged with TAG in a fit window.
Do not make the new window current unless FOCUS is set."
	  (org-tags-view nil tag)
	  (fit-window-to-buffer)
	  (unless focus
	    (other-window 1)))

	(defun air-org-display-any-tag ()
	  "Display entries tagged with a tag selected interactively."
	  (interactive)
	  (air--org-display-tag (air--org-select-tag)))

	(defun air--org-select-tag ()
	  "Interactively select or enter a single tag."
	  (let ((org-last-tags-completion-table
		 (if (derived-mode-p 'org-mode)
		     (org-uniquify
		      (delq nil (append (org-get-buffer-tags)
					(org-global-tags-completion-table))))
		   (org-global-tags-completion-table))))
	    (completing-read
	     "Tag: " 'org-tags-completion-function nil nil nil
	     'org-tags-history)))

	(defun air--org-global-custom-ids ()
	  "Find custom ID fields in all org agenda files."
	  (let ((files (org-agenda-files))
		file
		air-all-org-custom-ids)
	    (while (setf file (pop files))
	      (with-current-buffer (org-get-agenda-file-buffer file)
		(save-excursion
		  (save-restriction
		    (widen)
		    (goto-char (point-min))
		    (while (re-search-forward "^[ \t]*:CUSTOM_ID:[ \t]+\\(\\S-+\\)[ \t]*$"
					      nil t)
		      (add-to-list 'air-all-org-custom-ids
				   `(,(match-string-no-properties 1)
				     ,(concat file ":" (number-to-string (line-number-at-pos))))))))))
	    air-all-org-custom-ids))

	(defun air--org-find-custom-id (custom-id)
	  "Return the location of CUSTOM-ID."
	  (let* ((all-custom-ids (air--org-global-custom-ids)))
	    (let* ((val (cadr (assoc custom-id all-custom-ids)))
		   (id-parts (split-string val ":"))
		   (file (car id-parts))
		   (line (string-to-int (cadr id-parts))))
	      (with-current-buffer (org-get-agenda-file-buffer file)
		(goto-char (point-min))
		(forward-line line)
		(org-reveal)
		(org-up-element)
		(list (current-buffer) (point))))))

	(defun air-org-goto-custom-id (&optional split)
	  "Go to the location of a custom ID read interactively, maybe in a SPLIT."
	  (interactive "P")
	  (let* ((all-custom-ids (air--org-global-custom-ids))
		 (custom-id (completing-read
			     "Custom ID: "
			     all-custom-ids))
		 (id-location (air--org-find-custom-id custom-id)))
	    (when id-location
	      (let* ((buf (car id-location))
		     (loc (cadr id-location)))
		(pop-to-buffer buf (if split t nil))
		(goto-char loc)
		(org-reveal)))))

	(defun air-org-insert-custom-id-link ()
	  "Insert an Org link to a custom ID selected interactively."
	  (interactive)
	  (let* ((all-custom-ids (air--org-global-custom-ids))
		 (custom-id (completing-read
			     "Custom ID: "
			     all-custom-ids)))
	    (when custom-id
	      (let* ((val (cadr (assoc custom-id all-custom-ids)))
		     (id-parts (split-string val ":"))
		     (file (car id-parts))
		     (line (string-to-int (cadr id-parts))))
		(org-insert-link nil (concat file "::#" custom-id) custom-id)))))

	(defun air--org-swap-tags (tags)
	  "Replace any tags on the current headline with TAGS.
The assumption is that TAGS will be a string conforming to Org Mode's
tag format specifications, or nil to remove all tags."
	  (let ((old-tags (org-get-tags-string))
		(tags (if tags
			  (concat " " tags)
			"")))
	    (save-excursion
	      (beginning-of-line)
	      (re-search-forward
	       (concat "[ \t]*" (regexp-quote old-tags) "[ \t]*$")
	       (line-end-position) t)
	      (replace-match tags)
	      (org-set-tags t))))

	(defun air-org-set-tags (tag)
	  "Add TAG if it is not in the list of tags, remove it otherwise.
TAG is chosen interactively from the global tags completion table."
	  (interactive (list (air--org-select-tag)))
	  (let* ((cur-list (org-get-tags))
		 (new-tags (mapconcat 'identity
				      (if (member tag cur-list)
					  (delete tag cur-list)
					(append cur-list (list tag)))
				      ":"))
		 (new (if (> (length new-tags) 1) (concat " :" new-tags ":")
			nil)))
	    (air--org-swap-tags new)))

	:config
	(setf org-agenda-custom-commands
	      '(("d" "Daily agenda and all TODOs"
		 ((tags "PRIORITY=\"A\""
			((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
			 (org-agenda-overriding-header "High-priority unfinished tasks:")))
		  (agenda "" ((org-agenda-span 'day)))
		  (alltodo ""
			   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
							   (air-org-skip-subtree-if-priority ?A)
							   (org-agenda-skip-if nil '(scheduled deadline))))
			    (org-agenda-overriding-header "ALL normal priority tasks:"))))
		 ((org-agenda-compact-blocks t)))))

	(setf org-refile-targets '((nil :maxlevel . 2)
				   (org-agenda-files :maxlevel . 2)))

	(setf org-refile-use-outline-path t)
	(setf org-refile-allow-creating-parent-nodes 'confirm)
	(setf org-modules '(org-habit))
	(setf org-agenda-include-diary nil)

	(setf org-log-done (quote time))
	(setf org-log-redeadline (quote time))
	(setf org-log-reschedule (quote time))

	(setf org-pretty-entities t)

	(setf org-log-into-drawer t)
	(setf org-use-speed-commands t
	      org-hide-emphasis-markers t
	      org-src-fontify-natively t   ;; Pretty code blocks
	      org-src-tab-acts-natively t
	      org-confirm-babel-evaluate nil)
	(setf org-src-fontify-natively t)
	(setf org-src-tab-acts-natively t)
	(setf org-return-follows-link t)

	(setf org-ellipsis "⤵")
	(setf org-todo-keywords
	      '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAIT(w@/!)" "|"
			  "DONE(d!)" "CANCELED(c@)")))

	(setf org-todo-keyword-faces
	      '(("TODO" . "LightSkyBlue")
		("IN-PROGRESS" . "yellow2")
		("WAIT" . "IndianRed")
		("DONE" . "gold")
		("CANCELED" . "red")))

	(setf org-tag-alist '((:startgroup . nil)
			      ("@work" . ?w)
			      ("@home" . ?h)
			      ("@computer" . ?l)
			      ("@mobile" . ?p)
			      (:endgroup . nil)
			      ("ttdp" . ?t)
			      ("config" . ?c)
			      ("emacs" . ?E)
			      ("org" . ?o)
			      ("meeting" . ?M)
			      ("household" . ?H)
			      ("economy" . ?e)))

	(setf org-tag-faces
	      '(("@home"
		 :foreground "Green3"
		 :background nil
		 :weight bold)
		("@work"
		 :foreground "DeepSkyBlue"
		 :background nil
		 :weight bold)
		("@computer"
		 :foreground "LightSeaGreen"
		 :background nil
		 :weight bold)
		("@mobile"
		 :foreground "Orange"
		 :background nil
		 :weight bold)))

	(setf org-journal-dir "~/sync/org/journal/")
	(setf org-directory "~/sync/org/")
	(setf org-default-notes-file "~/sync/org/notes.org")
	(setf org-default-tasks-file "~/sync/org/tasks.org")

	;; Collect all .org from my Org directory and subdirs
	(setf org-agenda-file-regexp "\\`[^.].*\\.org\\'") ; default value

	(load-org-agenda-files-recursively "~/sync/org/") ; trailing slash required

	(org-babel-do-load-languages
	 'org-babel-load-languages
	 '((awk . t)
	   (emacs-lisp . t)
	   (python . t)
	   (ruby . t)
	   ))

	(font-lock-add-keywords
	 'org-mode
	 '(("^ +\\([-*]\\) "
	    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

	(add-hook 'org-mode-hook 'auto-fill-mode)
	(add-hook 'org-mode-hook 'flyspell-mode)

	(defun mmm/org-capture-mode-hook ()
	  (bind-keys :map org-capture-mode-map
		     ("C-d" . insert-current-time)
		     ("M->" . org-priority-up)
		     ("M-<" . org-priority-down)
		     ("C-t" . air-org-set-tags)))
	(add-hook 'org-capture-mode-hook 'mmm/org-capture-mode-hook)

	(define-key org-mode-map (kbd "M-C-n") 'org-end-of-item-list)
	(define-key org-mode-map (kbd "M-C-p") 'org-beginning-of-item-list)
	(define-key org-mode-map (kbd "M-C-u") 'outline-up-heading)
	(define-key org-mode-map (kbd "M-C-w") 'org-table-copy-region)
	(define-key org-mode-map (kbd "M-C-y") 'org-table-paste-rectangle)

	(define-key org-mode-map [remap org-return] (lambda () (interactive)
						      (if (org-in-src-block-p)
							  (org-return)
							(org-return-indent))))

	(use-package org-bullets
	  :ensure t
	  :init
	  (add-hook 'org-mode-hook 'org-bullets-mode)
	  (setf org-bullets-bullet-list '("\u2022")))

	(use-package ox-html5slide
	  :ensure t
	  :init
	  (setf org-html-postamble nil)
	  (setf org-export-with-section-numbers nil)
	  (setf org-export-with-toc nil)
	  (setf org-html-head-extra "
       <link href='http://fonts.googleapis.com/css?family=Source+Sans+Pro:400,700,400italic,700italic&subset=latin,latin-ext' rel='stylesheet' type='text/css'>
       <link href='http://fonts.googleapis.com/css?family=Source+Code+Pro:400,700' rel='stylesheet' type='text/css'>
       <style type='text/css'>
          body {
             font-family: 'Source Sans Pro', sans-serif;
          }
          pre, code {
             font-family: 'Source Code Pro', monospace;
          }
       </style>"))

	(use-package org-gcal)

	(use-package ox-reveal
	  :init
	  (setf org-reveal-postamble "Magnus Malm")
	  (setf org-reveal-root "file:///home/magnus/src/reveal.js"))

	(use-package org-pdfview)

	(use-package ical2org
	  :ensure nil
	  :load-path "lisp/"
	  :config
	  (setf ical2org/completing-read #'ivy-completing-read))

	(use-package calfw-ical)
	(use-package calfw-org)

	(use-package calfw
	  :config
	  (setf calendar-week-start-day 1)
	  (setf cfw:fchar-junction ?╬
    		cfw:fchar-vertical-line ?║
    		cfw:fchar-horizontal-line ?═
    		cfw:fchar-left-junction ?╠
    		cfw:fchar-right-junction ?╣
    		cfw:fchar-top-junction ?╦
    		cfw:fchar-top-left-corner ?╔
    		cfw:fchar-top-right-corner ?╗)

	  (setf mmm/cfw-sources
  		(list
  		 (cfw:org-create-source "Green")
  		 (cfw:ical-create-source (first mmm/cfw-cal-magnus)
  					 (second mmm/cfw-cal-magnus)
  					 (third mmm/cfw-cal-magnus))
  		 (cfw:ical-create-source (first mmm/cfw-cal-misc)
  					 (second mmm/cfw-cal-misc)
  					 (third mmm/cfw-cal-misc))))
	  (defun my-open-calendar ()
	    (interactive)
	    (cfw:open-calendar-buffer
	     :contents-sources mmm/cfw-sources))

	  (setf cfw:org-capture-template'("c" "calfw2org" entry (file nil)  "* %?
   %(cfw:org-capture-day)")))

	(use-package poporg))

      (setf org-capture-templates
	    '(
	      ("j" "Journal entry" plain
	       (file+olp+datetree "~/sync/org/journal/journal.org")
	       "%i\n\n**** %?\n" :empty-lines 1)
	      ("t" "Task Entry" entry
	       (file+headline "~/sync/org/tasks.org" "Todos")
	       "* TODO %?\n%u" :prepend t)
	      ("n" "Note" entry
	       (file+headline "~/sync/org/notes.org" "Notes")
	       "* %?\n%u" :prepend t)
	      )
	    )
      (defun place-agenda-tags ()
	"Put the agenda tags by the right border of the agenda window."
	(setf org-agenda-tags-column (- 4 (window-width)))
	(org-agenda-align-tags))
      ;; Place tags close to the right-hand side of the window
      (add-hook 'org-finalize-agenda-hook #'place-agenda-tags)

      (setf org-tags-column 0)))

(setf fill-column 80)

(use-package cflow-mode
  :ensure nil
  :straight (:host github
		   :repo "AaronNGray/cflow"
		   :branch "master"
		   :files ("elisp/cflow-mode.el")))

(use-package erc-nick-notify
  :ensure nil
  :straight (:host github
		   :repo "emacsmirror/erc-nick-notify"
		   :branch "master"))

;;;; KEYBINDINGS
(bind-key "C-S-O" 'find-file-in-config-dir)
(bind-key "C-S-M-O" 'find-file-in-sync-dir)
(bind-key "C-S-n" 'new-empty-buffer)
(bind-key* "C-w" 'close-current-buffer)
(bind-key "C-s" 'save-buffer)
(bind-key "C-S-s" 'write-file)
(bind-key "C-a" 'mark-whole-buffer)
(bind-key "C-<prior>" 'previous-emacs-buffer)
(bind-key "C-<next>" 'next-emacs-buffer)
(bind-key "M-<prior>" 'previous-user-buffer)
(bind-key "M-<next>" 'next-user-buffer)
(bind-key "s-I" 'previous-emacs-buffer)
(bind-key "s-K" 'next-emacs-buffer)
(bind-key "s-J" 'previous-user-buffer)
(bind-key "s-L" 'next-user-buffer)
(bind-key "C-x C-b" 'ibuffer)
(bind-key "H-c" 'compile)
(bind-key " " 'compile)
(bind-key "M-d" 'hungry-delete-backward)
(bind-key "M-f" 'hungry-delete-forward)
(bind-key "M-e" 'backward-kill-word)
(bind-key "M-r" 'kill-word)
(bind-key* "M-x" 'xah-cut-line-or-region)
(bind-key* "M-c" 'xah-copy-line-or-region)
(bind-key* "M-v" 'yank)
(bind-key "M-/" 'toggle-letter-case)
(bind-key "M-q" 'fill-paragraph)
(unbind-key (kbd "C-z")) ; suspend-frame
(unbind-key (kbd "s-p")) ; ns-print-buffer
(unbind-key (kbd "s-q")) ; save-buffers-kill-emacs
(unbind-key (kbd "s-t")) ; ns-popup-font-panel
(unbind-key (kbd "C-x C-c")) ; save-buffers-kill-terminal
(bind-key "H-r" 'repeat)
(bind-key "M-A" 'shell-command)
(bind-key "H-o" 'ff-find-other-file)
(bind-key "C-c C-u" 'mmm:uuid)
(bind-key* "M-j" 'backward-char)
(bind-key* "M-l" 'forward-char)
(bind-key* "M-i" 'previous-line)
(bind-key* "M-k" 'next-line)
(bind-key* "M-u" 'backward-word)
(bind-key* "M-o" 'forward-word)
(bind-key* "M-J" 'backward-paragraph)
(bind-key* "M-L" 'forward-paragraph)
(bind-key* "M-h" 'crux-move-beginning-of-line)
(bind-key* "M-H" 'move-end-of-line)
(bind-key "M-I" 'scroll-down)
(bind-key "M-K" 'scroll-up)
(bind-key "M-U" 'beginning-of-buffer)
(bind-key "M-O" 'end-of-buffer)
(bind-key* "M-g" 'goto-line)
(bind-key* "M-G" 'goto-char)
(bind-key* "M-S-SPC" 'mark-paragraph)
(bind-key* "M-SPC" 'set-mark-command)

;; comint mode (shell)
(bind-key "M-r" 'kill-word comint-mode-map)
(bind-key "M-e" 'backward-kill-word comint-mode-map)
(bind-key "C-d" 'eshell-send-eof-to-process comint-mode-map)
(bind-key "\C-i" 'endless/ispell-word-then-abbrev ctl-x-map)

;; C-x C-0 restores the default font size
(bind-key* "C-+" 'modi/global-font-size-incr)
(bind-key* "C--" 'modi/global-font-size-decr)
(bind-key* "C-x C-0" 'modi/global-font-size-reset)
(bind-key "C-S-w" 'delete-frame)
(bind-key "M-Z" 'zap-to-char)
(bind-key "M-z" 'undo)
(bind-key "C-z" 'undo)

(bind-key "M->" 'split-window-vertically)
(bind-key "M-<" 'split-window-horizontally)

;; https://www.emacswiki.org/emacs/Scrolling#toc2
(defun gcm-scroll-down ()
  (interactive)
  (scroll-up 1))

(defun gcm-scroll-up ()
  (interactive)
  (scroll-down 1))

(defun gcm-scroll-down-5 ()
  (interactive)
  (scroll-up 5))

(defun gcm-scroll-up-5 ()
  (interactive)
  (scroll-down 5))

(bind-key "C-<up>" 'gcm-scroll-up)
(bind-key "C-<down>" 'gcm-scroll-down)

(bind-key "C-M-<up>" 'gcm-scroll-up-5)
(bind-key "C-M-<down>" 'gcm-scroll-down-5)

(defun package--save-selected-packages (&rest opt) nil)
