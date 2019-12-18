(defun my-kill-emacs ()
  "Before killing Emacs, Add a note about user terminated Emacs in my status file"
  (interactive)
  (my-log (format "Terminated by user @ %s" (current-time-string)) t)
  (kill-emacs))

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

(defun my-lisp-mode-hook-fn ()
  (easy-escape-minor-mode 1)
  (setf fill-column 132)
  (setq-local company-backends (add-to-list 'company-backends 'company-elisp)))

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

(defun kill-line-backward ()
  "Kill text between the beginning of the line to the cursor position.
If there's no text, delete the previous line ending."
  (interactive)
  (if (looking-back "\n" nil)
      (delete-char -1)
    (kill-line 0)))

(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
	(beginning-of-line))))

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

(defun goto-line-show ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively #'goto-line))
    (linum-mode -1)))
(bind-key "C-s-L" 'goto-line-show)

(defun current-time-microseconds ()
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%d %T" nowtime) (format ".%d] " now-ms))))

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

(defun divide-and-follow (&optional number-of-windows)
  "Split the current frame vertically into multiple
windows (default 2) viewing the current buffer in follow
mode. Prefix argument splits into the indicated number of
windows. E.g. C-3 M-x divide-and-follow will give you three
columns."
  (interactive "p")
  (follow-delete-other-windows-and-split)
  (let ((splits-needed (max 0 (- number-of-windows 2))))
    (dotimes (s splits-needed) (split-window-horizontally)))
  (balance-windows))

(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setf col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))
(define-key global-map (kbd "H-<tab>") 'aj-toggle-fold)

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

(defun mmm/copy-file-name-to-clipboard (full-path)
  "Copy the current buffer file name to the clipboard."
  (interactive "P")
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (if full-path (buffer-file-name) (buffer-name)))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun air--pop-to-file (file &optional split)
  "Visit a FILE, either in the current window or a SPLIT."
  (if split
      (find-file-other-window file)
    (find-file file)))

(defun mmm/uuid ()
  (interactive)
  (insert (s-replace "\n" "" (shell-command-to-string "uuid"))))

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

;; from http://emacswiki.org/emacs/InsertingTodaysDate
(defun insert-todays-date (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%d-%m-%Y")
            (format-time-string "%Y-%m-%d"))))

;; TODO
;; (global-set-key "\C-c\C-d" 'insert-current-date-time)
;; (global-set-key "\C-c\C-t" 'insert-current-time)

(defun dired-wrdc155-ftp ()
  (interactive)
  (dired "/ssh:magma@ftp.labs.westermo.se:/srv/ftp/share/malm/"))

(defun copy-current-file-to-wemo-ftp ()
  (interactive)
  (copy-file (buffer-file-name) "/ssh:magma@ftp.labs.westermo.se:/srv/ftp/share/malm/" t t)
  (let ((str (concat "http://ftp.labs.westermo.se/share/malm/"
		     (file-name-nondirectory (buffer-file-name)))))
    (kill-new str)
    (message str)))

(defun ar/ediff-dir-content-size ()
  "Diff all subdirectories (sizes only) in two directories."
  (interactive)
  (let* ((dir1-path (read-directory-name "Dir 1: "))
         (dir2-path (read-directory-name "Dir 2: "))
         (buf1 (get-buffer-create (format "*Dir 1 (%s)*" (f-base dir1-path))))
         (buf2 (get-buffer-create (format "*Dir 2 (%s)*" (f-base dir2-path)))))
    (with-current-buffer buf1
      (erase-buffer))
    (with-current-buffer buf2
      (erase-buffer))
    (shell-command (format "cd %s; find . -type d | sort | du -h" dir1-path) buf1)
    (shell-command (format "cd %s; find . -type d | sort | du -h" dir2-path) buf2)
    (ediff-buffers buf1 buf2)))

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

;; https://raw.githubusercontent.com/Fuco1/.emacs.d/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el
;;
;; redefines the silly indent of keyword lists
;; before
;;   (:foo bar
;;         :baz qux)
;; after
;;   (:foo bar
;;    :baz qux)
(defun fuco1/lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setf method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(defun my/dynamic-xref-apropos ()
  (interactive)
  (let ((buf (current-buffer)))
    (ivy-read "Search for pattern: "
	      (lambda (str)
                (cond
                 ((< (length str) 1) (counsel-more-chars 1))
                 (t
                  (with-current-buffer buf
                    (when-let ((backend (xref-find-backend)))
		      (unless (eq backend 'etags)
                        (mapcar
                         (lambda (xref)
                           (let ((loc (xref-item-location xref)))
                             (propertize
			      (concat
			       (when (xref-file-location-p loc)
                                 (with-slots (file line column) loc
                                   (format "%s:%s:%s:"
                                           (propertize (file-relative-name file)
						       'face 'compilation-info)
                                           (propertize (format "%s" line)
						       'face 'compilation-line
						       )
                                           column)))
			       (xref-item-summary xref))
			      'xref xref)))
                         (xref-backend-apropos backend str))))))))
	      :dynamic-collection t
	      :action (lambda (item)
                        (xref--pop-to-location (get-text-property 0 'xref item))))))

(defun my/gns3-term (host port desc)
  (telnet host port)
  (rename-buffer (format "*%s - %s*" desc port))
  (message (format "%s:%s - %s" host port desc)))

(defun set-region-writeable (begin end)
  "Removes the read-only text property from the marked region."
  ;; See http://stackoverflow.com/questions/7410125
  (interactive "r")
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (remove-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))

(defun goto-symbol ()
  (interactive)
  (deactivate-mark)
  (condition-case err
      (progn
	(ring-insert find-tag-marker-ring (point-marker))
	(cl-flet ((always-no (&rest \_) (signal (car err) (cdr err))))
	  (cl-letf (((symbol-function 'y-or-no-p) #'always-no)
		    ((symbol-function 'yes-or-no-p) #'always-no))
	    (xref-find-definitions))))
    (error
     (set-marker (ring-remove find-tag-marker-ring 0) nil nil)
     (dumb-jump-go))))

(defun keymap-symbol (keymap)
  "Return the symbol to which KEYMAP is bound, or nil if no such symbol exists."
  (catch 'gotit
    (mapatoms (lambda (sym)
                (and (boundp sym)
                     (eq (symbol-value sym) keymap)
                     (not (eq sym 'keymap))
                     (throw 'gotit sym))))))

(use-package cl-format
  :straight (:host github
	     :repo "alvinfrancis/cl-format"))

(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

(defun window-split-n-below (n)
  (interactive "p")
  (if (and n (> n 1))
      (dotimes (_ (1- n))
	(split-window-below))
    (split-window-below))
  (balance-windows))

(defun window-split-n-right (n)
  (interactive "p")
  (if (and n (> n 1))
      (dotimes (_ (1- n))
	(split-window-right))
    (split-window-right))
  (balance-windows))

;;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
	;; This would override `fill-column' if it's an integer.
	(emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun list-env-paths ()
  (interactive)
  "Dumps a newline separated list of all items in $PATH."
  (message (replace-regexp-in-string (regexp-quote ":") "\n" (getenv "PATH") nil 'literal)))

(defun describe-thing-in-popup ()
  (interactive)
  (let* ((thing (symbol-at-point))
         (help-xref-following t)
         (description (with-temp-buffer
                        (help-mode)
                        (describe-variable thing)
                        (describe-symbol thing)
                        (buffer-string))))
    (popup-tip description
               :point (point)
               :around t
               :height 30
               :scroll-bar t
               :margin t)))

(defun mmm/switch-buffer-dwim ()
  (interactive)
  (if (projectile-project-p)
      (projectile-switch-to-buffer)
    (ivy-switch-buffer)))

(defun fhd/counsel-everything ()
  "list everything recursively"
  (interactive)
  (let* ((cands (split-string
                 (shell-command-to-string "fdfind .") "\n" t)))
    (ivy-read "File: " cands
              :action #'find-file
              :caller 'fhd/counsel-everything)))

(defun mmm/print-current-font ()
  (interactive)
  (message "%s" (face-attribute 'default :font nil 'default)))

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (progn (widen)
						  (recenter)))
	((region-active-p)
	 (narrow-to-region (region-beginning)
			   (region-end)))
	((derived-mode-p 'org-mode)
	 ;; `org-edit-src-code' is not a real narrowing
	 ;; command. Remove this first conditional if
	 ;; you don't want it.
	 (cond ((ignore-errors (org-edit-src-code) t)
		(delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
	((derived-mode-p 'latex-mode)
	 (LaTeX-narrow-to-environment))
	(t (narrow-to-defun))))

(define-prefix-command 'endless/toggle-map)
;; The manual recommends C-c for user keys, but C-x t is
;; always free, whereas C-c t is used by some modes.
(define-key ctl-x-map "t" 'endless/toggle-map)
(define-key endless/toggle-map "c" #'column-number-mode)
(define-key endless/toggle-map "d" #'toggle-debug-on-error)
(define-key endless/toggle-map "e" #'toggle-debug-on-error)
(define-key endless/toggle-map "f" #'auto-fill-mode)
(define-key endless/toggle-map "l" #'toggle-truncate-lines)
(define-key endless/toggle-map "q" #'toggle-debug-on-quit)
(define-key endless/toggle-map "t" #'endless/toggle-theme)

;;; Generalized version of `read-only-mode'.
(define-key endless/toggle-map "r" #'dired-toggle-read-only)
(autoload 'dired-toggle-read-only "dired" nil t)
(define-key endless/toggle-map "w" #'whitespace-mode)

(define-key endless/toggle-map "n"
  #'narrow-or-widen-dwim)
;; This line actually replaces Emacs' entire narrowing
;; keymap, that's how much I like this command. Only
;; copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)

(defun save-buffer-kill ()
  (interactive)
  (if (and (buffer-modified-p)
	   (yes-or-no-p "Save? "))
      (save-buffer)
    (not-modified))
  (kill-buffer-and-window))

;; Easily create .dir-locals
(defmacro project-specifics (name &rest body)
  (declare (indent 1))
  `(progn
     (add-hook 'find-file-hook
               (lambda ()
                 (when (string-match-p ,name (buffer-file-name))
                   ,@body)))
     (add-hook 'dired-after-readin-hook
               (lambda ()
                 (when (string-match-p ,name (dired-current-directory))
                   ,@body)))))

;; Example usage
;; (project-specifics "projects/zombietdd"
;; 		   (set (make-local-variable 'slime-js-target-url) "http://localhost:3000/")
;; 		   (ffip-local-patterns "*.js" "*.jade" "*.css" "*.json" "*.md"))

(provide 'cfg-utils)
;;; cfg-utils.el ends here

(defun clean-fawlty-logs ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(\0\\)" nil t)
      (replace-match ""))

    (goto-char (point-min))
    (while (re-search-forward "\\(\\)" nil t)
      (replace-match ""))

    (goto-char (point-min))
    (while (re-search-forward "\\([[].m\\)" nil)
      (replace-match ""))))
