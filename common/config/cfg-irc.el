(require 'erc)
(require 'erc-match)
(require 'erc-imenu)

;; (add-to-list 'erc-modules 'log)
;; (add-to-list 'erc-modules 'notifications)

(setf erc-auto-query 'window-noselect)
(setf erc-hide-list '("PART" "QUIT"))
(setf erc-modules '(log fill notifications move-to-prompt stamp
			;; spelling
			hl-nicks netsplit fill button
			match track completion readonly networks
			ring autojoin noncommands irccontrols
			move-to-prompt stamp menu list))
(setf erc-pcomplete-order-nickname-completions nil)


(setf erc-prompt  (lambda () (concat (buffer-name) ">")))

(defun my-erc-mode-hook-func ()
  (emojify-mode)
  ;; (flyspell-mode 1)
  ;; (erc-spelling-mode 1)
  (bind-key "M-<return>" 'counsel-irc-query-nick erc-mode-map))

(add-hook 'erc-mode-hook 'my-erc-mode-hook-func)

;; This is important when using ZNC, otherwise different network's connections will use the same ERC server buffer and ERC will
;; reconnect every few minutes.
(setf erc-rename-buffers t)

(setf erc-save-buffer-on-part t)
(setf erc-log-channels-directory "~/.erc/logs/")
(setf erc-server-reconnect-timeout 5)
(setf erc-server-reconnect-attempts t)

(defface erc-header-line-disconnected
  '((t (:inherit magit-diff-removed)))
  "Face to use when ERC has been disconnected.")

(defun erc-update-header-line-show-disconnected ()
  "Use a different face in the header-line when disconnected."
  (erc-with-server-buffer
   (cond ((erc-server-process-alive) 'erc-header-line)
	 (t 'erc-header-line-disconnected))))

(setf erc-header-line-face-method 'erc-update-header-line-show-disconnected)
(setf erc-hide-list nil)

(require 'erc-input-fill)

;; (setf erc-timestamp-only-if-changed-flag nil
;;       erc-timestamp-format "%H:%M "
;;       erc-fill-prefix "    | "
;;       erc-insert-timestamp-function 'erc-insert-timestamp-left)

;; timestamps
(make-variable-buffer-local
 (defvar erc-last-datestamp nil))

(defun ks-timestamp (string)
  (erc-insert-timestamp-left string)
  (let ((datestamp (erc-format-timestamp (current-time) erc-datestamp-format)))
    (unless (string= datestamp erc-last-datestamp)
      (erc-insert-timestamp-left datestamp)
      (setf erc-last-datestamp datestamp))))

(setf erc-timestamp-only-if-changed-flag t
      erc-timestamp-format "%H:%M "
      erc-datestamp-format " \n\n=== [%Y-%m-%d %a] ===\n" ; mandatory ascii art
      erc-fill-prefix "      "
      erc-insert-timestamp-function 'ks-timestamp)

(use-package erc-hl-nicks)

(defun irc-nick-list ()
  (sort (erc-get-channel-nickname-list) #'string-collate-lessp))

(defun counsel-irc-query-nick (&optional query)
  (interactive)
  (let ((nick-list (irc-nick-list)))
    (ivy-read "nick: " nick-list
	      :action '(1
			("i" (lambda (nick) (insert (string-join (list nick ": ") ""))) "Mention final")
			("I" (lambda (nick) (insert (string-join (list nick ", ") ""))) "Mention")
			("q" (lambda (nick) (erc-cmd-QUERY nick)) "Open query window")))))

(use-package erc-colorize
  :config
  (erc-colorize-mode 1))

;; https://github.com/thisirs/erc-colorize

;; (use-package erc-nick-notify
;;   :straight (:host github
;; 	     :repo "emacsmirror/erc-nick-notify"
;; 	     :branch "master"))

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

(setq erc-current-nick-highlight-type 'nick)
(setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"
				"324" "329" "477" "333" "353"))
(setq erc-track-use-faces t)
(setq erc-track-faces-priority-list
      '(erc-current-nick-face erc-keyword-face))
(setq erc-track-priority-faces-only 'all)

(defun switch-to-irc ()
  "Switch to an IRC buffer, or run `erc-select'.
    When called repeatedly, cycle through the buffers."
  (interactive)
  (let ((buffers (and (fboundp 'erc-buffer-list)
                      (erc-buffer-list))))
    (when (eq (current-buffer) (car buffers))
      (bury-buffer)
      (setf buffers (cdr buffers)))
    (if buffers
        (switch-to-buffer (car buffers))
      (call-interactively 'erc-select))))

(global-set-key (kbd "C-c e") 'switch-to-irc)

(defun fit-window-to-buffer-width (&optional window max-width min-width)
  "Fit WINDOW according to its buffer's width.
	     WINDOW, MAX-WIDTH and MIN-WIDTH have the same meaning as in
	     `fit-window-to-buffer'."
  (interactive)
  (let ((fit-window-to-buffer-horizontally 'only))
    (fit-window-to-buffer window nil nil max-width min-width)))

(use-package erc-nick-notify
  :straight (:host github
	     :repo "emacsmirror/erc-nick-notify"
	     :branch "master"))

(setf znc-identifier (system-name))

(setf erc-join-buffer 'bury)

(set-face-attribute 'erc-distinct-1-face nil :foreground "deep sky blue")
(set-face-attribute 'erc-distinct-3-face nil :foreground "green")
(set-face-attribute 'erc-notice-face nil :foreground "BFEBBF")

(defun mmm/change-nick (suffix)
  (interactive "sNick suffix: ")
  (if (string= "" suffix)
      (insert (format "/nick malm"))
    (insert (format "/nick malm_%s" suffix))))
(bind-key "C-n" #'mmm/change-nick erc-mode-map)

(defun mmm/me-text (text)
  (interactive "sMe text: ")
  (unless (string= "" text)
    (insert (format "/me %s" text))))
(bind-key "C-e" #'mmm/me-text erc-mode-map)

;; (defun mmm/correct-word ()
;;   (interactive)
;;   (let ((word (avy-goto-char-2))
;; 	(let ((ispell-dictionary "british"))
;; 	  (insert (format "s/%s" word))
;; 	  (flyspell-popup-correct)))))

(define-prefix-command 'emoji-map)
(global-set-key (kbd "C-c C-s") emoji-map)

(defmacro reg-emoji (name emoji)
  (let ((func-name (intern (concat "mmm/insert-" name))))
    `(progn
       (defun ,func-name ()
         ,(format "Insert the emoji %s." name)
         (interactive)
	 (insert ,emoji))
       #',func-name)))

(bind-key "+" (reg-emoji "thumbs-up" "👍") emoji-map)
(bind-key "-" (reg-emoji "thumbs-down" "👎") emoji-map)
(bind-key "g" (reg-emoji "thumbs-up" "👍") emoji-map)
(bind-key "b" (reg-emoji "thumbs-down" "👎") emoji-map)
(bind-key "s" (reg-emoji "smile" "🙂") emoji-map)
(bind-key "S" (reg-emoji "big-smile" "😃") emoji-map)
(bind-key "c" (reg-emoji "confused-smile" "😕") emoji-map)
(bind-key "a" (reg-emoji "angry-smile" "😠") emoji-map)
(bind-key "f" (reg-emoji "facepalm" "🤦") emoji-map)
(bind-key "p" (reg-emoji "ponder" "🤔") emoji-map)

(bind-key "C" (reg-emoji "coffee" "☕") emoji-map)
(bind-key "P" (reg-emoji "pizza" "🍕") emoji-map)
(bind-key "F" (reg-emoji "food" "🍲") emoji-map)
(bind-key "T" (reg-emoji "cake" "🎂") emoji-map)
(bind-key "U" (reg-emoji "uncertain" "¯\\_(\u30c4)_/¯") emoji-map)
(bind-key "u" (reg-emoji "shrug" "🤷") emoji-map)
(bind-key "y" (reg-emoji "yay" "\\o/") emoji-map)

(defun call-figlet (string)
  (interactive "sText: ")
  (unless (string= "" string)
    (push-mark)
    (call-process "figlet" nil (current-buffer) nil string)))

(bind-key "|" 'call-figlet emoji-map)

(defun call-crazy-cow (string)
  (interactive "sText: ")
  (unless (string= "" string)
    (push-mark)
    (call-process "cowsay" nil (current-buffer) nil
		  "-e" "oO" "-T" "U" string)))

(bind-key "/" 'call-crazy-cow emoji-map)

(defun call-fortune (who)
  (interactive "sWho: ")
  (push-mark
   (insert (shell-command-to-string
	    (format "fortune | cowsay -f %s" (if (string= "" who) "default" who))))))

(defun say-it (who text)
  (interactive "sWho:
sText: ")
  (push-mark
   (insert
    (shell-command-to-string
     (format "%s | cowsay -f %s"
	     (if (string= "" text) "fortune -s linux" (format "echo %s" text))
	     (if (string= "" who) "tux" who))))))

(defun pipe-it (cmd)
  (interactive "sCommand: ")
  (push-mark
   (insert
    (shell-command-to-string
     (format "%s" cmd)))))

(defun unwrap-line ()
  "Remove all newlines until we get to two consecutive ones.
    Or until we reach the end of the buffer.
    Great for unwrapping quotes before sending them on IRC."
  (interactive)
  (let ((start (point))
        (end (copy-marker (or (search-forward "\n\n" nil t)
                              (point-max))))
        (fill-column (point-max)))
    (fill-region start end)
    (goto-char end)
    (newline)
    (goto-char start)))

(bind-key "M-q" #'unwrap-line)

(defun erc-cmd-HOWMANY (&rest ignore)
  (interactive)
  "Display how many users (and ops) the current channel has."
  (erc-display-message nil 'notice (current-buffer)
                       (let ((hash-table (with-current-buffer
                                             (erc-server-buffer)
                                           erc-server-users))
                             (users 0)
                             (ops 0))
                         (maphash (lambda (k v)
                                    (when (member (current-buffer)
                                                  (erc-server-user-buffers v))
                                      (incf users))
                                    (when (erc-channel-user-op-p k)
                                      (incf ops)))
                                  hash-table)
                         (format
                          "There are %s users (%s ops) on the current channel"
                          users ops))))

(defun erc-cmd-FLUSH (&rest ignore)
  "Erase the current buffer."
  (let ((inhibit-read-only t))
    (buffer-disable-undo)
    (erase-buffer)
    (buffer-enable-undo)
    (message "Flushed contents of channel")
    t))
