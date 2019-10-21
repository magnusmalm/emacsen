(require 'erc)
(require 'erc-match)
(require 'erc-imenu)

(add-to-list 'erc-modules 'log)
(add-to-list 'erc-modules 'notifications)

(setf erc-auto-query 'window-noselect)
(setf erc-hide-list '("PART" "QUIT"))
(setf erc-modules '(log fill notifications move-to-prompt stamp
			spelling hl-nicks netsplit fill button match track
			completion readonly networks ring autojoin noncommands
			irccontrols move-to-prompt stamp menu list))
(setf erc-pcomplete-order-nickname-completions nil)


(setf erc-prompt  (lambda () (concat (buffer-name) ">")))

(defun my-erc-mode-hook-func ()
  (emojify-mode)
  (flyspell-mode 1)
  (erc-spelling-mode 1)
  (bind-key "M-RET" 'counsel-irc-query-nick erc-mode-map))

(add-hook 'erc-mode-hook 'my-erc-mode-hook-func)

;; This is important when using ZNC, otherwise different network's connections will use the same ERC server buffer and ERC will
;; reconnect every few minutes.
(setf erc-rename-buffers t)

(setf erc-save-buffer-on-part t)
(setf erc-log-channels-directory "~/.erc/logs/")
(setf erc-server-reconnect-timeout 5)
(setf erc-server-reconnect-attempts t)

(setf erc-track-exclude-types
      '("NICK" "333" "353"))

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
      erc-datestamp-format " === [%Y-%m-%d %a] ===\n" ; mandatory ascii art
      erc-fill-prefix "      "
      erc-insert-timestamp-function 'ks-timestamp)

(setf erc-auto-query 'window-noselect)

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
