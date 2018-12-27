
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(require 'mu4e)
(require 'smtpmail)
(require 'gnus-dired)
(require'org-mu4e)

;; List of mail accounts.
(defvar my-mu4e-account-alist nil)
;; List of email signatures that can be added to a mail.
(defvar mu4e-mail-sigs nil)

(setq org-mu4e-link-query-in-headers-mode nil)
(setf mu4e-maildir "~/.mail")
(setf mu4e-completing-read-function 'ivy-completing-read)

;; Directory to save attachments
(setq mu4e-attachment-dir
      (lambda (fname mtype)
	(cond
	 ((and fname (string-match "\\.vcs$" fname))  "~/sync/org/meetings")
	 ((and fname (string-match "\\.pdf$" fname))  "~/sync/Documents")
	 ;; ... other cases  ...
	 (t "~/sync/Downloads"))))

(define-key mu4e-main-mode-map "u" 'mu4e-update-index)

(load-file "~/.secrets/emacs-mail.el")

;; Shortcuts to often visited mailboxes
(setq mu4e-maildir-shortcuts
      '( ("/gmail/archive" . ?A)
	 ("/gmail/drafts" . ?D)
	 ("/gmail/inbox" . ?I)
	 ("/gmail/sent" . ?S)
	 ("/gmail/trash" . ?T)

	 ("/work/archive" . ?a)
	 ("/work/drafts" . ?d)
	 ("/work/inbox"  . ?i)
	 ("/work/sent" . ?s)
	 ("/work/trash" . ?t)))

(setf mu4e-compose-complete-only-personal t)
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
	 (:from-or-to . 35)
	 (:subject . nil)))

;; enable inline images
(setf mu4e-view-show-images t)

(setq mu4e-context-policy 'pick-first)

(setq mu4e-compose-context-policy nil)

(setq message-kill-buffer-on-exit t)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(add-hook 'mu4e-compose-mode-hook
	  (defun my-do-compose-stuff ()
	    "My settings for message composition."
	    (set-fill-column 72)
	    (super-save-mode -1)
	    (flyspell-mode)))

(setf mu4e-compose-signature-auto-include t)

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
