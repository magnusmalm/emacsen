(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(use-package mu4e
  :straight nil
  :bind ("<f1>" . mu4e)
  :preface
  (defadvice mu4e (before mu4e-start activate)
    (when (> 1 (count-windows))
      (window-configuration-to-register :mu4e-fullscreen)
      (delete-other-windows)))

  (defadvice mu4e-quit (after mu4e-close-and-push activate)
    (start-process "pushmail" "*pushmail-mbsync*" "mbsync" "-a" "--push")
    (when (get-register :mu4e-fullscreen)
      (jump-to-register :mu4e-fullscreen)))
  :init
  (require 'mu4e-contrib)
  (load-file "~/.secrets/emacs-mail.el")
  (setq mail-user-agent 'mu4e-user-agent
        ;; message-citation-line-format "\nEl %A %d de %B del %Y a las %H%M horas, %N escribiÃ³:\n"
        message-citation-line-function 'message-insert-formatted-citation-line
        message-cite-reply-position 'below
        message-kill-buffer-on-exit t

	message-send-mail-function 'message-send-mail-with-sendmail

	;; Directory to save attachments
	mu4e-attachment-dir
	(lambda (fname mtype)
	  (cond
	   ((and fname (string-match "\\.vcs$" fname))  "~/sync/org/meetings")
	   ((and fname (string-match "\\.pdf$" fname))  "~/sync/Documents")
	   ;; ... other cases  ...
	   (t "~/sync/Downloads")))

        mu4e-auto-retrieve-keys t
        mu4e-compose-context-policy 'ask
        mu4e-compose-dont-reply-to-self t
        mu4e-compose-keep-self-cc nil
        mu4e-context-policy 'pick-first
        mu4e-headers-date-format "%Y-%m-%d %H:%M"
        mu4e-headers-include-related t
        mu4e-headers-auto-update nil
        mu4e-headers-leave-behavior 'ignore
        mu4e-headers-visible-lines 8
        mu4e-headers-fields '((:date . 25)
                              (:flags . 6)
                              (:from . 22)
                              (:subject . nil))
        mu4e-view-prefer-html t
	mu4e-html2text-command "html2text"
        ;; mu4e-html2text-command "w3m -dump -T text/html -cols 72 -o display_link_number=true -o auto_image=false -o display_image=true -o ignore_null_img_alt=true"
        mu4e-maildir "/home/magnus/.mail"
        mu4e-view-show-images t
        ;; sendmail-program "msmtp"
        mu4e-get-mail-command "mbsync -aV")

  (defun mu4e-message-maildir-matches (msg rx)
    (when rx
      (if (listp rx)
          ;; If rx is a list, try each one for a match
          (or (mu4e-message-maildir-matches msg (car rx))
              (mu4e-message-maildir-matches msg (cdr rx)))
        ;; Not a list, check rx
        (string-match rx (mu4e-message-field msg :maildir)))))

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

  (run-at-time nil (* 60 5) 'mu4e-update-mail-and-index t)

  (bind-key "C-c c" 'org-mu4e-store-and-capture mu4e-headers-mode-map)
  (bind-key "C-c c" 'org-mu4e-store-and-capture mu4e-view-mode-map))

(use-package mu4e-alert
  :if (executable-find "mu")
  :init
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (setq mu4e-compose-forward-as-attachment t
        mu4e-compose-crypto-reply-encrypted-policy 'sign-and-encrypt
        mu4e-compose-crypto-reply-plain-policy 'sign
        mu4e-index-update-in-background t
        mu4e-alert-email-notification-types '(subjects))
  :config
  (defun conf:refresh-mu4e-alert-mode-line ()
    (interactive)
    (mu4e~proc-kill)
    (mu4e-alert-enable-mode-line-display))
  (run-with-timer 0 60 'conf:refresh-mu4e-alert-mode-line)
  (mu4e-alert-set-default-style 'libnotify))

(use-package mu4e-alert
  :if (executable-find "mu")
  :init
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (setq mu4e-compose-forward-as-attachment t
        mu4e-compose-crypto-reply-encrypted-policy 'sign-and-encrypt
        mu4e-compose-crypto-reply-plain-policy 'sign
        mu4e-index-update-in-background t
        mu4e-alert-email-notification-types '(subjects))
  :config
  (defun conf:refresh-mu4e-alert-mode-line ()
    (interactive)
    (mu4e~proc-kill)
    (mu4e-alert-enable-mode-line-display))
  (run-with-timer 0 60 'conf:refresh-mu4e-alert-mode-line)
  (mu4e-alert-set-default-style 'libnotify))

;; This overloads the amazing C-c C-c commands in org-mode with one more function
;; namely the htmlize-and-send, above.
(add-hook 'org-ctrl-c-ctrl-c-hook 'htmlize-and-send t)

;; Originally, I set the `mu4e-compose-mode-hook' here, but
;; this new hook works much, much better for me.
(add-hook 'mu4e-compose-post-hook
          (defun do-compose-stuff ()
            "My settings for message composition."
            (org-mu4e-compose-org-mode)))

(use-package mu4e-conversation
  :after mu4e
  :config
  (setq mu4e-conversation-print-function 'mu4e-conversation-print-tree
        mu4e-compose-dont-reply-to-self t
        mu4e-conversation-kill-buffer-on-exit t))

(use-package mu4e-maildirs-extension
  :after mu4e
  :config (mu4e-maildirs-extension))
