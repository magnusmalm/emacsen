
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

(defun app-launcher ()
  (interactive)
  (with-current-buffer (get-buffer-create "*modal-ivy*")
    (let ((frame (make-frame '((auto-raise . t)
                               (left-fringe . 0)
                               (line-spacing . 3)
                               (menu-bar-lines . 0)
                               (minibuffer . only)
                               (right-fringe . 0)
                               (undecorated . t)
                               (unsplittable . t)
                               (vertical-scroll-bars . nil)))))
      (let ((ivy-height 20)
            (ivy-count-format ""))
	(ivy-read "Emacs acronyms: "
                  '(" Emacs: Escape-Meta-Alt-Control-Shift "
                    " Emacs: Eight Megabytes And Constantly Swapping "
                    " Emacs: Even a Master of Arts Comes Simpler ")
                  :action (lambda (funny-quote)
                            (async-shell-command (format "notify-send \"Test\" %s" funny-quote)))
                  :unwind (lambda ()
                            (shell-command "notify-send \"Test\" done &")
                            (delete-frame)
                            (other-window 1)))))))

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

(setf temp-buffer-resize-mode t)


(use-package autorevert
  :commands auto-revert-mode
  :blackout auto-revert-mode
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
  :bind (("C-s-b" . broadcast-mode)))

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

(use-package vlf
  :config
  (setf vlf-application 'dont-ask)
  (setf vlf-tune-enabled t)
  (require 'vlf-setup))

(use-package uniquify
  :straight nil
  :ensure nil
  :config
  (setf uniquify-buffer-name-style 'forward))

;;;; *scratch* buffer
(setf initial-scratch-message nil)
(setf initial-major-mode 'text-mode)


;; DIRED
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

(use-package dired-hacks-utils)
