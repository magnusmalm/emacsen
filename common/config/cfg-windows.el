(use-package windmove
  :bind* (("C-M-i" . windmove-up)
	  ("C-M-k" . windmove-down)
	  ("C-M-j" . windmove-left)
	  ("C-M-l" . windmove-right))
  :config
  (setf windmove-wrap-around t))

(use-package switch-window
  :config
  (setf switch-window-shortcut-style 'qwerty)
  (setf switch-window-minibuffer-shortcut (string-to-char "m")))

(use-package windsize)

(use-package zygospore
  ;; (bind-key "H-f" 'zygospore-toggle-delete-other-windows)
  :bind (("M-1" . zygospore-toggle-delete-other-windows)
	 ("C-M-1" . zygospore-toggle-delete-other-windows))
  
  )


(bind-key "M->" 'split-window-vertically)
(bind-key "M-<" 'split-window-horizontally)

(bind-key "H-h" 'split-window-vertically)
(bind-key "H-v" 'split-window-horizontally)

(use-package ace-window
  :bind (("M-b" . ace-window)
	 ("C-M-K" . ace-swap-window)
	 ("C-M-O" . ace-window))
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
  (setf aw-keys '(?a ?s ?d ?f ?j ?k ?l))
  (setf aw-leading-char-style 'path))

(defun my-display-buffer (buffer alist)
  (require 'ace-window)
  (let ((aw-ignore-current (cdr (assq 'inhibit-same-window alist)))
        (aw-scope (pcase (cdr (assq 'reusable-frames alist))
                    ((pred not) 'frame)
                    ('visible 'visible)
                    (_ 'global))))
    (unless (<= (length (aw-window-list)) 1)
      (window--display-buffer
       buffer (aw-select "my-display-buffer") 'reuse))))

(setq display-buffer-base-action '((display-buffer-reuse-window
                                    my-display-buffer))
      display-buffer-alist `(,(cons "\\*helm" display-buffer-fallback-action)
                             ("magit-diff:" (my-display-buffer)
                              (inhibit-same-window . t))))

(defun delete-window-balance ()
  "Delete window and rebalance the remaining ones."
  (interactive)
  (delete-window)
  (balance-windows))

(defun split-window-below-focus ()
  "Split window horizontally and move focus to other window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-window-right-focus ()
  "Split window vertically and move focus to other window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(bind-key "H-=" #'delete-window-balance)
(bind-key "H-1" #'delete-other-windows)

(bind-key "H-h" #'split-window-below-focus)
(bind-key "H-2" #'split-window-below-focus)

(bind-key "H-v" #'split-window-right-focus)
(bind-key "H-3" #'split-window-right-focus)

(bind-key "H-0" #'window-toggle-split-direction)

(use-package minibuffer
  :straight nil
  :ensure nil
  :config
  (setf read-file-name-completion-ignore-case t)
  (setf completion-ignore-case t)
  (file-name-shadow-mode 1)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package windresize)


(customize-set-variable 'display-buffer-base-action
			'((display-buffer-reuse-window display-buffer-same-window)
			  (reusable-frames . t)))
;; avoid resizing
(customize-set-variable 'even-window-sizes nil)
