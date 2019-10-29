(use-package windmove
  :bind* (("H-i" . windmove-up)
	  ("H-k" . windmove-down)
	  ("H-j" . windmove-left)
	  ("H-l" . windmove-right))
  :config
  (setf windmove-wrap-around t))

(use-package switch-window
  :config
  (setf switch-window-shortcut-style 'qwerty)
  (setf switch-window-minibuffer-shortcut (string-to-char "m")))

(use-package windsize)

(use-package zygospore
  :bind* ("M-1" . zygospore-toggle-delete-other-windows))


(bind-key "M->" 'split-window-vertically)
(bind-key "M-<" 'split-window-horizontally)

(bind-key "H-h" 'split-window-vertically)
(bind-key "H-v" 'split-window-horizontally)

(bind-key "H-f" 'zygospore-toggle-delete-other-windows)

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
  (setf aw-keys '(?a ?s ?d ?f ?j ?k ?l))
  (setf aw-leading-char-style 'path))

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

(bind-key "s-=" #'delete-window-balance)
(bind-key "s-1" #'delete-other-windows)
(bind-key "s-2" #'split-window-below-focus)
(bind-key "s-3" #'split-window-right-focus)

;; Window layout management
;; (use-package eyebrowse
;;   :defer 1
;;   :config
;;   (setf eyebrowse-close-window-config-prompt t)
;;   (setf eyebrowse-mode-line-left-delimiter "{")
;;   (setf eyebrowse-mode-line-right-delimiter "}")
;;   (setf eyebrowse-mode-line-separator "|")
;;   (setf eyebrowse-new-workspace nil)
;;   (setf eyebrowse-switch-back-and-forth t)
;;   (setf eyebrowse-wrap-around t)
;;   (setq-default eyebrowse-new-workspace t)
;;   (eyebrowse-mode 1))

(use-package minibuffer
  :straight nil
  :ensure nil
  :config
  (setf read-file-name-completion-ignore-case t)
  (setf completion-ignore-case t)
  (setf resize-mini-windows t)
  (file-name-shadow-mode 1)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))
