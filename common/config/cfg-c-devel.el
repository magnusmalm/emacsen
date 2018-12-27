(use-package cc-mode
  :config
  (defun reformat-region (&optional b e)
    (interactive "r")
    (when (not (buffer-file-name))
      (error "A buffer must be associated with a file in order to use REFORMAT-REGION."))
    (when (not (executable-find "clang-format"))
      (error "clang-format not found."))
    (shell-command-on-region b e
			     "clang-format"
			     (current-buffer) t)
    (indent-region b e))

  (defun insert-semicolon ()
    "Add semicolon at the end of the line and return to current position"
    (interactive)
    (end-of-line)
    (insert ";")
    (c-newline-and-indent))
  :bind (:map c-mode-base-map ("M-RET" . insert-semicolon)))

(defun my-c-mode-hook-func ()
  ;; (semantic-mode 1)
  (rainbow-identifiers-mode -1)
  (setf fill-column 132)
  (yas-minor-mode)
  (electric-pair-mode 1)
  (company-mode 1)
  (key-chord-mode 1)
  (ws-butler-mode 1)
  )
(add-hook 'c-mode-hook 'my-c-mode-hook-func)
