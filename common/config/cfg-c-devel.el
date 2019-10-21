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
    (insert ";"))
  :bind (:map c-mode-base-map ("C-RET" . insert-semicolon)))

(defun my-c-mode-hook-func ()
  (setf fill-column 132)
  ;; (yas-minor-mode)
  ;; (electrci-pair-mode 1)
  ;; (company-mode 1)
  ;; (flycheck-mode 1)
  (key-chord-mode 1)
  (ws-butler-mode 1)
  (setq company-transformers nil)
  (bind-key "M-j" 'backward-char c-mode-map)
  (bind-key "M-e" 'backward-kill-word c-mode-map)
  (push 'company-lsp company-backends)
  )

(add-hook 'c-mode-hook 'my-c-mode-hook-func)


;; This gives a regular `compile-command' prompt.
(define-key prog-mode-map [C-f9] #'compile)

;; This just compiles immediately.
(define-key prog-mode-map [f9]
  #'endless/compile-please)

;; I'm not scared of saving everything.
(setq compilation-ask-about-save nil)

;; Stop on the first error.
(setq compilation-scroll-output 'next-error)

;; Don't stop on info or warnings.
(setq compilation-skip-threshold 2)

(defcustom endless/compile-window-size 105
  "Width given to the non-compilation window."
  :type 'integer
  :group 'endless)

(defun endless/compile-please (comint)
  "Compile without confirmation.
With a prefix argument, use comint-mode."
  (interactive "P")
  ;; Do the command without a prompt.
  (save-window-excursion
    (compile (eval compile-command) (and comint t)))
  ;; Create a compile window of the desired width.
  (pop-to-buffer (get-buffer "*compilation*"))
  (enlarge-window
   (- (frame-width)
      endless/compile-window-size
      (window-width))
   'horizontal))
