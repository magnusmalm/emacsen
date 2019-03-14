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
  ;; (electrci-pair-mode 1)
  (company-mode 1)
  (key-chord-mode 1)
  (ws-butler-mode 1)
  (setq company-transformers nil)
  )
(add-hook 'c-mode-hook 'my-c-mode-hook-func)

;; (defun eglot-ccls-inheritance-hierarchy (&optional derived)
;;   "Show inheritance hierarchy for the thing at point.
;; If DERIVED is non-nil (interactively, with prefix argument), show
;; the children of class at point."
;;   (interactive "P")
;;   (if-let* ((res (jsonrpc-request
;;                   (eglot--current-server-or-lose)
;;                   :$ccls/inheritance
;;                   (append (eglot--TextDocumentPositionParams)
;;                           `(:derived ,(if derived t :json-false))
;;                           '(:levels 100) '(:hierarchy t))))
;;             (tree (list (cons 0 res))))
;;       (with-help-window "*ccls inheritance*"
;;         (with-current-buffer standard-output
;;           (while tree
;;             (pcase-let ((`(,depth . ,node) (pop tree)))
;;               (cl-destructuring-bind (&key uri range) (plist-get node :location)
;;                 (insert (make-string depth ?\ ) (plist-get node :name) "\n")
;;                 (make-text-button (+ (point-at-bol 0) depth) (point-at-eol 0)
;;                                   'action (lambda (_arg)
;;                                             (interactive)
;;                                             (find-file (eglot--uri-to-path uri))
;;                                             (goto-char (car (eglot--range-region range)))))
;;                 (cl-loop for child across (plist-get node :children)
;;                          do (push (cons (1+ depth) child) tree)))))))
;;     (eglot--error "Hierarchy unavailable")))

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
