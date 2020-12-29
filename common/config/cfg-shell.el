(bind-key "M-A" 'shell-command)

(setf shell-file-name "bash")
(add-to-list 'exec-path "/usr/local/bin")

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "HISTFILE")
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package vterm
  :bind (:map vterm-mode-map
	 ("M-b" . ace-window)
	 ("M-h" . vterm-send-C-a)))

;; (use-package vterm-toggle
;;   :bind (("H-s" . vterm-toggle))
;;   :config
;;   (setq vterm-toggle-fullscreen-p nil)
;;   (add-to-list 'display-buffer-alist
;; 	       '("^.*v?term.*"
;; 		 (display-buffer-reuse-window display-buffer-at-bottom)
;; 		 ;;(display-buffer-reuse-window display-buffer-in-direction)
;; 		 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
;; 		 ;;(direction . bottom)
;; 		 ;;(dedicated . t) ;dedicated is supported in emacs27
;; 		 (reusable-frames . visible)
;; 		 (window-height . 0.2)))
;;   )

;; (add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;; (use-package multi-libvterm
;;   :straight (:host github
;; 	     :repo "suonlight/multi-libvterm"))
