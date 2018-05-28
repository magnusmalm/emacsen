(setq gc-cons-threshold most-positive-fixnum)

(add-to-list 'load-path
	     (expand-file-name "config/" user-emacs-directory))

(setf custom-file
      (expand-file-name
       (concat "config/" "custom.el") user-emacs-directory))

;;;; Emacs version check
(when (or (< emacs-major-version 25)
	  (and (= emacs-major-version 25) (< emacs-minor-version 1)))
  (x-popup-dialog
   t `(,(format "Sorry, you need GNU Emacs version 25.1 or later
to use this Emacs config.
Your installed Emacs reports:
%s" (emacs-version))
       ("OK :(" . t)))
  (save-buffers-kill-emacs t))

(defun server-is (name)
  (string-equal name server-name))

;;; Loading rest of the config

;; Load user's secrets file
;; Ensure that the secrets file is not part of any public repo!
(load-file "~/.secrets/emacs-secrets.el")

;; Setup package management
(load "package-bootstrap")

;; Load the rest of the packages
(load "master.el")

;; (when (server-is "irc")
;;   (message "Loading IRC stuff...")
;;   (load "irc-erc.el"))

;; (when (server-is "mail")
;;   (message "Loading MAIL stuff...")
;;   (load "mail.el"))

;; (when (server-is "org")
;;   (message "Loading ORG stuff...")
;;   (load "org.el"))

;; Host specific configuration (config/setup-<hostname>.el)
;; (load (expand-file-name
;;        (concat "config/setup-" (system-name) ".el")
;;        user-emacs-directory))

;; user specific configuration (config/setup-<logged in name>.el)
;; (load (expand-file-name
;;        (concat "config/setup-" (user-login-name) ".el")
;;        user-emacs-directory))

;;;; Place custom stuff in separate file
(load custom-file)

;; (set-fontset-font t 'unicode "FontAwesome" nil 'prepend)

;;;; End Init
(message "Done loading init.el")

(setq gc-cons-threshold 800000)
