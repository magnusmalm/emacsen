(setq gc-cons-threshold most-positive-fixnum)

(defvar status-fname (format "/tmp/emacs-status-%s" server-name))

(defun my-log (msg &optional append)
  (write-region (format "%s\n" msg) nil status-fname append))

(defun server-is (name)
  (string-equal name server-name))

(defun my-load (file &optional absolute-path)
  (let ((f (if absolute-path file (expand-file-name (concat "config/" file ".el") user-emacs-directory))))
    (when (file-exists-p f)
      (my-log (format "Loading %s @ %s" file (current-time-string)) t)
      (load file nil t))))

(my-log (format "Startup @ %s" (current-time-string)))

(setf load-prefer-newer t)

(add-to-list 'load-path
	     (expand-file-name "config/" user-emacs-directory))

(setf custom-file "custom.el")

;;;; Emacs version check
(when (or (< emacs-major-version 26)
	  (and (= emacs-major-version 26) (< emacs-minor-version 1)))
  (x-popup-dialog
   t `(,(format "Sorry, you need GNU Emacs version 26.1 or later
to use this Emacs config.
Your installed Emacs reports:
%s" (emacs-version))
       ("OK :(" . t)))
  (save-buffers-kill-emacs t))

;; Load user's secrets file
;; Ensure that the secrets file is not part of any public repo!
(my-load "~/.secrets/emacs-secrets.el" t)

;; Setup package management
(my-load "package-bootstrap")

;; The stuff we want for all servers
(my-load "cfg-basics")
(my-load "cfg-themes")
(my-load "cfg-visuals")
(my-load "cfg-utils")
(my-load "cfg-shell")
(my-load "cfg-windows")
(my-load "cfg-buffers")
(my-load "cfg-edit")
(my-load "cfg-magit")
(my-load "cfg-org")

;; The stuff we want for general devel servers
(when (or (server-is "devel")
	  (server-is "lisp"))
  (my-load "cfg-devel"))

;; The stuff we want for C and python lang devel servers
(when (server-is "devel")
  (my-load "cfg-c-devel")
  (my-load "cfg-python-devel"))

;; The stuff we want for Lisp devel servers
(when (server-is "lisp")
  (my-load "cfg-lisp"))

;; The stuff we want for mail
(when (server-is "mail")
  (my-load "cfg-mail"))

;; The stuff we want for IRC
(when (server-is "irc")
  (my-load "cfg-irc"))

;; All my keybindings (based on ErgoKeys)
(my-load "cfg-keys")

;; Other random stuff
(my-load "cfg-misc")

;; Host specific configuration (cfg-<hostname>.el)
(my-load (format "cfg-%s" (system-name)))

;; user specific configuration (cfg-<logged in name>.el)
(my-load (format "cfg-%s" (user-login-name)))

;; Place custom stuff in separate file
(my-load custom-file)

;; End Init
(message "Done loading init.el")

(setq gc-cons-threshold 800000)

(defun package--save-selected-packages (&rest opt) nil)

(my-log (format "Running @ %s" (current-time-string)) t)
