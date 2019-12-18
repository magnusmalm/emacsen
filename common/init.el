(setq gc-cons-threshold most-positive-fixnum)

(defvar status-fname (format "/tmp/emacs-status-%s" server-name))

(defvar *total-time* 0.0)

(defvar url-http-method nil)
(defvar url-http-data nil)
(defvar url-http-extra-headers nil)
(defvar oauth--token-data nil)
(defvar url-callback-function nil)
(defvar url-callback-arguments nil)

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  (let ((time (gensym))
	(diff (gensym)))
    `(let ((,time (current-time)))
       ,@body
       (let ((,diff (time-since ,time)))
	 (setf *total-time* (+ *total-time* (float-time ,diff)))
	 (message "%s - Time spent: %.06f (total: %.06f)" ',@body (float-time ,diff) *total-time*)))))

(defun display-startup-echo-area-message ()
  (message "Emacs loaded in %.01f seconds. Let the hacking begin!" *total-time*))

(defun my-log (msg &optional append)
  (let ((inhibit-message t))
    (write-region (format "%s\n" msg) nil status-fname append)
    (message "%s" msg)))

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

(setf custom-file (expand-file-name "config/custom.el" user-emacs-directory))

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
(measure-time (my-load "~/.secrets/emacs-secrets.el" t))

;; Setup package management
(measure-time (my-load "package-bootstrap"))
(measure-time (my-load "cfg-themes"))

;; The stuff we want for all servers
(measure-time (my-load "cfg-org"))
(measure-time (my-load "cfg-basics"))
(measure-time (my-load "cfg-visuals"))
(measure-time (my-load "cfg-utils"))
(measure-time (my-load "cfg-shell"))
(measure-time (my-load "cfg-windows"))
(measure-time (my-load "cfg-buffers"))
(measure-time (my-load "cfg-edit"))
(measure-time (my-load "cfg-magit"))

(measure-time (my-load "cfg-hide-mode-line"))

;; The stuff we want for general devel servers
(when (or (server-is "devel")
	  (server-is "lisp"))
  (measure-time (my-load "cfg-devel")))

;; The stuff we want for devel servers
(when (or (server-is "devel")
	  (server-is "lisp"))
  (measure-time (my-load "cfg-c-devel"))
  ;; (measure-time (my-load "cfg-rust-devel"))
  (measure-time (my-load "cfg-python-devel"))
  (measure-time (my-load "cfg-lisp-devel")))

;; The stuff we want for mail
;; (when (server-is "mail")
;; (my-load "cfg-mail")

;; The stuff we want for IRC
(when (server-is "irc")
  (measure-time (my-load "cfg-irc")))

;; Other random stuff
(measure-time (my-load "cfg-misc"))

;; Host specific configuration (cfg-<hostname>.el)
(measure-time (my-load (format "cfg-%s" (system-name))))

;; user specific configuration (cfg-<logged in name>.el)
(measure-time (my-load (format "cfg-%s" (user-login-name))))

;; Place custom stuff in separate file
(measure-time (my-load custom-file))

;; End Init
(message "Done loading init.el. Took %.06f" *total-time*)


(setq gc-cons-threshold 800000)

(defun package--save-selected-packages (&rest opt) nil)

(my-log (format "Running @ %s" (current-time-string)) t)
