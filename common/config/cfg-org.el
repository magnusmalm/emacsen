;; Hack to make loading latest org mode work.
;; org-git-version, org-release
(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
		   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
	      "--match=release\*"
	      "--abbrev=6"
	      "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
		   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
	       "--match=release\*"
	       "--abbrev=0"
	       "HEAD")))))

(provide 'org-version)

(use-package org
  :init
  (defun load-org-agenda-files-recursively (dir) "Find all directories in DIR."
	 (unless (file-directory-p dir)
	   (error "Not a directory `%s'" dir))
	 (unless (equal (directory-files dir nil org-agenda-file-regexp t) nil)
	   (add-to-list 'org-agenda-files dir))
	 (dolist (file (directory-files dir nil nil t))
	   (unless (member file '("." ".."))
	     (let ((file (concat dir file "/")))
	       (when (file-directory-p file)
		 (load-org-agenda-files-recursively file))))))
  :config
  (setq org-agenda-files '("~/sync/org/work.org"
			   "~/sync/org/private.org"
			   "~/sync/org/solkatten.org"
			   "~/sync/org/wemo_cal/"
			   ))
	;; (load-org-agenda-files-recursively "~/sync/org/") ; trailing slash required
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (emacs-lisp . t)
     (lisp . t)
     (python . t)
     (ruby . t)
     (shell . t)
     (ditaa . t)
     (C . t)
     ))
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))

  ;;  set to nil to use symmetric encryption.
  (setq org-crypt-key nil)

  ;; Global Tags
  (setf org-tag-alist '((:startgroup . nil)
			("@work" . ?w)
			("@home" . ?h)
			("@computer" . ?l)
			("@mobile" . ?p)
			(:endgroup . nil)
			("ttdp" . ?t)
			("config" . ?c)
			("emacs" . ?E)
			("org" . ?o)
			("meeting" . ?M)
			("household" . ?H)
			("economy" . ?e)
			("crypt" . ?y)))
)


(use-package oauth2)



;; (use-package org-caldav
;;   :init
;;   ;; This is the sync on close function; it also prompts for save after syncing so
;;   ;; no late changes get lost
;;   (defun org-caldav-sync-at-close ()
;;     (org-caldav-sync)
;;     (save-some-buffers))

;;   ;; This is the delayed sync function; it waits until emacs has been idle for
;;   ;; "secs" seconds before syncing.  The delay is important because the caldav-sync
;;   ;; can take five or ten seconds, which would be painful if it did that right at save.
;;   ;; This way it just waits until you've been idle for a while to avoid disturbing
;;   ;; the user.
;;   (defvar org-caldav-sync-timer nil
;;     "Timer that `org-caldav-push-timer' used to reschedule itself, or nil.")
;;   (defun org-caldav-sync-with-delay (secs)
;;     (when org-caldav-sync-timer
;;       (cancel-timer org-caldav-sync-timer))
;;     (setq org-caldav-sync-timer
;; 	  (run-with-idle-timer
;; 	   (* 1 secs) nil 'org-caldav-sync)))

;;   ;; Actual calendar configuration edit this to meet your specific needs
;;   ;; (setf org-caldav-uuid-extension ".EML")
;;   (setq org-caldav-calendars
;; 	'(;; (:url google
;; 	  ;;  :calendar-id "Main"
;; 	  ;;  :files ("~/sync/org/google_cal/main.org")
;; 	  ;;  :inbox "~/sync/org/google_cal/main_inbox.org")
;; 	  ;; (:url 'google
;; 	  ;;  :calendar-id "Övrigt"
;; 	  ;;  :files ("~/sync/org/google_cal/övrigt.org")
;; 	  ;;  :inbox "~/sync/org/google_cal/main_övrigt.org")
;; 	  (:url "http://localhost:1080/users/"
;; 	   :calendar-id "magnus.malm@westermo.se/calendar"
;; 	   :files ("~/sync/org/wemo_cal/main.org")
;; 	   :inbox "~/sync/org/wemo_cal/main-inbox.org")))
;;   (setq org-caldav-backup-file "~/sync/org/caldav/org-caldav-backup.org")
;;   (setq org-caldav-save-directory "~/sync/org/caldav/")
;;   (setf org-caldav-uuid-extension ".EML")

;;   :config
;;   (setq org-caldav-oauth2-providers
;;       '((google
;;          "https://accounts.google.com/o/oauth2/v2/auth"
;;          "https://www.googleapis.com/oauth2/v4/token"
;;          "https://www.googleapis.com/auth/calendar"
;;          "https://apidata.googleusercontent.com/caldav/v2/%s/events")))
;;   (setq org-icalendar-alarm-time 1)
;;   ;; This makes sure to-do items as a category can show up on the calendar
;;   (setq org-icalendar-include-todo t)
;;   ;; This ensures all org "deadlines" show up, and show up as due dates
;;   (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due))
;;   ;; This ensures "scheduled" org items show up, and show up as start times
;;   (setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
;;   (setq org-icalendar-timezone "Europe/Stockholm")
;;   (setq org-icalendar-date-time-format ";TZID=%Z:%Y%m%dT%H%M%S")
;;   ;; Add the delayed save hook with a five minute idle timer
;;   (add-hook 'after-save-hook
;; 	    (lambda ()
;; 	      (when (eq major-mode 'org-mode)
;; 		(org-caldav-sync-with-delay 300))))
;;   ;; Add the close emacs hook
;;   ;; (add-hook 'kill-emacs-hook 'org-caldav-sync-at-close)
;;   )
