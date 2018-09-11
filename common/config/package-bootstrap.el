;;; package-bootstrap --- Summary

;;; Commentary:

;;; Code:

(setf straight-recipes-gnu-elpa-use-mirror t)
(setf straight-recipes-gnu-elpa-url "https://github.com/emacs-straight/gnu-elpa-mirror")

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setf straight-use-package-by-default t)

(defun straight-reload-init ()
  "Reload init.el."
  (interactive)
  (straight-transaction
   (straight-mark-transaction-as-init)
   (message "Reloading init.el...")
   (load user-init-file nil 'nomessage)
   (message "Reloading init.el... done.")))

(defun straight-eval-buffer ()
  "Evaluate the current buffer as Elisp code."
  (interactive)
  (message "Evaluating %s..." (buffer-name))
  (straight-transaction
   (if (null buffer-file-name)
       (eval-buffer)
     (when (string= buffer-file-name user-init-file)
       (straight-mark-transaction-as-init))
     (load-file buffer-file-name)))
  (message "Evaluating %s... done." (buffer-name)))

(straight-use-package 'use-package)

(provide 'package-bootstrap)
;;; package-bootstrap.el ends here
