;;; powerline-theme --- Magnus Malm's Theme for Powerline

;;; Commentary:

;;; Code:

(defun powerline-theme ()
  "Setup powerline mode-line theme."
  (interactive)
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let*
	  ((active (powerline-selected-window-active))
	   (mode-line-buffer-id
	    (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
	   (mode-line (if active 'mode-line 'mode-line-inactive))
	   (face1 (if active 'powerline-active1 'powerline-inactive1))
	   (face2 (if active 'powerline-active2 'powerline-inactive2))
	   (separator-left
	    (intern (format "powerline-%s-%s"
			    (powerline-current-separator)
			    (car powerline-default-separator-dir))))
	   (separator-right
	    (intern (format "powerline-%s-%s"
			    (powerline-current-separator)
			    (cdr powerline-default-separator-dir))))
	   (lhs (list (powerline-raw "%*" mode-line 'l)
		      (powerline-raw "%l" face1 'r)
		      (powerline-raw ":" face1)
		      (powerline-raw "%c" face1 'r)
		      (powerline-buffer-id mode-line-buffer-id 'l)
		      (powerline-raw " ")
		      (funcall separator-left mode-line face1)
		      (powerline-narrow face1 'l)
		      (powerline-vc face1)))
	   (rhs (list (powerline-raw global-mode-string face1 'r)
		      (powerline-buffer-size mode-line 'r)
		      (funcall separator-right face1 mode-line)
		      (powerline-raw " ")
		      (powerline-raw "%6p" mode-line 'r)
		      (powerline-hud face2 face1)))
	   (center (list (powerline-raw " " face1)
			 (funcall separator-left face1 face2)
			 (when (and (boundp 'erc-track-minor-mode)
				    erc-track-minor-mode)
			   (powerline-raw erc-modified-channels-object face2 'l))
			 (powerline-major-mode face2 'l)
			 (powerline-process face2)
			 (powerline-raw " :" face2)
			 (powerline-minor-modes face2 'l)
			 (powerline-raw " " face2)
			 (funcall separator-right face2 face1))))
	(concat (powerline-render lhs)
		(powerline-fill-center face1 (/ (powerline-width center) 2.0))
		(powerline-render center)
		(powerline-fill face1 (powerline-width rhs))
		(powerline-render rhs)))))))

(provide 'powerline-theme)

;;; powerline-theme.el ends here
