(defun daytimep ()
  (sunrise-sunset)
  (let ((range (mapcar
		'car
		(butlast (solar-sunrise-sunset (calendar-current-date))))))
    (let ((sunrise (car range))
	  (sunset (cadr range))
	  (now (string-to-number (format-time-string "%H"))))
      (< sunrise now sunset))))


;; Based on https://emacs.stackexchange.com/a/26981
(setf mmm/themes '(solarized-light solarized-dark zenburn))
(setf mmm/themes-index 2)

(defun mmm/cycle-theme ()
  (interactive)
  (setf mmm/themes-index (% (1+ mmm/themes-index) (length mmm/themes)))
  (mmm/load-indexed-theme))
(bind-key "<f5>" 'mmm/cycle-theme)

(defun mmm/load-indexed-theme ()
  (mmm/try-load-theme (nth mmm/themes-index mmm/themes)))

(defun mmm/try-load-theme (theme)
  (if (ignore-errors (load-theme theme :no-confirm))
      (mapcar #'disable-theme (remove theme custom-enabled-themes))))

(defun mmm/load-theme-dwim ()
  (if (daytimep)
      (setf mmm/themes-index (cl-position 'solarized-light mmm/themes))
    (setf mmm/themes-index (cl-position 'solarized-dark mmm/themes)))
  (mmm/load-indexed-theme))

(use-package zenburn-theme)
(use-package solarized-theme)

;; Set solarized light if daylight, otherwise
;; solarized dark
;; (mmm/load-theme-dwim)

(mmm/load-indexed-theme)

(provide 'cfg-themes)
