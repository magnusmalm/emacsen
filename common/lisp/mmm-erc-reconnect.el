(defun mmm/erc-open-server-buffer-p (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (and (erc-server-buffer-p)
	 (erc-server-process-alive))))

(defun mmm/erc-reconnect-server-buffer (buffer)
  (with-current-buffer buffer
    (erc-server-reconnect)))

(defun mmm/erc-reconnet-all ()
  (interactive)
  (mapcar #'mmm/erc-reconnect-server-buffer
	  (cl-remove-if-not #'mmm/erc-open-server-buffer-p
			    (erc-buffer-list #'erc-server-buffer-p))))

(provide 'mmm/erc-reconnect)
