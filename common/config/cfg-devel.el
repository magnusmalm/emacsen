(setq-default fill-column 100)

(use-package smerge-mode
  :straight nil
  :ensure nil
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

(use-package doom-todo-ivy
  :straight (:host github
		   :repo "jsmestad/doom-todo-ivy"))

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook (lambda () (prettify-symbols-mode -1))))

(use-package yaml-tomato)

(use-package eglot
  :config
  (defun my/dynamic-xref-apropos ()
    (interactive)
    (let ((buf (current-buffer)))
      (ivy-read "Search for pattern: "
		(lambda (str)
                  (cond
                   ((< (length str) 1) (counsel-more-chars 1))
                   (t
                    (with-current-buffer buf
                      (when-let ((backend (xref-find-backend)))
			(unless (eq backend 'etags)
                          (mapcar
                           (lambda (xref)
                             (let ((loc (xref-item-location xref)))
                               (propertize
				(concat
				 (when (xref-file-location-p loc)
                                   (with-slots (file line column) loc
                                     (format "%s:%s:%s:"
                                             (propertize (file-relative-name file)
							 'face 'compilation-info)
                                             (propertize (format "%s" line)
							 'face 'compilation-line
							 )
                                             column)))
				 (xref-item-summary xref))
				'xref xref)))
                           (xref-backend-apropos backend str))))))))
		:dynamic-collection t
		:action (lambda (item)
                          (xref--pop-to-location (get-text-property 0 'xref item))))))
  (add-hook 'prog-mode-hook 'eglot-ensure))

(setf gdb-many-windows t)

(setf compilation-scroll-output t)
(use-package bury-successful-compilation
  :config
  (bury-successful-compilation 1))

(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,
close the *compilation* buffer if the compilation is successful,
and set the focus back to Emacs frame"
  (if (string-match "^finished" msg)
      (progn
	(notify "Compilation" "Compilation Successful"))
    (notify "Compilation" "Compilation Failed")))

(add-to-list 'compilation-finish-functions
	     'notify-compilation-result)

(use-package flycheck
  :delight flymake-mode
  :demand t
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  :bind ("H-l" . hydra-flycheck/body)
  :config
  (setf flycheck-cppcheck-suppressions '("variableScope"))
  (setf flycheck-display-errors-delay 0.3)
  (setf flycheck-idle-change-delay 1.0)
  (setf flycheck-indication-mode 'left-fringe)
  (setf flycheck-display-errors-function nil)

  (use-package flycheck-pos-tip
    :config
    (setf flycheck-pos-tip-timeout (* 60 10))
    (flycheck-pos-tip-mode))

  (defhydra hydra-flycheck
    (:pre (progn (setf hydra-lv t) (flycheck-list-errors))
     :post (progn (setf hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
     :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("n"  flycheck-next-error                                       "Next")
    ("p"  flycheck-previous-error                                   "Previous")
    ("k"  flycheck-next-error                                       "Next")
    ("i"  flycheck-previous-error                                   "Previous")
    ("gg" flycheck-first-error                                      "First")
    ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil))
  (global-flycheck-mode))

;;;; INDENTING

(use-package aggressive-indent
  :delight
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'haml-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(use-package auto-indent-mode)

(use-package elf-mode)

(use-package dtrt-indent
  :delight
  :config
  (setf dtrt-indent-global-mode t)
  (setf dtrt-indent-ignore-single-chars-flag t)
  (setf dtrt-indent-min-quality 70.0)
  (setf dtrt-indent-run-after-smie t))

(use-package flycheck-inline
  :straight (:host github
	     :repo "flycheck/flycheck-inline")
  :config
  ;; (setf flycheck-inline-display-function 'flycheck-inline-display-phantom)
  ;; (setf flycheck-inline-clear-function 'flycheck-inline-clear-phantoms)
  (setf flycheck-inline-display-function
  	(lambda (msg pos)
          (let* ((ov (quick-peek-overlay-ensure-at pos))
  		 (contents (quick-peek-overlay-contents ov)))
            (setf (quick-peek-overlay-contents ov)
                  (concat contents (when contents "\n") msg))
            (quick-peek-update ov)))
  	flycheck-inline-clear-function #'quick-peek-hide)
  (global-flycheck-inline-mode))

(use-package gitlab)
(use-package ivy-gitlab)

(use-package lua-mode
  :config
  (setf lua-default-application "luajit"))

(use-package go-guru)


(use-package ivy-xref
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  :config
  (setf ivy-xref-remove-text-properties nil)
  (setf ivy-xref-use-file-path t)
  )
