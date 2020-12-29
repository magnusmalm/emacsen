(use-package py-autopep8)

(use-package pipenv
  :hook (python-mode . pipenv-mode))

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package pyvenv)

(defun my-python-mode-hook-func ()
  (setf fill-column 79)
  ;; (prism-whitespace-mode 1)
  )

(add-hook 'python-mode-hook 'my-python-mode-hook-func)

(use-package indent-tools)
