(use-package py-autopep8)

(use-package pipenv
  :hook (python-mode . pipenv-mode))

(defun my-python-mode-hook-func ()
  (setf fill-column 79))

(add-hook 'python-mode-hook 'my-python-mode-hook-func)
