(use-package sly
  :straight (:files ("*.el" ("lib" "lib/*")
		     ("slynk/backend" "slynk/backend/*")
		     ("slynk" "slynk/*") ("contrib" "contrib/*")
		     "doc/*.texi" "doc/*.info" "doc/dir")
		    :host github :repo "joaotavora/sly")
  :config
  (setf inferior-lisp-program "sbcl"))

(use-package sly-quicklisp)
(use-package sly-macrostep)

(use-package elisp-docstring-mode)

(use-package common-lisp-snippets)
