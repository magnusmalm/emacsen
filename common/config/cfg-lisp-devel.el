(use-package sly
  :config
  (setf inferior-lisp-program "sbcl"))

(use-package sly-quicklisp
  :requires sly)

(use-package sly-named-readtables
  :requires sly)

(use-package sly-asdf
  :requires sly)
