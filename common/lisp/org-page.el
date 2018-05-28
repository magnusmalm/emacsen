(use-package org-page
  :ensure t
  :config

  (defun th/op-publish ()
    (interactive)
    (op/do-publication t))

  (setq op/site-domain "blog.tinyhappybits.com/"
	op/repository-directory "~/blogging/tinyhappybits/repo"
	;; I had to adapt the default mdo theme and mustache resources so
	;; that they don't include prettify.js.  I also changed the CSS for
	;; code/pre a bit to have code listings a bit smaller.
	;; op/theme-root-directory "~/blogging/tinyhappybits/repo/themes"
	op/theme 'mdo
	op/repository-org-branch "master"   ;; default is source
	op/repository-html-branch "publish" ;; default is master
	;; The default `js' uses prettify.js for syntax highlighting which
	;; doesn't work nicely with Emacs Lisp (some words in the comments
	;; were highlighted but the non-commented code not at all...).
	op/highlight-render 'htmlize
	op/site-main-title "Hacking Life"
	op/site-sub-title "Zeros and ones and everything in-between."
	;; op/personal-github-link "https://github.com/tsdh"
	op/personal-disqus-shortname "tinyhappybits"))
