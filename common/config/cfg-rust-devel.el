;; Pre-requisites:
;; rustup default nightly
;; cargo component add rust-src
;; cargo +nightly install racer

;; Flycheck-Begin. On the fly syntax checking for multiple programming languages.
;; Enable Flycheck in buffers with supported languages.
(use-package flycheck-rust
  ;; :ensure t
  )

(use-package rust-mode
  ;; :ensure t
  :config
  (setq rust-format-on-save t))

(use-package racer
  :after company
  ;; :ensure t
  :config
  (progn
    (add-hook 'rust-mode-hook #'racer-mode) ;; Activate racer in rust buffers.
    (add-hook 'racer-mode-hook #'eldoc-mode) ;; Shows signature of current function in minibuffer.
    ;; Rust completions with Company and Racer.
    (add-hook 'racer-mode-hook #'company-mode)
    (add-hook 'racer-mode-hook
	      (lambda ()
		(setq-local company-tooltip-align-annotations t)))))

;; Enable Cargo minor mode allows us to do cargo commands
;; rust-mode and toml-mode
(use-package cargo
  ;; :ensure t
  :config
  (progn
    (add-hook 'rust-mode-hook 'cargo-minor-mode)
    (add-hook 'toml-mode-hook 'cargo-minor-mode)))

(add-hook 'after-init-hook #'global-flycheck-mode)

;; Flycheck Rust support.
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;; Flycheck-End.

;; Path to rust source.
(when (equal system-type 'gnu/linux)
  (setq racer-rust-src-path (concat (getenv "HOME") "/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")))
(when (equal system-type 'darwin)
  (setq racer-rust-src-path (concat (getenv "HOME") "/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src")))
;; Racer bin path.
(setq racer-cmd (concat (getenv "HOME") "/.cargo/bin/racer"))

;; Map TAB key to completions.
(local-set-key (kbd "TAB") 'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;; Rust-End.
