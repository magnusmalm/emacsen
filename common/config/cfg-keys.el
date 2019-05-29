;; Source: https://www.reddit.com/r/emacs/comments/89yofa/keychord_mode/
(use-package key-chord
  :ensure nil
  :config
  (key-chord-mode 1)
  (key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
  (key-chord-define-global "UU" 'undo-tree-visualize)
  (key-chord-define-global "ZZ" 'undo)
  (key-chord-define-global "XX" 'counsel-M-x)
  (key-chord-define-global "yy" 'popup-kill-ring)
  (key-chord-define-global "YY" 'counsel-yank-pop)
  (key-chord-define-global "MM" 'magit-file-popup)
  (key-chord-define-global "QQ" 'hydra-avy/body)
  (key-chord-define-global "  " ". ")

  ;; This create a prefix keymap
  ;; https://stackoverflow.com/questions/25473660/how-do-i-use-a-key-chord-combination-as-a-prefix-binding

  (let
      ((sub-keymap (make-sparse-keymap)))
    (define-key sub-keymap "B" 'bm-toggle)
    (define-key sub-keymap "N" 'bm-next)
    (define-key sub-keymap "P" 'bm-previous)
    (define-key sub-keymap "L" 'bm-show-all)
    (define-key sub-keymap "K" 'bm-remove-all-all-buffers)
    (key-chord-define-global "BB" sub-keymap))

  (let
      ((sub-keymap (make-sparse-keymap)))
    (define-key sub-keymap "L" 'link-hint-open-link)
    (define-key sub-keymap "C" 'link-hint-copy-link)
    (key-chord-define-global "LL" sub-keymap))

  (let
      ((sub-keymap (make-sparse-keymap)))
    (define-key sub-keymap "p" 'counsel-projectile-switch-project)
    (define-key sub-keymap "f" 'counsel-projectile-find-file)
    (define-key sub-keymap "s" 'counsel-projectile-rg)
    (define-key sub-keymap "c" 'projectile-compile-project)
    (define-key sub-keymap "b" 'projectile-switch-to-buffer)
    (define-key sub-keymap "d" 'projectile-dired)
    (define-key sub-keymap "m" 'magit-status)
    (define-key sub-keymap "M" 'magit-status)
    (define-key sub-keymap "w" 'git-messenger:popup-message)
    (define-key sub-keymap "l" 'multi-line)
    (key-chord-define-global "WW" sub-keymap))

  (let
      ((sub-keymap (make-sparse-keymap)))
    (define-key sub-keymap "a" "!")
    (define-key sub-keymap "q" "@")
    (define-key sub-keymap "s" "\"")
    (define-key sub-keymap "f" "#")
    (define-key sub-keymap "d" "$")
    (define-key sub-keymap "g" "%")
    (define-key sub-keymap "h" "&")
    (define-key sub-keymap "j" "/")
    (define-key sub-keymap "i" "\\")
    (define-key sub-keymap "k" "\\")
    (define-key sub-keymap "u" "'")
    (define-key sub-keymap "l" "*")
    (define-key sub-keymap "v" "|")
    (define-key sub-keymap "o" "|")
    (define-key sub-keymap "ö" "'")
    (define-key sub-keymap "c" 'compile)
    (define-key sub-keymap "p" 'pwd)
    (key-chord-define-global "II" sub-keymap)))


;;;; KEYBINDINGS
(bind-key "C-S-O" 'find-file-in-config-dir)
(bind-key "C-S-M-O" 'find-file-in-sync-dir)
(bind-key "C-S-n" 'new-empty-buffer)
(bind-key* "C-w" 'close-current-buffer)
(bind-key "C-s" 'save-buffer)
(bind-key "C-S-s" 'write-file)
(bind-key "C-a" 'mark-whole-buffer)
(bind-key "C-<prior>" 'previous-emacs-buffer)
(bind-key "C-<next>" 'next-emacs-buffer)
(bind-key "M-<prior>" 'previous-user-buffer)
(bind-key "M-<next>" 'next-user-buffer)
(bind-key "C-x C-b" 'ibuffer)
(bind-key "H-c" 'compile)
(bind-key " " 'compile)
(bind-key "M-d" 'hungry-delete-backward)
(bind-key "M-f" 'hungry-delete-forward)
(bind-key "M-e" 'backward-kill-word)
(bind-key "M-r" 'kill-word)
(bind-key* "M-x" 'xah-cut-line-or-region)
(bind-key* "M-c" 'xah-copy-line-or-region)
(bind-key* "M-v" 'yank)
(bind-key "M-/" 'toggle-letter-case)
(bind-key "M-q" 'fill-paragraph)
(unbind-key (kbd "C-z")) ; suspend-frame
(unbind-key (kbd "s-p")) ; ns-print-buffer
(unbind-key (kbd "s-q")) ; save-buffers-kill-emacs
(unbind-key (kbd "s-t")) ; ns-popup-font-panel
(unbind-key (kbd "C-x C-c")) ; save-buffers-kill-terminal
(bind-key "H-r" 'repeat)
(bind-key "M-A" 'shell-command)
(bind-key "C-c C-u" 'mmm:uuid)
(bind-key* "M-j" 'backward-char)
(bind-key* "M-l" 'forward-char)
(bind-key* "M-i" 'previous-line)
(bind-key* "M-k" 'next-line)
(bind-key* "M-u" 'backward-word)
(bind-key* "M-o" 'forward-word)
(bind-key* "M-J" 'backward-paragraph)
(bind-key* "M-L" 'forward-paragraph)
(bind-key* "M-h" 'crux-move-beginning-of-line)
(bind-key* "M-H" 'move-end-of-line)
(bind-key "M-I" 'scroll-down-command)
(bind-key "M-K" 'scroll-up-command)
(bind-key "M-U" 'beginning-of-buffer)
(bind-key "M-O" 'end-of-buffer)
(bind-key* "M-g" 'goto-line)
(bind-key* "M-S-SPC" 'mark-paragraph)
(bind-key* "M-SPC" 'set-mark-command)

;; comint mode (shell)
(bind-key "M-r" 'kill-word comint-mode-map)
(bind-key "M-e" 'backward-kill-word comint-mode-map)
(bind-key "C-d" 'eshell-send-eof-to-process comint-mode-map)
(bind-key "\C-i" 'endless/ispell-word-then-abbrev ctl-x-map)

;; C-x C-0 restores the default font size
(bind-key* "C-+" 'default-text-scale-increase)
(bind-key* "C--" 'default-text-scale-decrease)
(bind-key* "C-=" 'default-text-scale-reset)
(bind-key* "C-x C-0" 'modi/global-font-size-reset)
(bind-key "C-S-w" 'delete-frame)
(bind-key "M-Z" 'zap-to-char)
(bind-key "M-z" 'undo)
(bind-key "C-z" 'undo)

(bind-key "M->" 'split-window-vertically)
(bind-key "M-<" 'split-window-horizontally)

;; (bind-key "C-<up>" 'gcm-scroll-up)
;; (bind-key "C-<down>" 'gcm-scroll-down)

(bind-key "C-M-<up>" 'gcm-scroll-up-5)
(bind-key "C-M-<down>" 'gcm-scroll-down-5)

(bind-key "<f9>" 'menu-bar-open)
(bind-key "<f10>" 'mu4e)
(bind-key "<f12>" 'org-agenda-list)
(bind-key "S-<f12>" 'org-agenda)

