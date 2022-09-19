(use-package evil
  :demand t
  :general
  (keys
    :keymap 'overwrite
    "C-h" 'evil-window-left
    "C-j" 'evil-window-down
    "C-k" 'evil-window-up
    "C-l" 'evil-window-right)

  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
	evil-want-fine-undo t
	evil-undo-system 'undo-fu
	evil-intercept-esc 'always)
  :config
  (evil-mode t))

(use-package undo-fu)

(use-package evil-collection
  :demand t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :demand t
  :config
  (global-evil-surround-mode t))

(use-package evil-nerd-commenter
  :commands 'evilnc-comment-or-uncomment-lines
  :general
  (keys-l
    :states '(normal visual)
    "cc" 'evilnc-comment-or-uncomment-lines))

;; TODO doesn't work?
(use-package evil-search-highlight-persist
  :demand t
  :config
  (global-evil-search-highlight-persist t)
  (keys :keymaps 'prog-mode-map
    "RET" 'evil-search-highlight-persist-remove-all))

(provide 'stx-module-evil)
