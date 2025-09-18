;;;  -*- lexical-binding: t -*-

(use-package evil
  :demand t

  :general-config
  (:keymap 'overwrite
   :states '(normal emacs motion)
   "C-h" 'evil-window-left
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up
   "C-l" 'evil-window-right)

  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-fine-undo t)
  (evil-undo-system 'undo-redo)
  (evil-intercept-esc 'always)

  :config
  (evil-mode t))

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
  :general
  (:states '(normal visual)
   "SPC cc" 'evilnc-comment-or-uncomment-lines))

(use-package evil-search-highlight-persist
  :demand t

  :config
  (global-evil-search-highlight-persist t)

  :general-config
  (:keymaps 'prog-mode-map
   :states 'normal
   "RET" 'evil-search-highlight-persist-remove-all))

(provide 'stx-module-evil)
