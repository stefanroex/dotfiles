(use-package evil
  :config
  (setq evil-intercept-esc 'always
        evil-want-fine-undo t
        evil-shift-width 2)
  (keys :states nil
        :keymaps '(minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-isearch-map
                   ivy-minibuffer-map)
        [escape] 'minibuffer-keyboard-quit)

  (evil-add-hjkl-bindings 'dired-mode-map 'emacs)
  (evil-add-hjkl-bindings 'ibuffer-mode-map 'emacs)

  (keys :states 'emacs
        :keymap 'ibuffer-mode-map
        "J" 'ibuffer-jump-to-buffer)

  (keys "C-n" 'next-error
        "C-p" 'previous-error
        "C-h" 'evil-window-left
        "C-j" 'evil-window-down
        "C-k" 'evil-window-up
        "C-l" 'evil-window-right)

  (evil-mode t))

(use-package evil-surround
  :config
  (global-evil-surround-mode t))

(use-package evil-nerd-commenter
  :commands 'evilnc-comment-or-uncomment-lines
  :init
  (keys-l
   :states '(normal visual)
   "cc" 'evilnc-comment-or-uncomment-lines))

(use-package evil-search-highlight-persist
  :config
  (global-evil-search-highlight-persist t)
  (keys :keymaps 'prog-mode-map
        "RET" 'evil-search-highlight-persist-remove-all))

(provide 'init-evil)
