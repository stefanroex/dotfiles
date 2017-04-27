(use-package evil
  :init
  (setq evil-intercept-esc 'always
        evil-want-fine-undo t
        evil-shift-width 2)
  :config
  (evil-mode t)
  (keys :states nil
        :keymaps '(minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-isearch-map
                   ivy-minibuffer-map)
        [escape] 'minibuffer-keyboard-quit)

  (keys "C-n" 'next-error
        "C-p" 'previous-error
        "C-h" 'evil-window-left
        "C-j" 'evil-window-down
        "C-k" 'evil-window-up
        "C-l" 'evil-window-right))

(use-package evil-surround
  :config
  (global-evil-surround-mode t))

(use-package evil-nerd-commenter
  :defer t
  :init
  (keys-l
   :states '(normal visual)
   "cc" 'evilnc-comment-or-uncomment-lines))

(use-package evil-search-highlight-persist
  :init
  (global-evil-search-highlight-persist t)
  :config
  (keys :keymaps 'prog-mode-map
        "RET" 'evil-search-highlight-persist-remove-all))

(provide 'init-evil)
