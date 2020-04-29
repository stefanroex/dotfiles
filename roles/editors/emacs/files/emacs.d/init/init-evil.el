(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :config
  (setq evil-intercept-esc 'always
        evil-want-fine-undo t
        evil-shift-width 2)

  (fset 'evil-visual-update-x-selection 'ignore)

  (keys :states nil
    :keymaps '(minibuffer-local-map
               minibuffer-local-ns-map
               minibuffer-local-completion-map
               minibuffer-local-must-match-map
               minibuffer-local-isearch-map
               ivy-minibuffer-map)
    [escape] 'minibuffer-keyboard-quit)

  (defun evil-execute-q-macro ()
    "Execute @q"
    (interactive)
    (evil-execute-macro 1 "@q"))

  (keys
    "Q" 'evil-execute-q-macro
    "C-n" 'next-error
    "C-p" 'previous-error
    "C-h" 'evil-window-left
    "C-j" 'evil-window-down
    "C-k" 'evil-window-up
    "C-l" 'evil-window-right)

  (evil-mode t))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-mode-list '(dired ibuffer ivy help eshell))
  :config
  (evil-collection-init))

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
  :demand
  :config
  (global-evil-search-highlight-persist t)
  (keys :keymaps 'prog-mode-map
    "RET" 'evil-search-highlight-persist-remove-all))

(provide 'init-evil)
