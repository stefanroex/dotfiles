(defconst stx/lisp-mode-hooks
  '(cider-mode-hook
    cider-repl-mode-hook
    clojure-mode-hook
    emacs-lisp-mode-hook
    lisp-interaction-mode-hook
    lisp-mode-hook
    scheme-mode-hook))

(defun stx/add-lisp-mode-hook (mode)
  (dolist (hook stx/lisp-mode-hooks)
    (add-hook hook mode)))

(use-package paredit
  :init (stx/add-lisp-mode-hook 'paredit-mode))

(use-package evil-cleverparens
  :init (stx/add-lisp-mode-hook 'evil-cleverparens-mode)
  :custom
  (evil-cleverparens-use-regular-insert t))

(use-package paxedit
  :init (stx/add-lisp-mode-hook 'paxedit-mode)
  :general
  (keys
    :states 'normal
    :keymaps 'override
    "M-j" 'paxedit-transpose-backward
    "M-k" 'paxedit-transpose-forward))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook ((clojure-mode emacs-lisp-mode) . aggressive-indent-mode))

(use-package eldoc
  :diminish eldoc-mode
  :hook ((cider-mode emacs-lisp-mode) . eldoc-mode))

(provide 'stx-module-lisp)
