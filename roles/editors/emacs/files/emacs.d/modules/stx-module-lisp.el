(defconst lisp-hooks
  '(cider-mode-hook
    cider-repl-mode-hook
    clojure-mode-hook
    emacs-lisp-mode-hook
    lisp-mode-hook
    lisp-interaction-mode-hook
    scheme-mode-hook))

(use-package paredit
  :init
  (dolist (mode-hook lisp-hooks)
    (add-hook mode-hook #'paredit-mode)))

(use-package evil-cleverparens
  :init
  (setq evil-cleverparens-use-regular-insert t)
  (dolist (mode-hook lisp-hooks)
    (add-hook mode-hook #'evil-cleverparens-mode)))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :init
  (dolist (mode-hook '(clojure-mode-hook emacs-lisp-mode-hook))
    (add-hook mode-hook #'aggressive-indent-mode)))

(use-package eldoc
  :diminish eldoc-mode
  :init
  (dolist (mode-hook '(cider-mode-hook emacs-lisp-mode-hook))
    (add-hook mode-hook #'eldoc-mode)))

(use-package paxedit
  :init
  (dolist (mode-hook lisp-hooks)
    (add-hook mode-hook #'paxedit-mode))
  :config
  (keys
    :states 'normal
    :keymaps 'override
    "M-j" 'paxedit-transpose-backward
    "M-k" 'paxedit-transpose-forward))

(provide 'stx-module-lisp)
