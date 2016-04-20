(defun add-hooks (mode mode-hooks)
  "Add mode to all mode hooks"
  (dolist (mode-hook mode-hooks)
    (add-hook mode-hook mode)))

(defvar paredit-hooks
  '(cider-mode-hook
    cider-repl-mode-hook
    clojure-mode-hook
    emacs-lisp-mode-hook
    lisp-mode-hook
    lisp-interaction-mode-hook
    scheme-mode-hook))

(use-package paredit
  :defer t
  :init
  (add-hooks #'paredit-mode paredit-hooks))

(use-package aggressive-indent
  :defer t
  :diminish aggressive-indent-mode
  :init
  (add-hooks #'aggressive-indent-mode '(clojure-mode-hook emacs-lisp-mode-hook)))

(use-package indent-guide
  :init
  (setq indent-guide-delay 0.5)
  (add-hooks #'indent-guide-mode '(clojure-mode-hook emacs-lisp-mode-hook)))

(use-package evil-cleverparens
  :defer t
  :init
  (setq evil-cleverparens-use-regular-insert t)
  (add-hooks #'evil-cleverparens-mode '(clojure-mode-hook emacs-lisp-mode-hook)))

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :init
  (add-hooks #'eldoc-mode '(cider-mode-hook emacs-lisp-mode-hook)))

(provide 'init-lisp)
