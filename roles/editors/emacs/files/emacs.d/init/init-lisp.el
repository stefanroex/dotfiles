(defun add-hooks (mode-hooks mode)
  "Add mode to all mode hooks"
  (dolist (mode-hook mode-hooks)
    (add-hook mode-hook mode)))

(defconst lisp-hooks
  '(cider-mode-hook
    cider-repl-mode-hook
    clojure-mode-hook
    emacs-lisp-mode-hook
    lisp-mode-hook
    lisp-interaction-mode-hook
    scheme-mode-hook))

(use-package lispy
  :init
  (add-hooks lisp-hooks #'lispy-mode)
  :config
  (define-key lispy-mode-map-lispy "[" #'lispy-brackets)
  (define-key lispy-mode-map-lispy "]" #'lispy-brackets)
  (lispy-set-key-theme '(lispy c-digits)))

(use-package lispyville
  :init
  (add-hooks lisp-hooks #'lispyville-mode)
  :config
  (lispyville-set-key-theme '(operators
                              c-w
                              escape
                              slurp/barf-lispy
                              additional
                              additional-wrap
                              additional-motions
                              additional-insert))
  (keys
    :keymaps 'lispyville-mode-map
    :modes 'motion
    "{" #'evil-backward-paragraph
    "}" #'evil-forward-paragraph))

(use-package aggressive-indent
  :defer t
  :diminish aggressive-indent-mode
  :init
  (add-hooks
   '(clojure-mode-hook emacs-lisp-mode-hook)
   #'aggressive-indent-mode))

(use-package indent-guide
  :diminish indent-guide-mode
  :init
  (setq indent-guide-delay 0.5)
  (add-hooks
   '(clojure-mode-hook emacs-lisp-mode-hook)
   #'indent-guide-mode))

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :init
  (add-hooks
   '(cider-mode-hook emacs-lisp-mode-hook)
   #'eldoc-mode))

(provide 'init-lisp)
