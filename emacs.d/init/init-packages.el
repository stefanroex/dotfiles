(use-package better-defaults
  :pin melpa-stable)

(use-package general
  :config
  (progn
    (setq general-default-states '(normal emacs motion))
    (general-create-definer keys-l :prefix ",")
    (defalias 'keys 'general-define-key)

    (keys-l :states 'normal
            :keymaps 'emacs-lisp-mode-map
            "e" 'eval-last-sexp
            "d" 'helm-apropos)

    (keys-l "hk" 'describe-key
            "hm" 'describe-mode
            "hf" 'describe-function
            "hv" 'describe-variable)

    (keys-l "," 'my-switch-to-other-buffer
            "k" 'kill-other-buffers
            "q" 'kill-buffer-and-window
            "w" 'delete-window
            "x" 'next-code-buffer
            "z" 'previous-code-buffer
            "v" 'open-emacs-config)))

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
                   minibuffer-local-isearch-map)
        [escape] 'minibuffer-keyboard-quit)

  (keys "C-n" 'next-error
        "C-p" 'previous-error
        "C-h" 'evil-window-left
        "C-j" 'evil-window-down
        "C-k" 'evil-window-up
        "C-l" 'evil-window-right))

(use-package exec-path-from-shell
  :pin melpa-stable
  :config
  (exec-path-from-shell-initialize))

(use-package base16-theme
  :init
  (load-theme 'base16-ashes-dark t))

(use-package yasnippet
  :pin melpa-stable
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode t))

(use-package ag
  :defer t
  :init
  (setq ag-reuse-buffers t)
  (keys-l "s" 'ag))

(use-package company
  :diminish company-mode
  :init
  (global-company-mode t)
  :config
  (define-key prog-mode-map (kbd "<tab>") 'company-complete)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer))

(use-package magit
  :defer t
  :init
  (setq magit-display-buffer-function 'magit-buffer-full-screen)
  (evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-log-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-process-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-status-mode-map 'emacs)
  (keys :states nil
        :keymaps 'magit-file-section-map
        "K" 'magit-discard)

  (keys-l "gs" 'magit-status
          "gl" 'magit-log
          "gb" 'magit-blame))

(use-package project-explorer
  :defer t
  :init
  (setq pe/omit-gitignore t)
  (keys-l "a" 'project-explorer-toggle)
  :config
  (keys :keymaps 'project-explorer-mode-map
        "o" 'pe/return
        "r" 'pe/rename-file
        "TAB" 'pe/tab
        "q" 'pe/quit
        "c" 'pe/copy-file
        "R" 'revert-buffer
        "d" 'pe/delete-file
        "RET" 'pe/return))

(use-package paredit
  :defer t
  :init
  (dolist (mode-hook lisp-mode-hooks)
    (add-hook mode-hook #'paredit-mode)))

(use-package clj-refactor
  :defer t
  :init
  (setq cljr-auto-sort-ns t
        cljr-favor-prefix-notation nil
        cljr-favor-private-functions nil
        cljr-clojure-test-declaration
        "[cljs.test :refer-macros [deftest is]]")
  (add-hook 'clojure-mode-hook #'clj-refactor-mode)
  :config
  (dolist (details cljr--all-helpers)
    (let ((key (car details))
          (fn (cadr details)))
      (keys :prefix ",r"
            :keymaps '(clojure-mode-map clojurescript-mode-map)
            key fn)))
  (add-to-list 'cljr-magic-require-namespaces
               '("reagent"  . "reagent.core")))

(use-package evil-cleverparens
  :defer t
  :init
  (setq evil-cleverparens-use-regular-insert t)
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode))

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :init
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode))

(use-package clojure-mode
  :defer t)

(use-package clojure-mode-extra-font-locking
  :defer t)

(use-package evil-nerd-commenter
  :defer t
  :init
  (keys-l
   :states '(normal visual)
   "cc" 'evilnc-comment-or-uncomment-lines))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode t))

(use-package evil-search-highlight-persist
  :init
  (global-evil-search-highlight-persist t)
  :config
  (keys "RET" 'evil-search-highlight-persist-remove-all))

(use-package ace-jump-mode
  :init
  (keys "SPC" 'ace-jump-mode))

(use-package cider
  :defer t
  :init
  (setq cider-show-error-buffer t
        cider-repl-display-help-banner nil
        cider-eval-result-duration nil
        cider-auto-select-error-buffer t
        cider-request-dispatch 'static
        cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-wrap-history t)

  :config
  (defvar cider-mode-maps
    '(cider-repl-mode-map
      clojure-mode-map
      clojurescript-mode-map))

  (keys :keymaps cider-mode-maps
        "\\" 'cider-eval-defun-at-point)

  (keys-l :keymaps cider-mode-maps
          "E" 'cider-pprint-eval-last-sexp
          "T" 'cider-test-run-test
          "d" 'cider-doc
          "e" 'cider-eval-defun-at-point)

  (keys :keymaps cider-mode-maps
        :prefix ",c"
        "I" 'cider-display-connection-info
        "J" 'cider-jack-in-clojurescript
        "R" 'cider-restart
        "c" 'cider-connection-browser
        "d" 'cider-doc-map
        "eb" 'cider-load-buffer
        "ef" 'cider-eval-defun-at-point
        "ef" 'cider-load-file
        "el" 'cider-eval-last-sexp
        "er" 'cider-eval-last-sexp-and-replace
        "i" 'cider-inspect
        "j" 'cider-jack-in
        "m" 'cider-macroexpand-1
        "pf" 'cider-pprint-eval-defun-at-point
        "pl" 'cider-pprint-eval-last-sexp
        "q" 'cider-quit
        "r" 'cider-refresh
        "sc" 'cider-rotate-default-connection
        "sn" 'cider-repl-set-ns
        "sr" 'cider-switch-to-repl-buffer
        "t" 'cider-test-run-tests)

  (add-to-list 'evil-emacs-state-modes 'cider-connections-buffer-mode)
  (evil-add-hjkl-bindings cider-connections-buffer-mode-map 'emacs)

  (advice-add 'evil-search-highlight-persist-remove-all :after #'cider--remove-result-overlay))

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-side-window-max-width 0.7
        which-key-add-column-padding 1)
  (which-key-mode +1)
  (which-key-setup-side-window-right))

(use-package helm
  :init
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-projectile-fuzzy-match t
        helm-autoresize-mode t
        helm-apropos-fuzzy-match t
        helm-recentf-fuzzy-match t)
  :config
  (keys-l "B" 'helm-buffers-list
          "y" 'helm-show-kill-ring)
  (keys :states nil
        "M-x" 'helm-M-x))

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-create-missing-test-files t
        projectile-completion-system 'helm
        projectile-switch-project-action 'helm-projectile-find-file)
  :config
  (keys-l "p" 'projectile-command-map)
  (keys-l "b" 'projectile-switch-to-buffer)
  (projectile-global-mode t))

(use-package helm-projectile
  :config
  (keys-l "f" 'helm-projectile
          "F" 'helm-find-files))

(use-package aggressive-indent
  :defer t
  :diminish aggressive-indent-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'clojure-mode-hook #'aggressive-indent-mode)))

(use-package evil-anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode t))

(use-package indent-guide
  :init
  (setq indent-guide-delay 0.5)
  :config
  (indent-guide-global-mode))

(provide 'init-packages)
