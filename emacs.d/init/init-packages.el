(use-package better-defaults
  :pin melpa-stable)

(use-package exec-path-from-shell
  :pin melpa-stable
  :init
  (exec-path-from-shell-initialize))

(use-package base16-theme
  :init
  (load-theme 'base16-ashes-dark t))

(use-package yasnippet
  :pin melpa-stable
  :init
  (yas-global-mode 1))

(use-package evil-leader
  :pin melpa-stable
  :init (global-evil-leader-mode t)
  :config
  (progn
    ;; Evil leader
    (defun my-switch-to-other-buffer ()
      "Switch to other buffer"
      (interactive)
      (switch-to-buffer (other-buffer)))

    (defun open-emacs-config ()
      (interactive)
      (find-file "~/.emacs.d/init.el"))

    (defun circle-code-buffers (circle-fn)
      (let ((bread-crumb (buffer-name)))
        (circle-fn)
        (while
            (and
             (string-match-p "^\*" (buffer-name))
             (not (equal bread-crumb (buffer-name))) )
          (circle-fn))))

    (defun next-code-buffer ()
      (interactive)
      (circle-code-buffers next-buffer))

    (defun previous-code-buffer ()
      (interactive)
      (circle-code-buffers previous-buffer))

    (defun kill-other-buffers ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer
            (delq (current-buffer)
                  (remove-if-not 'buffer-file-name (buffer-list)))))

    (evil-leader/set-leader ",")

    ;; Easier window switching
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

    ;; Evil Window switching similar in emacs mode
    (global-set-key (kbd "C-h") 'evil-window-left)
    (global-set-key (kbd "C-j") 'evil-window-down)
    (global-set-key (kbd "C-k") 'evil-window-up)
    (global-set-key (kbd "C-l") 'evil-window-right)

    (evil-leader/set-key-for-mode 'emacs-lisp-mode
      "e" 'eval-last-sexp
      "d" 'helm-apropos
      "hf" 'describe-function
      "hv" 'describe-variable)

    (evil-leader/set-key
      "," 'my-switch-to-other-buffer
      "hk" 'describe-key
      "hm" 'describe-mode
      "k" 'kill-other-buffers
      "q" 'kill-buffer-and-window
      "w" 'quit-window
      "x" 'next-code-buffer
      "z" 'previous-code-buffer
      "v" 'open-emacs-config)))

(use-package evil
  :init
  (evil-mode t)
  :config
  (progn
    ;; esc quits
    (setq evil-intercept-esc 'always)
    (defun minibuffer-keyboard-quit ()
      (interactive)
      (if (and delete-selection-mode transient-mark-mode mark-active)
          (setq deactivate-mark t)
        (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
        (abort-recursive-edit)))

    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

    ;; Next and previous search results
    (define-key evil-normal-state-map (kbd "C-n") 'next-error)
    (define-key evil-normal-state-map (kbd "C-p") 'previous-error)

    ;; Set offset used by < and > to 2
    (setq-default evil-shift-width 2)))

(use-package evil-surround
  :init (global-evil-surround-mode t))

(use-package ag
  :defer t
  :init
  (evil-ex-define-cmd "Ag" 'ag))

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
  (evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-log-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-process-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-status-mode-map 'emacs)
  (evil-leader/set-key
    "gs" 'magit-status
    "gl" 'magit-log
    "gb" 'magit-blame))

(use-package project-explorer
  :defer t
  :init
  (evil-leader/set-key "a" 'project-explorer-toggle)
  :config
  (setq pe/omit-gitignore t)
  (evil-declare-key 'normal project-explorer-mode-map
    (kbd "o") 'pe/return
    (kbd "r") 'pe/rename-file
    (kbd "TAB") 'pe/tab
    (kbd "q") 'pe/quit
    (kbd "c") 'pe/copy-file
    (kbd "R") 'revert-buffer
    (kbd "d") 'pe/delete-file
    (kbd "RET") 'pe/return))

(use-package paredit
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

(use-package clj-refactor
  :defer t
  :init
  (progn
    (evil-leader/set-key-for-mode 'clojure-mode
      "rp" 'cljr-promote-function
      "rf" 'cljr-create-fn-from-example
      "rl" 'cljr-move-to-let
      "rd" 'cljr-add-project-dependency)

    (evil-leader/set-key-for-mode 'clojurescript-mode
      "rp" 'cljr-promote-function
      "rf" 'cljr-create-fn-from-example
      "rl" 'cljr-move-to-let
      "rd" 'cljr-add-project-dependency)

    (setq cljr-favor-private-functions nil)

    (defun my-clojure-mode-hook ()
      (clj-refactor-mode 1)
      (yas-minor-mode 1))

    (add-hook 'clojure-mode-hook #'my-clojure-mode-hook)))

(use-package evil-cleverparens
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
    (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)))

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode))

(use-package clojure-mode
  :defer t
  :config
  (evil-leader/set-key-for-mode 'clojurescript-mode
    "cs" 'cider-jack-in)
  (evil-leader/set-key-for-mode 'clojure-mode
    "cs" 'cider-jack-in))

(use-package clojure-mode-extra-font-locking
  :defer t)

(use-package evil-nerd-commenter
  :defer t
  :init
  (define-key evil-normal-state-map ",cc" 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map ",cc" 'evilnc-comment-or-uncomment-lines))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode t))

(use-package evil-search-highlight-persist
  :init
  (global-evil-search-highlight-persist t)
  :config
  (define-key evil-normal-state-map (kbd "RET") 'evil-search-highlight-persist-remove-all))

(use-package ace-jump-mode
  :defer t
  :config
  (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode))

(use-package cider
  :defer t
  :init
  (setq cider-show-error-buffer t
        cider-auto-select-error-buffer t
        cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-wrap-history t)
  :config
  (evil-leader/set-key-for-mode 'clojure-mode
    "e" 'cider-eval-last-sexp
    "T" 'cider-test-run-test
    "t" 'cider-test-run-tests
    "d" 'cider-doc)
  (evil-leader/set-key-for-mode 'clojurescript-mode
    "e" 'cider-eval-last-sexp
    "E" 'cider-pprint-eval-last-sexp
    "T" 'cider-test-run-test
    "t" 'cider-test-run-tests
    "d" 'cider-doc))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1)
  (which-key-setup-side-window-right))

(use-package helm
  :init
  (progn
    (setq helm-M-x-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-projectile-fuzzy-match t
          helm-autoresize-mode t
          helm-apropos-fuzzy-match t
          helm-recentf-fuzzy-match t)

    (evil-leader/set-key
      "b" 'helm-projectile-switch-to-buffer
      "B" 'helm-mini
      "y" 'helm-show-kill-ring)

    (global-set-key (kbd "M-x") 'helm-M-x)))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-global-mode t)
  (setq projectile-switch-project-action 'helm-projectile-find-file))

(use-package helm-projectile
  :config
  (evil-ex-define-cmd "A" 'projectile-toggle-between-implementation-and-test)
  (evil-leader/set-key
    "f" 'helm-projectile-find-file
    "F" 'helm-find-files
    "p" 'helm-projectile-switch-project))

(use-package aggressive-indent
  :defer t
  :diminish aggressive-indent-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'clojure-mode-hook #'aggressive-indent-mode)))

(use-package highlight-parentheses
  :defer t
  :diminish highlight-parentheses-mode
  :init
  (progn
    (setq hl-paren-colors '("OrangeRed" "DodgerBlue2" "DarkOliveGreen2"))
    (global-highlight-parentheses-mode t)))

(provide 'init-packages)
