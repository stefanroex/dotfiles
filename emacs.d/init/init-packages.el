(use-package better-defaults
  :pin melpa-stable)

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

    ;; Better undo behavior, more like vim.
    (setq evil-want-fine-undo 'fine)

    ;; Set offset used by < and > to 2
    (setq-default evil-shift-width 2)))

(use-package general
  :ensure t
  :config
  (progn
    (setq general-default-states '(normal))

    (defun my-switch-to-other-buffer ()
      "Switch to previous buffer."
      (interactive)
      (switch-to-buffer (other-buffer)))

    (defun open-emacs-config ()
      "Open the emacs packages file for quick changes."
      (interactive)
      (find-file "~/.emacs.d/init/init-packages.el"))

    (defun circle-code-buffers (circle-fn)
      (let ((bread-crumb (buffer-name)))
        (funcall circle-fn)
        (while
            (and
             (string-match-p "^\*" (buffer-name))
             (not (equal bread-crumb (buffer-name))) )
          (funcall circle-fn))))

    (defun next-code-buffer ()
      "Open next active buffer, ignoring non-code related buffers."
      (interactive)
      (circle-code-buffers 'next-buffer))

    (defun previous-code-buffer ()
      "Open next active buffer, ignoring non-code related buffers."
      (interactive)
      (circle-code-buffers 'previous-buffer))

    (defun kill-other-buffers ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer
            (delq (current-buffer)
                  (remove-if-not 'buffer-file-name (buffer-list)))))

    (general-define-key
     :keymaps 'emacs-lisp-mode-map
     :prefix ","
     "e" 'eval-last-sexp
     "d" 'helm-apropos)

    (general-define-key
     :prefix ","
     "hk" 'describe-key
     "hm" 'describe-mode
     "hf" 'describe-function
     "hv" 'describe-variable)

    (general-define-key
     :prefix ","
     "," 'my-switch-to-other-buffer
     "k" 'kill-other-buffers
     "q" 'kill-buffer-and-window
     "w" 'delete-window
     "x" 'next-code-buffer
     "z" 'previous-code-buffer
     "v" 'open-emacs-config)))

(use-package exec-path-from-shell
  :pin melpa-stable
  :init
  (exec-path-from-shell-initialize))

(use-package base16-theme
  :init
  (load-theme 'base16-ashes-dark t))

(use-package yasnippet
  :pin melpa-stable
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1))

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

  (defun display-buffer-full-screen (buffer alist)
    (delete-other-windows)
    ;; make sure the window isn't dedicated, otherwise
    ;; `set-window-buffer' throws an error
    (set-window-dedicated-p nil nil)
    (set-window-buffer nil buffer)
    ;; return buffer's window
    (get-buffer-window buffer))

  (setq magit-display-buffer-function
        (lambda (buffer)
          (if magit-display-buffer-noselect
              ;; the code that called `magit-display-buffer-function'
              ;; expects the original window to stay alive, we can't go
              ;; fullscreen
              (magit-display-buffer-traditional buffer)
            (display-buffer buffer '(display-buffer-full-screen)))))

  (general-define-key
   :prefix ","
   "gs" 'magit-status
   "gl" 'magit-log
   "gb" 'magit-blame))

(use-package project-explorer
  :defer t
  :config
  (general-define-key
   :prefix ","
   "a" 'project-explorer-toggle)
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
    (setq cljr-auto-sort-ns t
          cljr-favor-prefix-notation nil
          cljr-favor-private-functions nil
          cljr-clojure-test-declaration
          "[cljs.test :refer-macros [deftest is]]")

    (add-hook 'clojure-mode-hook #'clj-refactor-mode))
  :config
  (add-to-list 'cljr-magic-require-namespaces
               '("reagent"  . "reagent.core")))

(use-package evil-cleverparens
  :defer t
  :config
  (setq evil-cleverparens-use-regular-insert t)
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
  :defer t)

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
  :config
  (general-define-key "SPC" 'ace-jump-mode))

(use-package cider
  :defer t
  :init
  (setq cider-show-error-buffer t
        cider-repl-display-help-banner nil
        cider-eval-result-duration nil
        cider-auto-select-error-buffer t
        cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-wrap-history t)
  :config
  (general-define-key
   :prefix ","
   "cs" 'cider-jack-in
   "cS" 'cider-restart)

  (define-key evil-normal-state-map (kbd "\\") 'cider-eval-defun-at-point)

  (general-define-key
   :keymaps ('clojure-mode-map 'clojurescript-mode-map)
   :prefix ","
   "cE" 'cider-pprint-eval-last-sexp
   "cT" 'cider-test-run-test
   "cd" 'cider-doc
   "ce" 'cider-eval-defun-at-point
   "cm" 'cider-macroexpand-1
   "cr" 'cider-switch-to-repl-buffer
   "ct" 'cider-test-run-tests)

  (advice-add 'evil-search-highlight-persist-remove-all :after #'cider--remove-result-overlay))

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

    (general-define-key
     :prefix ","
     "b" 'helm-projectile-switch-to-buffer
     "B" 'helm-mini
     "y" 'helm-show-kill-ring)

    (global-set-key (kbd "M-x") 'helm-M-x)))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-global-mode t)
  (setq projectile-keymap-prefix (kbd "C-c p")
        projectile-create-missing-test-files t
        projectile-switch-project-action 'helm-projectile-find-file))

(use-package helm-projectile
  :config
  (evil-ex-define-cmd "A" 'projectile-toggle-between-implementation-and-test)
  (general-define-key
   :prefix ","
   "f" 'helm-projectile
   "F" 'helm-find-files))

(use-package aggressive-indent
  :defer t
  :diminish aggressive-indent-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'clojure-mode-hook #'aggressive-indent-mode)))

(use-package evil-anzu
  :ensure t
  :diminish anzu-mode
  :config
  (global-anzu-mode t))

(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (setq whitespace-line-column 80
        whitespace-style '(face tabs empty trailing lines-tail))
  (global-whitespace-mode t))

(provide 'init-packages)
