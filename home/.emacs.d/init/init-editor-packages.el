(use-package better-defaults
  :pin melpa-stable)

(use-package challenger-deep-theme)

(use-package exec-path-from-shell
  :pin melpa-stable
  :config
  (exec-path-from-shell-initialize))

(use-package smart-mode-line
  :config
  (sml/setup))

(use-package general
  :init
  (setq general-default-states '(normal emacs motion))
  :config
  (progn
    (general-create-definer keys-l :prefix ",")
    (defalias 'keys 'general-define-key)

    (keys-l :states 'normal
            :keymaps 'emacs-lisp-mode-map
            "e" 'eval-last-sexp)

    (keys-l "hk" 'describe-key
            "hm" 'describe-mode
            "hf" 'describe-function
            "hv" 'describe-variable
            "," 'my-switch-to-other-buffer
            "k" 'kill-other-buffers
            "q" 'kill-buffer-and-window
            "w" 'delete-window
            "x" 'next-code-buffer
            "z" 'previous-code-buffer
            "B" 'ibuffer
            "O" 'open-iterm-in-project-root
            "v" 'open-emacs-config)))

(use-package yasnippet
  :pin melpa-stable
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package ag
  :defer t
  :config
  (setq ag-reuse-buffers t))

(use-package company
  :diminish company-mode
  :config
  (global-company-mode t)
  (define-key prog-mode-map (kbd "<tab>") 'company-complete)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer))

(use-package company-flow
  :defer t
  :config
  (push 'company-flow company-backends)
  (add-to-list 'company-flow-modes 'web-mode))

(use-package project-explorer
  :commands project-explorer-toggle
  :init
  (keys-l "a" 'project-explorer-toggle)
  :config
  (add-hook 'project-explorer-mode-hook
            (lambda () (linum-mode -1)))

  (setq pe/omit-gitignore t
        pe/omit-regex "^#\\|^\\.git$\\|~$\\|^node_modules$")
  (keys :keymaps 'project-explorer-mode-map
        "o" 'pe/return
        "i" 'pe/toggle-omit
        "r" 'pe/rename-file
        "TAB" 'pe/tab
        "q" 'pe/quit
        "c" 'pe/copy-file
        "R" 'revert-buffer
        "d" 'pe/delete-file
        "RET" 'pe/return))

(use-package avy
  :commands avy-goto-char-time
  :init
  (setq avy-timeout-seconds 0.3)
  (keys "SPC" 'avy-goto-char-timer))

(use-package which-key
  :defer 1
  :diminish which-key-mode
  :config
  (setq which-key-side-window-max-width 0.7
        which-key-add-column-padding 1)
  (which-key-mode +1)
  (which-key-setup-side-window-right))

(use-package flx)

(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode t)
  (setq ivy-height 20
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil
        ivy-on-del-error-function nil
        ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        enable-recursive-minibuffers t))

(use-package counsel
  :config
  (keys-l "y" 'counsel-yank-pop
          "f" 'counsel-projectile
          "F" 'counsel-find-file
          "b" 'counsel-projectile-switch-to-buffer)
  (keys :states nil
        "M-x" 'counsel-M-x))

(use-package projectile
  :diminish projectile-mode
  :config
  (setq projectile-create-missing-test-files t
        projectile-completion-system 'ivy)

  (keys-l "p" 'projectile-command-map
          "s" 'projectile-ag)

  (defun advice-projectile-no-sub-project-files ()
    "Directly call `projectile-get-ext-command'. No need to try to get a
        list of sub-project files if the vcs is git."
    (projectile-files-via-ext-command (projectile-get-ext-command)))

  (advice-add 'projectile-get-repo-files :override #'advice-projectile-no-sub-project-files)
  (projectile-global-mode t))

(use-package counsel-projectile
  :config
  (setq projectile-switch-project-action 'counsel-projectile-find-file))

(use-package yaml-mode
  :defer t)

(use-package markdown-mode
  :defer t)

(use-package slim-mode
  :defer t)

(use-package reveal-in-osx-finder
  :config
  (keys-l "o" 'reveal-in-osx-finder))

(use-package rspec-mode
  :defer t
  :config
  (keys-l :keymaps 'rspec-mode-map
          "t" 'rspec-verify))

(use-package dumb-jump
  :commands 'dumb-jump-go
  :init
  (setq dumb-jump-selector 'ivy)
  (keys :keymaps 'prog-mode-map
        :modes 'normal
        "gD" 'dumb-jump-go))

(use-package evil-snipe
  :config
  (evil-snipe-override-mode 1)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

(provide 'init-editor-packages)
