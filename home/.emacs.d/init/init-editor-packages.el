(use-package better-defaults
  :pin melpa-stable)

;; (use-package base16-theme
;;   :ensure t
;;   :init
;;   (load-theme 'base16-ashes t)
;;   (set-background-color "black"))

(use-package challenger-deep-theme
  :ensure t)

(use-package exec-path-from-shell
  :pin melpa-stable
  :config
  (exec-path-from-shell-initialize))

(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup))

(use-package general
  :config
  (progn
    (setq general-default-states '(normal emacs motion))
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
            "S" 'sf/focus-at-point
            "z" 'previous-code-buffer
            "B" 'ibuffer
            "v" 'open-emacs-config)))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode t))

(use-package yasnippet
  :pin melpa-stable
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package ag
  :defer t
  :init
  (setq ag-reuse-buffers t))

(use-package company
  :diminish company-mode
  :init
  (global-company-mode t)
  :config
  (define-key prog-mode-map (kbd "<tab>") 'company-complete)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer))

(use-package company-flow
  :defer t
  :commands company-flow
  :init
  (progn
    (push 'company-flow company-backends))
  :config
  (add-to-list 'company-flow-modes 'web-mode))

(use-package project-explorer
  :defer t
  :init
  (setq pe/omit-gitignore t)
  (keys-l "a" 'project-explorer-toggle)
  :config
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

(use-package ace-jump-mode
  :init
  (keys "SPC" 'ace-jump-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-side-window-max-width 0.7
        which-key-add-column-padding 1)
  (which-key-mode +1)
  (which-key-setup-side-window-right))

(use-package flx
  :ensure t)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (ivy-mode t)
  :config
  (setq ivy-height 20
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        enable-recursive-minibuffers t))

(use-package counsel
  :ensure t
  :config
  (keys-l "y" 'counsel-yank-pop
          "f" 'counsel-projectile
          "F" 'counsel-find-file
          "b" 'counsel-projectile-switch-to-buffer)
  (keys :states nil
        "M-x" 'counsel-M-x))

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-create-missing-test-files t
        projectile-completion-system 'ivy
        projectile-switch-project-action 'counsel-projectile-find-file)
  :config
  (keys-l "p" 'projectile-command-map
          "s" 'projectile-ag)

  (defun advice-projectile-no-sub-project-files ()
    "Directly call `projectile-get-ext-command'. No need to try to get a
        list of sub-project files if the vcs is git."
    (projectile-files-via-ext-command (projectile-get-ext-command)))
  (advice-add 'projectile-get-repo-files :override #'advice-projectile-no-sub-project-files)

  (projectile-global-mode t))

(use-package projectile-rails
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package slim-mode
  :ensure t)

(use-package reveal-in-osx-finder
  :ensure t
  :config
  (keys-l "o" 'reveal-in-osx-finder))

(use-package rspec-mode
  :ensure t
  :config
  (keys-l :keymaps 'rspec-mode-map
          "t" 'rspec-verify))

(provide 'init-editor-packages)
