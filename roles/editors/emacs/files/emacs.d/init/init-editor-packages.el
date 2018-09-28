(use-package challenger-deep-theme)

(use-package diminish
  :ensure t)

(use-package better-defaults
  :pin melpa-stable)

(use-package exec-path-from-shell
  :pin melpa-stable
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode))

(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-init))

(use-package general
  :demand
  :config
  (progn
    (general-auto-unbind-keys)

    (general-define-key :states 'motion "SPC" nil)

    (general-create-definer
      keys
      :states '(normal emacs motion))


    (general-create-definer
      keys-l
      :prefix "SPC"
      :states '(normal emacs motion))

    (keys-l
      :states 'normal
      :keymaps 'emacs-lisp-mode-map
      "e" 'eval-last-sexp)

    (keys-l
      "hk" 'describe-key
      "hm" 'describe-mode
      "hf" 'describe-function
      "hv" 'describe-variable
      "SPC" 'my-switch-to-other-buffer
      "k" 'kill-other-buffers
      "q" 'kill-buffer-and-window
      "w" 'delete-window
      "z" 'next-code-buffer
      "x" 'previous-code-buffer
      "B" 'ibuffer
      "O" 'open-iterm-in-project-root
      "v" 'open-emacs-config)))

(use-package ag
  :defer t
  :init
  (keys-l "s" 'ag-project-regexp
    "S" 'ag-project)
  :config
  (setq ag-reuse-buffers t)
  (define-key ag-mode-map (kbd "k") nil))

(use-package company
  :diminish company-mode
  :init (global-company-mode)
  :config
  (keys :states 'insert
    "<tab>" 'company-complete-common-or-cycle)
  (general-def 'company-active-map
    "C-s" 'company-filter-candidates
    "C-d" 'company-show-doc-buffer
    "<tab>" 'company-complete-common-or-cycle
    "S-<tab>" 'company-select-previous-or-abort))

(use-package neotree
  :init
  (keys-l "a" 'neotree-toggle-project-root)
  :config
  (setq neo-theme 'arrow)
  (keys :keymaps 'neotree-mode-map
    "o" 'neotree-enter
    "i" 'neotree-hidden-file-toggle
    "r" 'neotree-rename-node
    "TAB" 'neotree-dir
    "q" 'neotree-hide
    "c" 'neotree-copy-node
    "R" 'neotree-refresh
    "d" 'neotree-delete-node
    "RET" 'neotree-enter))

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
        ivy-virtual-abbreviate 'full
        ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        enable-recursive-minibuffers t)
  (general-def 'ivy-minibuffer-map
    "C-j" 'ivy-next-line
    "C-k" 'ivy-previous-line))

(use-package counsel
  :config
  (keys-l "y" 'counsel-yank-pop
    "f" 'counsel-projectile-find-file
    "F" 'counsel-find-file
    "b" 'counsel-projectile-switch-to-buffer)
  (keys :states nil
    "M-x" 'counsel-M-x))

(use-package projectile
  :diminish projectile-mode
  :config
  (setq projectile-create-missing-test-files t
        projectile-completion-system 'ivy)

  (keys-l "p" 'projectile-command-map)

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
  (keys
    :keymaps 'prog-mode-map
    :modes 'normal
    "gD" 'dumb-jump-go))

(use-package expand-region
  :commands 'er/expand-region
  :init
  (keys "<C-return>" 'er/expand-region))

(use-package shell-pop
  :commands 'shell-pop
  :init
  (keys-l "j" 'shell-pop)
  :config
  (add-hook 'shell-pop-in-hook
            (lambda ()
              (custom-set-variables
               '(shell-pop-default-directory
                 (projectile-project-root)))))

  (keys :modes 'eshell-mode
    :states 'normal
    "q" 'kill-buffer-and-window)

  (custom-set-variables
   '(shell-pop-full-span t)
   '(shell-pop-autocd-to-working-dir nil)
   '(shell-pop-shell-type
     (quote ("eshell" "*eshell*" (lambda nil (eshell)))))))

(use-package prettier-js
  :config
  (add-hook 'markdown-mode-hook 'prettier-js-mode)
  (add-hook 'css-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode))

(use-package flycheck
  :defer 1
  :config
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp-checkdoc
                  javascript-jshint
                  handlebars
                  scss-lint
                  scss))
  (setq flycheck-idle-change-delay 1
        flycheck-check-syntax-automatically '(save mode-enabled idle-change))
  (global-flycheck-mode))

(use-package web-mode
  :mode (("\\.js$" . web-mode))
  :config
  (setq web-mode-enable-auto-quoting nil
        web-mode-markup-indent-offset 2 web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-indent-style 2)

  (keys-l :keymaps 'web-mode-map
    "d" 'flow-type)

  (add-to-list 'company-dabbrev-code-modes 'web-mode)

  (add-hook 'web-mode-hook
            (lambda ()
              (if (equal web-mode-content-type "javascript")
                  (web-mode-set-content-type "jsx")
                (message "now set to: %s" web-mode-content-type)))))

(use-package magit
  :diminish 'auto-revert-mode
  :defer 1
  :init
  (keys-l "gs" 'magit-status
    "gl" 'magit-log-head
    "gb" 'magit-blame)
  :config
  (setq magit-display-buffer-function 'magit-buffer-full-screen)
  (global-auto-revert-mode t))

(use-package magithub
  :after magit
  :defer t
  :config (magithub-feature-autoinject t))

(use-package css-mode
  :config
  (setq css-indent-offset 2))

(use-package sass-mode
  :mode "\\.sass\\'")

(use-package scss-mode
  :mode "\\.scss\\'")

(use-package esup)

(provide 'init-editor-packages)
