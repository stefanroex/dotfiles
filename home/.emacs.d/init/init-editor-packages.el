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
  :demand
  :config
  (progn
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

(use-package yasnippet
  :pin melpa-stable
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package ag
  :defer t
  :init
  (keys-l "s" 'ag-project-regexp
          "S" 'ag-project)
  :config
  (setq ag-reuse-buffers t))

(use-package company
  :diminish company-mode
  :config
  (setq company-idle-delay nil)
  (global-company-mode t)
  (define-key prog-mode-map (kbd "<tab>") 'company-indent-or-complete-common)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer))

(use-package company-flow
  :defer t
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-flow)))

(defun neotree-project-root ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-show)
    (if project-dir
        (progn
          (neotree-dir project-dir)
          (neotree-find file-name)))))

(defun neotree-toggle-project-root ()
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (neotree-project-root)))

(use-package neotree
  :init
  (keys-l "a" 'neotree-toggle-project-root)
  :config
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

(use-package avy
  :commands avy-goto-char-time
  :init
  (setq avy-timeout-seconds 0.3)
  ;; (keys "SPC" 'avy-goto-char-timer)
  )

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
  (keys :keymaps 'prog-mode-map
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

(use-package fzf)

(use-package prettier-js
  :config
  (add-hook 'markdown-mode-hook 'prettier-js-mode)
  (add-hook 'css-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode))

;; (shell-pop-in-hook)
(provide 'init-editor-packages)
