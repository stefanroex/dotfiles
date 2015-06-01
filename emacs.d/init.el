(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; use-package
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish)

(set-face-attribute 'default nil :height 150)
(setq-default line-spacing 4)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'zenburn t)

;; Window switching similar in emacs mode
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-l") 'windmove-right)

;; Tab navigation
(global-set-key (kbd "s-w") 'elscreen-kill)
(global-set-key (kbd "s-t") 'elscreen-create)
(global-set-key (kbd "s-}") 'elscreen-next)
(global-set-key (kbd "s-{") 'elscreen-previous)

;; Encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Environment fixup
(use-package exec-path-from-shell
  :ensure t
  :init
  (progn
    (exec-path-from-shell-initialize)))

;; UI
(use-package init-custom
  :load-path "init/")

(use-package subr-x
  :load-path "init/")

;; Fancy battery info for mode line
(use-package fancy-battery
  :ensure t
  :defer t
  :config (setq fancy-battery-show-percentage t)
  :init (fancy-battery-mode))

(use-package evil-leader
  :ensure t
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

    (defun next-code-buffer ()
      (interactive)
      (let (( bread-crumb (buffer-name) ))
        (next-buffer)
        (while
            (and
             (string-match-p "^\*" (buffer-name))
             (not ( equal bread-crumb (buffer-name) )) )
          (next-buffer))))

    (defun previous-code-buffer ()
      (interactive)
      (let (( bread-crumb (buffer-name) ))
        (previous-buffer)
        (while
            (and
             (string-match-p "^\*" (buffer-name))
             (not ( equal bread-crumb (buffer-name) )) )
          (previous-buffer))))

    (defun kill-other-buffers ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer 
            (delq (current-buffer) 
                  (remove-if-not 'buffer-file-name (buffer-list)))))

    (evil-leader/set-leader ",")

    (evil-leader/set-key-for-mode 'emacs-lisp-mode
      "e" 'eval-last-sexp
      "d" 'helm-apropos
      "hf" 'describe-function
      "hv" 'describe-variable)

    (evil-leader/set-key
      "," 'my-switch-to-other-buffer
      "F" 'helm-find-files
      "hk" 'describe-key
      "hm" 'describe-mode
      "k" 'kill-other-buffers
      "n" 'multi-term
      "q" 'kill-buffer-and-window
      "w" 'quit-window
      "x" 'next-code-buffer
      "z" 'previous-code-buffer
      "v" 'open-emacs-config)))

(use-package evil
  :ensure t
  :init (evil-mode t)
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

    ;; Dont' ring bell when we quit minibuffer
    (setq ring-bell-function
          (lambda ()
            (unless (memq this-command
                          '(keyboard-quit minibuffer-keyboard-quit))
              (ding))))

    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

    ;; Next and previous search results
    (define-key evil-normal-state-map (kbd "C-n") 'next-error)
    (define-key evil-normal-state-map (kbd "C-p") 'previous-error)

    ;; Set offset used by < and > to 2
    (setq-default evil-shift-width 2)

    ;; Easy window management
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

    (defadvice evil-delete-backward-char-and-join (before remove-indent-level activate)
      "Remove extra char (for 2 level-indent) when you're at that indent level"
      (if (looking-back "  ")
          (delete-char -1)))

    (evil-ex-define-cmd "A" 'projectile-toggle-between-implementation-and-test)
    (evil-ex-define-cmd "Ag" 'ag)))

(use-package evil-surround
  :ensure t
  :defer t
  :init (global-evil-surround-mode t))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-switch-project-action 'helm-projectile-find-file)

    (projectile-global-mode)

    (defun find-file-projectile-or-dir ()
      (interactive)
      (if (projectile-project-p)
          (helm-projectile-find-file)
        (helm-find-files)))

    (evil-leader/set-key
      "f" 'find-file-projectile-or-dir
      "p" 'helm-projectile-switch-project)))

(use-package ag
  :ensure t
  :defer t
  :config
  (evil-ex-define-cmd "Ag" 'ag))

(use-package better-defaults
  :ensure t)

(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :init
  (global-company-mode t)
  :config
  (progn
    (define-key prog-mode-map (kbd "<tab>") 'company-complete)
    (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
    (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)))

(use-package magit
  :ensure t
  :defer t
  :diminish magit-auto-revert-mode
  :init
  (progn
    (setq magit-last-seen-setup-instructions "1.4.0")
    (evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
    (evil-add-hjkl-bindings magit-log-mode-map 'emacs)
    (evil-add-hjkl-bindings magit-process-mode-map 'emacs)
    (evil-add-hjkl-bindings magit-status-mode-map 'emacs)

    ;; fullscreen magit
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    (evil-leader/set-key
      "gs" 'magit-status
      "gl" 'magit-log
      "gb" 'magit-blame-mode)))

(use-package move-text
  :ensure t
  :defer t
  :init
  (progn
    (define-key evil-normal-state-map (kbd "] e") 'move-text-down)
    (define-key evil-normal-state-map (kbd "[ e") 'move-text-up)))

(use-package project-explorer
  :ensure t
  :defer t
  :init
  (evil-leader/set-key "a" 'project-explorer-toggle)
  :config
  (progn
    (setq pe/omit-gitignore t)
    (evil-declare-key 'normal project-explorer-mode-map
      (kbd "o") 'pe/return
      (kbd "r") 'pe/rename-file
      (kbd "d") 'pe/delete-file
      (kbd "RET") 'pe/return)))

(use-package paredit
  :ensure t
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

;; (use-package evil-paredit
;;   :defer t
;;   :ensure t
;;   :init
;;   (progn
;;     (add-hook 'emacs-lisp-mode-hook #'evil-paredit-mode)
;;     (add-hook 'cider-repl-mode-hook #'evil-paredit-mode)
;;     (add-hook 'clojure-mode-hook #'evil-paredit-mode)
;;     (add-hook 'scheme-mode-hook #'evil-paredit-mode)))

(use-package evil-cleverparens
  :ensure t
  :diminish evil-cleverparens-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode) 
    (add-hook 'cider-repl-mode-hook #'evil-cleverparens-mode) 
    (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package eldoc
  :ensure t
  :defer t
  :diminish eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode))

(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (evil-leader/set-key-for-mode 'clojure-mode
    "cs" 'cider-jack-in))

(use-package clojure-mode-extra-font-locking
  :ensure t
  :defer t)

(use-package evil-nerd-commenter
  :ensure t
  :defer t
  :init
  (progn
    (define-key evil-normal-state-map ",cc" 'evilnc-comment-or-uncomment-lines)
    (define-key evil-visual-state-map ",cc" 'evilnc-comment-or-uncomment-lines)))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode t))

(use-package evil-search-highlight-persist
  :ensure t
  :init
  (global-evil-search-highlight-persist t)
  :config
  (progn
    (define-key evil-normal-state-map (kbd "RET") 'evil-search-highlight-persist-remove-all)))

(use-package coffee-mode
  :ensure t
  :mode (("\\.coffee\\'" . coffee-mode)
         ("\\.cjsx" . coffee-mode))
  :config
  (progn
    (defun javascript/coffee-indent ()
      (if (coffee-line-wants-indent)
          (coffee-insert-spaces (+ (coffee-previous-indent) coffee-tab-width))
        (coffee-insert-spaces (coffee-previous-indent))))

    (add-hook 'coffee-mode-hook '(lambda ()
                                   (advice-remove 'evil-delete-backward-char-and-join #'remove-indent-level)
                                   (setq indent-line-function 'javascript/coffee-indent)))

    (setq whitespace-action '(auto-cleanup))
    (custom-set-variables '(coffee-tab-width 2))))

(use-package elscreen
  :ensure t
  :init (elscreen-start)
  :config
  (progn
    (global-set-key (kbd "s-w") 'elscreen-kill)
    (global-set-key (kbd "s-t") 'elscreen-create)
    (global-set-key (kbd "s-}") 'elscreen-next)
    (global-set-key (kbd "s-{") 'elscreen-previous)))

(use-package multi-term
  :ensure t
  :config
  (progn
    (setq system-uses-terminfo nil)))

(use-package ace-jump-mode
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode))

(use-package cider
  :ensure t
  :config
  (progn
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-mode-hook #'company-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode)
    (add-hook 'cider-repl-mode-hook #'company-mode)

    (defun cider-save-and-eval ()
      (interactive)
      (save-buffer)
      (cider-eval-buffer))

    ;; should be cider-mode, doesn't work
    (evil-leader/set-key-for-mode 'clojure-mode
      "E" 'cider-eval-last-sexp
      "T" 'cider-test-run-test
      "t" 'cider-test-run-tests
      "d" 'cider-doc
      "e" 'cider-save-and-eval)

    ;; Add support for evil
    (push 'cider-stacktrace-mode evil-motion-state-modes)
    (push 'cider-popup-buffer-mode evil-motion-state-modes)

    (setq cider-repl-pop-to-buffer-on-connect t
          cider-show-error-buffer t
          cider-auto-select-error-buffer t
          cider-repl-history-file "~/.emacs.d/cider-history"
          cider-repl-wrap-history t)))

(use-package sass-mode
  :ensure t
  :config
  (progn
    (defadvice haml-electric-backspace (before remove-indent-level activate)
      "Remove extra char (for 2 level-indent) when you're at that indent level"
      (if (and (looking-back "  ") (not (looking-at "\w")))
          (delete-char -1)))))

(setq backward-delete-char-untabify-method 'all)

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.erb\\'" . web-mode))
  :init
  (progn
    (setq web-mode-content-types-alist '(("jsx"  . "\\.js\\'")))
    (add-hook 'web-mode-hook (lambda ()
                               (set-fill-column 120)))))

(use-package rainbow-mode
  :ensure t)

(use-package slim-mode
  :ensure t
  :mode ("\\.slim\\'" . slim-mode))

(use-package guide-key
  :ensure t
  :diminish guide-key-mode
  :config
  (progn
    (guide-key-mode t)        
    (setq guide-key/guide-key-sequence '("," ",c" ",h" ",b")
          guide-key/recursive-key-sequence-flag t)))

(use-package ruby-hash-syntax
  :ensure t)

(use-package ruby-refactor
  :ensure t)

(use-package projectile-rails
  :ensure t
  :config
  (progn
    (add-hook 'projectile-mode-hook 'projectile-rails-on)))

(use-package robe
  :ensure t
  :config
  (progn
    (add-hook 'ruby-mode-hook 'robe-mode)
    (eval-after-load 'company
      '(push 'company-robe company-backends))))

(use-package rspec-mode
  :ensure t
  :config
  (progn
    (defadvice rspec-compile (around rspec-compile-around)
      "Use BASH shell for running the specs because of ZSH issues."
      (let ((shell-file-name "/bin/bash"))
        ad-do-it))

    (ad-activate 'rspec-compile)

    (setq rspec-use-rake-when-possible nil
          rspec-spec-command "rspec"
          rspec-use-bundler-when-possible nil
          rspec-use-spring-when-possible t)

    (setq compilation-scroll-output t)

    (evil-leader/set-key-for-mode 'ruby-mode
      "cs" 'robe-start
      "d" 'robe-doc
      "ch" 'ruby-toggle-hash-syntax)

    (evil-leader/set-key-for-mode 'ruby-mode
      "t" 'rspec-verify
      "T" 'rspec-verify-single)))

(use-package ruby-mode
  :ensure t
  :mode (("\\.rb\\'"       . ruby-mode)
         ("\\.ru\\'"       . ruby-mode)
         ("\\.gemspec\\'"  . ruby-mode)
         ("\\.rake\\'"     . ruby-mode)
         ("Rakefile\\'"    . ruby-mode)
         ("Gemfile\\'"     . ruby-mode))
  :config
  (progn
    (setq ruby-deep-indent-paren nil
          ruby-deep-arglist nil)
    (modify-syntax-entry ?_ "w" ruby-mode-syntax-table)))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'"  . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package evil-iedit-state
  :ensure t
  :config
  (progn
    (evil-leader/set-key
      "i" 'evil-iedit-state/iedit-mode)))

(use-package mmm-mode
  :ensure t
  :config
  (progn
    (mmm-add-classes
     '((cjsx
        :submode web-mode
        :face mmm-code-submode-face
        :front "\\(\\)[:space:]*<\\Sc"
        :front-match 1
        :back "\\Sc>[:space:]*\\(\\)"
        :back-match 1)))

    (setq mmm-global-mode 'maybe
          mmm-parse-when-idle t)

    (mmm-add-mode-ext-class 'coffee-mode "\\.cjsx\\'" 'cjsx)))

(defun w ()
  "Mimicks evil ex-mode :w command"
  (interactive)
  (save-buffer))

(use-package restclient
  :ensure t)

(use-package helm
  :ensure t
  :defer t
  :init
  (progn
    (use-package helm-projectile
      :ensure t)

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

    (global-set-key ";" 'helm-M-x)
    (define-key evil-normal-state-map ";" 'helm-M-x)
    (global-set-key (kbd "M-x") 'helm-M-x)))

(use-package helm-spotify
  :ensure t
  :defer t)

(use-package expand-region
  :ensure t
  :init
  (evil-leader/set-key "r" 'er/expand-region))

(use-package flycheck
  :ensure t
  :init
  (progn 
    (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change)
          flycheck-idle-change-delay 1.5)
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
    (add-hook 'prog-mode-hook 'flycheck-mode)
    (add-hook 'mmm-mode-hook 'flycheck-mode)))

(use-package flycheck-clojure
  :ensure t
  :init
  :config
  (flycheck-clojure-setup))

(use-package flycheck-pos-tip
  :ensure t
  :config
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(use-package yasnippet
  :ensure t
  :init
  (progn 
    (add-hook 'prog-mode-hook 'yas-minor-mode))
  :config
  (yas-reload-all))

(use-package compile
  :commands compile
  :config
  (progn
    (setq compilation-ask-about-save nil
          compilation-always-kill t
          compilation-scroll-output 'first-error)))

(provide 'init)
