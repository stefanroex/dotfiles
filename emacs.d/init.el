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

(use-package base16-theme
  :ensure t
  :init (load-theme 'base16-tomorrow-dark t))

;; Font
(set-frame-font "Inconsolata 18")
(setq-default line-spacing 4)

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
    ;; (setq shell-file-name "bash")
    (exec-path-from-shell-initialize)
    (setq shell-command-switch "-ic")))

;; UI
(use-package init-custom
  :load-path "init/")

;; Fancy battery info for mode line
(use-package fancy-battery
  :ensure t
  :defer t
  :config (setq fancy-battery-show-percentage t)
  :init (fancy-battery-mode))

(use-package ido
  :ensure t
  :init (ido-mode t)
  :config
  (setq ido-use-faces nil
        ido-enable-flex-matching t
        ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace" "^\*compilation" "^\*GTAGS" "^session\.*")))

(use-package ido-vertical-mode
  :ensure t
  :init (ido-vertical-mode t))

(use-package ido-ubiquitous
  :ensure t
  :config
  (setq ido-everywhere 1))

(use-package flx-ido
  :ensure t
  :config
  (setq flx-ido-threshhold 1000
        flx-ido-mode 1))

(use-package evil
  :ensure t
  :init (evil-mode t)
  :config
  (progn
    ;; Map ; to : for easier ex access
    (define-key evil-normal-state-map (kbd ";") 'evil-ex)
    (define-key evil-visual-state-map (kbd ";") 'evil-ex)
    (define-key evil-motion-state-map (kbd ";") 'evil-ex)

    ;; esc quits
    (setq evil-intercept-esc 'always)
    (defun minibuffer-keyboard-quit ()
      (interactive)
      (if (and delete-selection-mode transient-mark-mode mark-active)
          (setq deactivate-mark  t)
        (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
        (abort-recursive-edit)))
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
    (global-set-key [escape] 'evil-exit-emacs-state)

    ;; Next and previous search results
    (define-key evil-normal-state-map (kbd "C-n") 'next-error)
    (define-key evil-normal-state-map (kbd "C-p") 'previous-error)

    ;; Set offset used by < and > to 2
    (setq evil-shift-width 2)

    ;; Easy window management
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

    (evil-ex-define-cmd "A" 'projectile-toggle-between-implementation-and-test)
    (evil-ex-define-cmd "Ag" 'ag)))

(use-package evil-surround
  :ensure t
  :defer t
  :init (global-evil-surround-mode t))

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

    (evil-leader/set-leader ",")

    (evil-leader/set-key-for-mode 'emacs-lisp-mode "e" 'eval-last-sexp)

    (evil-leader/set-key
      "," 'my-switch-to-other-buffer
      "F" 'find-file
      "b" 'switch-to-buffer
      "k" 'kill-buffer
      "m" 'smex
      "n" 'multi-term
      "q" 'kill-buffer-and-window
      "w" 'quit-window
      "x" 'next-code-buffer
      "z" 'previous-code-buffer
      "v" 'open-emacs-config)))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-enable-caching t)
    (projectile-global-mode)
    (defun find-file-projectile-or-dir ()
      (interactive)
      (if (projectile-project-p)
          (projectile-find-file)
        (ido-find-file)))

    (evil-leader/set-key
      "f" 'find-file-projectile-or-dir
      "p" 'projectile-switch-project)

    (defvar projectile-rails-rspec '("Gemfile" "app" "config" "spec"))))

(use-package ag
  :ensure t
  :defer t
  :config
  (evil-ex-define-cmd "Ag" 'ag))

(use-package better-defaults
  :ensure t)

(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :config
  (progn
    (require 'auto-complete-config)
    (setq ac-use-fuzzy t
          ac-auto-start t
          ac-use-quick-help nil
          ac-delay 0.1
          ac-ignore-case t)
    (set-default 'ac-sources
                 '(ac-source-imenu
                   ac-source-dictionary
                   ac-source-words-in-buffer
                   ac-source-words-in-same-mode-buffers
                   ac-source-words-in-all-buffer))
    ;; (setq ac-sources '(ac-source-words-in-buffer
    ;;                    ac-source-semantic
    ;;                    ac-source-yasnippet
    ;;                    ac-source-abbrev))
    (global-auto-complete-mode 1)))

(use-package magit
  :ensure t
  :defer t
  :diminish magit-auto-revert-mode
  :init
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package git-blame
  :ensure t)

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

(use-package evil-paredit
  :defer t
  :ensure t
  :config
  (after 'evil
    (add-hook 'macs-lisp-mode-hook #'evil-paredit-mode)
    (add-hook 'cider-repl-mode-hook #'evil-paredit-mode)
    (add-hook 'clojure-mode-hook #'evil-paredit-mode)
    (add-hook 'scheme-mode-hook #'evil-paredit-mode)))

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
    "c" 'cider-jack-in))

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

;; (use-package yasnippet
;;   :ensure t
;;   :defer t
;;   :diminish yas-minor-mode)

(use-package flycheck
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook #'global-flycheck-mode)
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))))

(use-package coffee-mode
  :ensure t)

;; (use-package rspec-mode
;;   :ensure t
;;   :config
;;   (progn
;;     (add-hook 'ruby-mode-hook 'rspec-mode)

;;     (setq rspec-use-bundler-when-possible nil
;;           rspec-use-rake-when-possible nil
;;           rspec-spec-command "./bin/rspec")

;;     (rspec-install-snippets)

;;     (evil-leader/set-key
;;       "t" 'rspec-verify
;;       "T" 'rspec-verify-single)))

(use-package ruby-test-mode
  :defer t
  :init
  (add-hook 'ruby-mode-hook 'ruby-test-mode)
  :config
  (progn
    (evil-leader/set-key 'ruby-mode
      "t" 'ruby-test-run
      "T" 'ruby-test-run-at-point)))

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


(use-package smex
  :ensure t)

(use-package ace-jump-mode
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode))

(use-package cider
  :ensure t
  :config
  (progn
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode)

    (defun cider-save-and-eval ()
      (interactive)
      (save-buffer)
      (cider-eval-buffer))

    ;; should be cider-mode, doesn't work
    (evil-leader/set-key-for-mode 'clojure-mode
      "E" 'cider-eval-last-sexp
      "T" 'cider-test-run-test
      "d" 'cider-doc
      "e" 'cider-save-and-eval
      "t" 'cider-test-run-tests)

    (setq cider-repl-pop-to-buffer-on-connect t
          cider-show-error-buffer t
          cider-auto-select-error-buffer t
          cider-repl-history-file "~/.emacs.d/cider-history"
          cider-repl-wrap-history t)))

(defun which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))

(use-package ac-cider
  :ensure t
  :config
  (progn
    (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
    (add-hook 'cider-mode-hook 'ac-cider-setup)
    (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
    (eval-after-load "auto-complete"
      '(progn
         (add-to-list 'ac-modes 'cider-mode)
         (add-to-list 'ac-modes 'cider-repl-mode)))))

(provide 'init)
