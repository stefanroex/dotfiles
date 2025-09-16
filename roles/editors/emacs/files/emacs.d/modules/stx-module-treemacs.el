;;;  -*- lexical-binding: t -*-

(defun stx/treemacs-toggle-only-current-project ()
  "Toggle treemacs in the current project exclusively"
  (interactive)
  (require 'treemacs)
  (let ((visible (treemacs-current-visibility)))
    (if (eq visible 'visible)
        (treemacs)
      (treemacs-display-current-project-exclusively))))

(use-package treemacs
  :general
  (keys-l
    "a" 'stx/treemacs-toggle-only-current-project)
  :config
  (keys
    :keymaps 'treemacs-mode-map
    "o" 'treemacs-RET-action
    "R" 'treemacs-refresh
    "r" 'treemacs-rename
    "d" 'treemacs-delete-file
    "i" 'treemacs-toggle-show-dotfiles)
  (setq treemacs-collapse-dirs 3
        treemacs-eldoc-display nil
        treemacs-project-follow-cleanup t
        treemacs-no-png-images t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit))

(provide 'stx-module-treemacs)
