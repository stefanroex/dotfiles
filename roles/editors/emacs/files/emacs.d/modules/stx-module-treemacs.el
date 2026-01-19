;;;  -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

(defun stx/treemacs-toggle-only-current-project ()
  "Toggle Treemacs for the current project exclusively."
  (interactive)
  (require 'treemacs)
  (if (eq (treemacs-current-visibility) 'visible)
      (treemacs)
    (treemacs-display-current-project-exclusively)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages

(use-package treemacs
  :general
  (:states '(normal emacs motion)
   "SPC a" 'stx/treemacs-toggle-only-current-project)

  :general-config
  (:keymaps 'treemacs-mode-map
   :states '(normal emacs motion)
   "o" 'treemacs-RET-action
   "R" 'treemacs-refresh
   "r" 'treemacs-rename-file
   "d" 'treemacs-delete-file
   "i" 'treemacs-toggle-show-dotfiles)

  :custom
  (treemacs-collapse-dirs 3)
  (treemacs-eldoc-display nil)
  (treemacs-follow-after-init t)
  (treemacs-project-follow-cleanup t)
  (treemacs-no-png-images t)

  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-git-mode 'deferred))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

(provide 'stx-module-treemacs)
