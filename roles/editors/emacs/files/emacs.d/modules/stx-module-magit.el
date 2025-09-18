;;;  -*- lexical-binding: t -*-

(use-package magit
  :general
  (:states '(normal emacs motion)
   "SPC gs" 'magit-status
   "SPC gl" 'magit-log-head
   "SPC gb" 'magit-blame)

  :general-config
  (:keymaps 'magit-blame-read-only-mode-map
   :states '(normal emacs motion)
   "RET" 'magit-show-commit)
  (:keymaps 'magit-mode-map
   "SPC" nil)
  (:keymaps 'transient-map
   "q" 'transient-quit-one)
  (:keymaps 'transient-edit-map
   "q" 'transient-quit-one)
  (:keymaps 'transient-sticky-map
   "q" 'transient-quit-one)
  (:keymaps 'magit-status-mode-map
   "K" 'magit-discard)

  :custom
  (magit-diff-refine-hunk t)
  (magit-diff-visit-prefer-worktree t)
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function 'magit-restore-window-configuration)

  :config
  (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header)
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

(use-package forge
  :after
  (magit evil-collection)

  :general
  (:states '(normal emacs motion)
   "SPC go" 'forge-browse)

  :custom
  (forge-add-default-bindings nil)
  (forge-add-default-sections nil)

  :config
  (evil-collection-forge-setup))

(provide 'stx-module-magit)
