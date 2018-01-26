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

(use-package evil-magit
  :config
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  (keys :keymaps 'magit-status-mode-map
        "K" 'magit-discard))

(provide 'init-magit)
