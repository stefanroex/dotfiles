(use-package magit
  :defer t
  :init
  (keys-l "gs" 'magit-status
          "gl" 'magit-log-head
          "gb" 'magit-blame)
  :config
  (setq magit-display-buffer-function 'magit-buffer-full-screen)
  (evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-log-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-process-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-status-mode-map 'emacs)
  (add-hook 'git-commit-mode-hook 'evil-insert-state)

  (keys :keymaps 'magit-status-mode-map
        "TAB" 'magit-section-toggle
        "K" 'magit-discard)

  (keys :keymaps 'magit-blame-mode-map
        "q" 'magit-blame-quit)

  (global-auto-revert-mode t))

(provide 'init-magit)
