(use-package magit
  :defer t
  :init
  (setq magit-display-buffer-function 'magit-buffer-full-screen)
  (evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-log-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-process-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-status-mode-map 'emacs)
  (keys :states nil
        :keymaps 'magit-file-section-map
        "K" 'magit-discard)

  (keys-l "gs" 'magit-status
          "gl" 'magit-log
          "gb" 'magit-blame))

(provide 'init-magit)
