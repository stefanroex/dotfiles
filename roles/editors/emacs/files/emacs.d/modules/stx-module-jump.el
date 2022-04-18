(use-package dumb-jump
  :commands 'dumb-jump-go
  :init
  (setq dumb-jump-selector 'ivy)
  (keys
    :keymaps 'prog-mode-map
    :modes 'normal
    "gD" 'dumb-jump-go))

(use-package smart-jump
  :commands (smart-jump smart-jump-go)
  :init
  (keys
    "M-." 'smart-jump-go
    "M-," 'smart-jump-back)
  :config
  (smart-jump-setup-default-registers))

(provide 'stx-module-jump)
