(use-package smart-jump
  :general
  (keys
    "M-." 'smart-jump-go
    "M-," 'smart-jump-back
    "M-?" 'smart-jump-references)
  :config
  (smart-jump-setup-default-registers))

(use-package dumb-jump
  :init
  (setq dumb-jump-aggressive nil
	dumb-jump-selector 'ivy)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(provide 'stx-module-jump)
