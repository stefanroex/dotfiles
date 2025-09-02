(use-package vertico
  :demand t
  :init
  (vertico-mode +1)
  (setq vertico-cycle t)
  (keys-l
    "f" 'projectile-find-file
    "F" 'find-file))

(use-package consult
  :general
  (keys-l
    "B" 'consult-buffer
    "b" 'consult-project-buffer
    "y" 'consult-yank-pop))

(use-package hotfuzz
  :demand t
  :config
  (setq completion-styles '(hotfuzz)))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package savehist
  :init
  (savehist-mode))

(provide 'stx-module-completion)
