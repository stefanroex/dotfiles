;;;  -*- lexical-binding: t -*-

(use-package smart-jump
  :general
  (:states '(normal emacs motion)
   "M-." 'smart-jump-go
   "M-," 'smart-jump-back
   "M-?" 'smart-jump-references)

  :init
  (smart-jump-setup-default-registers))

(use-package dumb-jump
  :custom
  (dumb-jump-aggressive nil)

  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(provide 'stx-module-jump)
