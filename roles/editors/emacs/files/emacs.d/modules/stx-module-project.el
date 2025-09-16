;;;  -*- lexical-binding: t -*-

(use-package projectile
  :diminish projectile-mode
  :config
  (setq projectile-create-missing-test-files t)
  (keys-l "p" 'projectile-command-map)
  (projectile-global-mode t))

(provide 'stx-module-project)
