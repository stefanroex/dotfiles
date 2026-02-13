;;;  -*- lexical-binding: t -*-

(use-package projectile
  :demand
  :diminish projectile-mode

  :custom
  (projectile-create-missing-test-files t)

  :general
  (:states '(normal emacs motion)
   "SPC p" 'projectile-command-map
   "SPC f" 'projectile-find-file)

  :config
  (projectile-global-mode t))

(provide 'stx-module-project)
