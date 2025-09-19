;;;  -*- lexical-binding: t -*-

(use-package enh-ruby-mode)

(use-package slim-mode)

(use-package projectile-rails
  :hook (projectile-mode . projectile-rails-on))

(provide 'stx-module-lang-ruby)
