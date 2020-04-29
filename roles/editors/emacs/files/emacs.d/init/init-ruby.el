(use-package ruby-mode
  :config
  (setq ruby-insert-encoding-magic-comment nil))

(use-package rubocopfmt
  :init
  (add-hook 'ruby-mode-hook #'rubocopfmt-mode)
  :config
  (setq rubocopfmt-use-bundler-when-possible nil
        rubocopfmt-show-errors nil
        rubocopfmt-rubocop-command "rubocop-daemon-wrapper"))

(use-package rspec-mode
  :defer t
  :config
  (setq rspec-use-bundler-when-possible nil)
  (keys-l :keymaps 'ruby-mode-map
    "t" 'rspec-verify
    "T" 'rspec-verify-single
    "l" 'rspec-rerun))

(use-package inf-ruby
  :defer t
  :init
  (add-hook 'ruby-mode-hook 'inf-ruby-switch-setup))

(use-package projectile-rails
  :defer t
  :config
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(provide 'init-ruby)
