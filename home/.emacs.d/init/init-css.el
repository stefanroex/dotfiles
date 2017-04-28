(use-package css-mode
  :defer t
  :config
  (setq css-indent-offset 2))

(use-package sass-mode
  :ensure t
  :defer t)

(use-package scss-mode
  :ensure t
  :defer t)

(provide 'init-css)
