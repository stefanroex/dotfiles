(provide 'init-appearance)

;; Font
(set-default-font "Inconsolata 18")
(setq-default line-spacing 4)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'tomorrow-night-bright t)
