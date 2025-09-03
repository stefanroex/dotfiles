(use-package iflipb
  :init
  (setq iflipb-wrap-around t)
  :general
  (keys-l
    "z" 'iflipb-previous-buffer
    "x" 'iflipb-next-buffer))

(use-package terraform-mode)

(provide 'stx-module-packages)
