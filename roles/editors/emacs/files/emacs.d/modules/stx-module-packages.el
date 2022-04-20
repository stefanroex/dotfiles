(use-package iflipb
  :init
  (setq iflipb-wrap-around t)
  :general
  (keys-l
    "z" 'iflipb-previous-buffer
    "x" 'iflipb-next-buffer))

(provide 'stx-module-packages)

(use-package string-edit)
